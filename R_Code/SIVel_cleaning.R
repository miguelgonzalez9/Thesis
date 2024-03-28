#################################
# This script cleans the political violnece
# data from SIVel, gathered by Noche y Niebla 
# and CINEP. 
# Date: 24/03/2024 
# Author: Miguel Gonzalez Lugo
##########################################
path <- "D:/Documents/GitHub/Thesis/Data/Conflicto Armado/"
setwd(path)
pol_vio <- read_xlsx("CINEP_pol_vio.xlsx", col_names = T)
mun_char <- read_dta("D:/Documents/GitHub/Thesis/Data/CEDE/PANEL_CARACTERISTICAS_GENERALES(2022).dta")

## Set colnames, clean data without location, set col types --------------
colnames(pol_vio) <- c("date", "location", "respon", "type", "victims", "report", "actions")
pol_vio <- pol_vio %>% select(-actions) %>% filter(location != "")

pol_vio$date <- as.Date(pol_vio$date)

## Expand multi value cells --------------
pol_vio_try <- split_multicell(pol_vio, c("type", "victims"), ",")
pol_vio <- pol_vio_try

## Get municipality and department -----------------
loc <- str_split_fixed(pol_vio$location, pattern = "/", n = 2)
pol_vio$dpto <- loc[,1]
pol_vio$mun <- loc[,2]
pol_vio <- pol_vio %>% mutate(mun = case_when(
  str_detect(dpto, "Bogotá") ~ "Bogotá, D.C.", 
  T ~ mun)) %>% filter(mun != "")

## Clean strings ---------------------

pol_vio_c <- pol_vio %>% 
  mutate(across(location:mun, ~ stri_trans_general(.x, id = "Latin-ASCII"))) %>% 
  mutate(across(location:mun, ~ tolower(.x))) %>% 
  mutate(across(location:mun, ~ trimws(.x)))
pol_vio <- pol_vio_c

## Create violence type variables -------------------
pol_vio$type %>% unique()
othe_vio_collective <- c("colectivo lesionado|hambre como metodo de guerra|confinamiento colectivo|ataque indiscriminado|colectivo escudo")
other_ind_viol <- c("lesion a persona protegida|lesion fisica|lesion por objetivos|detencion arbitraria|bienes civiles|rapto|tortura")
pol_vio <- pol_vio %>% mutate(year = year(date), 
                                    month = month(date), 
                                    day = day(date),
                                    lid_asis = as.integer(str_detect(type, "asesinato|ejecucion extrajudicial|homicidio")), 
                                    threat = as.integer(str_detect(type, "amenazado|amenaza")),
                                    collective_threat = as.integer(str_detect(type, "colectivo amenazado")),
                                    individual_threat = as.integer(str_detect(type, "amenaza")),
                                    collective_violence = as.integer(str_detect(type, "colectivo")), 
                                    displacement = as.integer(str_detect(type, "desplazamiento")), 
                                    failed_asis = as.integer(str_detect(type, "atentado")), 
                                    other_collective = as.integer(str_detect(type,othe_vio_collective)),
                                    other_ind = as.integer(str_detect(type,other_ind_viol)), 
                                    other_vio = as.integer(str_detect(type,paste0(other_ind_viol,othe_vio_collective, collapse = "|")))
                                    )

## Association case variables. Imperfect measurement of responsible.
pol_vio <- pol_vio %>% mutate(report = report %>% str_replace_all("[\r\n]", "")) %>% 
  mutate(report =  report %>%  str_replace_all("\u0093", ""))

unique(pol_vio$respon)
sample(pol_vio$report, 3)
# Using Osorio classification
insuregency <- c("eln|epl|disidencia|farc|grupos postacuerdo de paz|guerrilla|farc-ep|ejercito de liberacion nacional|guerrilla|comandos ernesto rojas")
paramilitary <- c("paramilitares|aguc|auc|milicias|autodefensas|ejercito revolucionario popular antisubversivo|antisubversivo|anti-comunista|libertadores de vichada|grupos de intolerancia")
criminal_bands <- c("bandas criminales|alianzas criminales|bacrim|urabenos|rastrojos|aguilas negras|los paisas|oficina de envigado|los 400")
# Differentiate based on Avila (2019)
aguilas_negras <- c("aguilas negras")
gov <- c("fuerza publica|ejercito|fuerza aerea|policia|ejercito|estado colombiano|ejercito|agentes estado")

pol_vio <- pol_vio %>% mutate(
  insurgent_respon = as.integer(str_detect(respon, insuregency) | str_detect(report, insuregency)),
  param_respon = as.integer(str_detect(respon, paramilitary) | str_detect(report, paramilitary)), 
  bacrim_respon = as.integer(str_detect(respon, criminal_bands) | str_detect(report, criminal_bands)), 
  gov_respon =as.integer(str_detect(respon, gov) | str_detect(report, gov)), 
  an_respon = as.integer(str_detect(respon, aguilas_negras) | str_detect(report, aguilas_negras))
)

# Delete farc respon after 2016. Differentiate with demobilized farc victims. 
pol_vio <- pol_vio %>% mutate(insurgent_respon = case_when(
  str_detect(report, "farc-ep") & !str_detect(report, "disidencia") & year >= 2016 ~ 0,
  T ~ insurgent_respon
))

## Municipality clean and code


pol_vio <- pol_vio %>% mutate(mun = stri_trans_general(mun, id = "Latin-ASCII") %>% 
                                      str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                      str_replace_all(" ", ""), 
                                    dpto = stri_trans_general(dpto, id = "Latin-ASCII") %>% 
                                      str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                      str_replace_all(" ", ""))

pol_vio <- pol_vio %>% mutate(cod = stri_trans_general(paste0(mun, dpto), id = "Latin-ASCII") %>% 
                                      str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                      str_replace_all(" ", "")) %>% 
  filter(mun != "")

mnames <- mun_char %>% select(municipio, depto, codmpio) %>% distinct() %>% 
  mutate(cod = stri_trans_general(paste0(municipio, depto), id = "Latin-ASCII") %>% 
           str_remove(pattern = "//n|//r|-|") %>% tolower()%>% 
           str_replace_all(" ", ""))

dist_name_matrix <- adist(unique(pol_vio$cod), unique(mnames$cod), partial = TRUE, ignore.case = TRUE)
colnames(dist_name_matrix) <- unique(mnames$cod)
rownames(dist_name_matrix) <- unique(lideres_sd$cod)

dist_df <- dist_name_matrix %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(., "cod_inv") %>% 
  pivot_longer(cols = 2:last_col(), names_to = "cod_lid", values_to = "dist") %>% 
  filter(dist < 3)

### Probabilistic merge ----------------

dist_unique <- data.frame()
dist_mult <- data.frame()
for(i in 1:length(unique(pol_vio$cod))) {
  x <- min_dist_str(unique(pol_vio$cod)[i], unique(mnames$cod))
  print(x)
  if(length(x) == 1){
    dist_unique <- rbind(dist_unique, c(x, unique(pol_vio$cod)[i]))
  }
  if (length(x) > 1){
    dist_mult <- rbind(dist_mult, c(paste(x, collapse = "-----"), unique(pol_vio$cod)[i]))
  }
}
View(dist_mult)
mult_to_un <- matrix(c("mercaderescauca", "mercaderes,cauca/mercaderescauca", "puracecauca", "purace,huila/pitalitocauca"), ncol = 2, byrow = T) %>% as.data.frame()
colnames(dist_unique) <- c("cod_vic_inv", "cod_lid")
colnames(mult_to_un) <- c("cod_vic_inv", "cod_lid")
dist <- rbind(dist_unique,mult_to_un)
dist <- dist %>% left_join(mnames %>% select(-c(depto, municipio)) %>% unique(), by = (c("cod_vic_inv" = "cod")))
pol_vio <- pol_vio %>% left_join(dist, by = c("cod" = "cod_lid"))

if (dim(pol_vio[is.na(pol_vio$codmpio),])[1] != 0){
  warning("NAs in merge")
}

### Five digit code 

pol_vio$codmpio <- as.character(pol_vio$codmpio)
pol_vio <- pol_vio %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
  ))

## Type of victims ---------------------------------

pol_vio <- pol_vio %>% mutate(report = report %>% str_replace_all("[\r\n]", "") %>% tolower())

pol_vio <- pol_vio %>% select(-c(cod_vic_inv, cod))

# String detection patters
partidos_izq <- c("UNION PATRIOTICA|ALIANZA NACIONAL POPULAR|ANAPO|MOVIMIENTO AUTORIDADES INDIGENAS DE COLOMBIA|AICO|PARTIDO SOCIALDEMOCRATA COLOMBIANO|
MOVIMIENTO INDEPENDIENTE FRENTE DE ESPERANZA FE|MOVIMIENTO OBRERO INDEPENDIENTE REVOLUCIONARIO|MOIR|
MOVIMIENTO ALTERNATIVA DEMOCRATICA|MOVIMIENTO 19 DE ABRIL|PARTIDO UNIDAD DEMOCRATICA|MOVIMIENTO FRENTE SOCIAL Y POLITICO|
MOVIMIENTO AUTORIDADES INDIGENAS DE COLOMBIA|AICO|PARTIDO POLO DEMOCRATICO INDEPENDIENTE|POLO DEMOCRATICO ALTERNATIVO|MOVIMIENTO VAMOS IPIALES|
colombia humana|alianza social independiente|partido comunista colombiano|pacto historico") %>% tolower() %>% str_replace("\n", "")
comunal_sector <- c("lider comunitario|comunitaria|junta de accion comunal|JAC|consejo comunitario|lideresa comunitaria|defensora de derechos humanos|defensor de derechos humanos|lider comunal|lider civico|lider social")
indig_sector <- c("indigena|igena|minga|minguero|cabildo|resguardo|lidere(s) indigena(s)")
campesino_sector <- c("campesino|reclamante|lider(es) campesino(s)|anuc")
politics_sector <- c("candidata|candidato|electo|electa")


pol_vio <- pol_vio %>% mutate(
  comunal_sector = as.integer(str_detect(report, comunal_sector)), 
  lgbt_sector = as.integer(str_detect(report, "lgtb|lgbt")), 
  campesino_sector = as.integer(str_detect(report, campesino_sector)),
  indig_sector = as.integer(str_detect(report, indig_sector)), 
  afro_sector = as.integer(str_detect(report, "afrodescen|negritud|palenque")), 
  sindical_sector = as.integer(str_detect(report, "sindic|obrer|sindicato")), 
  ambiental_sector = as.integer(str_detect(report, "ambiental|sostenible|minrero|mineria|consevacion|medio ambient|ecolog")), 
  displaced_sector = as.integer(str_detect(report, "desplazad")), 
  pnis_sector = as.integer(str_detect(report, "pnis|sustitucion de cultivos")), 
  izq_sector = as.integer(str_detect(report, partidos_izq)), 
  mujer_secor = as.integer(str_detect(report, "feminismo|feminista")),
  ex_FARC = as.integer((str_detect(report, "excombatiente") & str_detect(report, "farc-ep")) | 
                         str_detect(report, "firmante|ex-farc|demovilizado")), 
  politics_sector =  as.integer(str_detect(report, politics_sector))
)


## Create RD dataset. 

pol_vio <- pol_vio %>% mutate(vio = 1)
sum_cols <- colnames(pol_vio)[-c(1:11, 27)]
pol_vio$year <- as.character(pol_vio$year)
pol_vio_RD <- summarize_data_count(pol_vio, sum_cols, sum_cols)


## Create dataset for DiD setting. 


rm(pol_vio_try, pol_vio_c,mult_to_un, dist, dist_mult, dist_unique, sum_cols)
View(pol_vio_day)
