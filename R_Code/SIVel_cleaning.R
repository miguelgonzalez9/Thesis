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
pol_vio <- pol_vio %>% select(-actions) %>% filter(location != "") %>% distinct()

pol_vio$date <- as.Date(pol_vio$date)

## Expand multi value cells --------------
pol_vio$non_match_comma <- NA

# Add identifyer
pol_vio$id <- 1:dim(pol_vio)[1]

# Delete commas from category
pol_vio$type <- str_replace_all(pol_vio$type, "MUERTO POR OBJETIVOS, MÉTODOS Y MEDIOS ILÍCITOS", 
                                replacement = "MUERTO POR OBJETIVOS MÉTODOS Y MEDIOS ILÍCITOS")


# Keep order of type, victims. Do not add more variables to split. 
pol_vio_try <- split_multicell(pol_vio, c("type", "victims"), ", ")

dim(pol_vio_try) # 10649
# Most duplicated rows have general names of victims: "N N", Integrantes comunidad...
#View(pol_vio_try[duplicated(pol_vio_try),] %>% filter(!(victims %in% c("N N", "N.N. N", "NNN N"))))

# Save un splited cases in excel and manually change them 

#pol_vio_mult <- pol_vio_try %>% filter(non_match_comma ==  1) %>% split_multicell(c("victims"), ", ")
#write_xlsx(pol_vio_mult, path = "D:/Documents/GitHub/Thesis/Data/manual_data/SiVel.xlsx")
pol_vio_mult <- read_excel("D:/Documents/GitHub/Thesis/Data/manual_data/SiVel_man.xlsx") %>% 
  split_multicell(c("type"), ", ")

# Replace and further split type variable
pol_vio_try <- pol_vio_try %>% filter(non_match_comma ==  0 | is.na(non_match_comma)) %>% 
  rbind(pol_vio_mult)

dim(pol_vio_try)# must be > 10649

pol_vio_try <- pol_vio_try %>% mutate(type = 
                                        case_when(type == "B:2:46 ATENTADO," ~ "B:2:46 ATENTADO", 
                                                  T ~ type))  

pol_vio_try <- pol_vio_try %>% split_multicell(c("type"), ", ")

pol_vio_try <- pol_vio_try %>% mutate(mult_vict = str_detect(victims, ","), 
                                      mult_type = str_detect(type, ","))
dim(pol_vio_try) #11666
#pol_vio_try$victims[pol_vio_try$mult_vict]
#pol_vio_try$type[pol_vio_try$mult_type]

pol_vio_try <- pol_vio_try %>% select(-c(mult_vict,mult_type))

pol_vio <- pol_vio_try

## Get municipality and department -----------------
loc <- str_split_fixed(pol_vio$location, pattern = "/", n = 2)
pol_vio$dpto <- loc[,1]
pol_vio$mun <- loc[,2]
pol_vio <- pol_vio %>% mutate(mun = case_when(
  str_detect(dpto, "BogotÃ¡") ~ "BogotÃ¡, D.C.", 
  T ~ mun)) %>% filter(mun != "")

## Clean strings ---------------------

pol_vio_c <- pol_vio %>% 
  mutate(across(location:mun, ~ stri_trans_general(.x, id = "Latin-ASCII"))) %>% 
  mutate(across(location:mun, ~ tolower(.x))) %>% 
  mutate(across(location:mun, ~ trimws(.x)))
pol_vio <- pol_vio_c

## Create violence type variables -------------------
#pol_vio$type %>% table() %>% sort(decreasing = T) %>% View()
pol_vio <- pol_vio %>% mutate(type = case_when(
  str_detect(type, "homicidio intencional de persona protegida") ~ "homicidio intencional de persona protegida",
  str_detect(type, "ejecucion extrajudicial") ~ "ejecucion extrajudicial",
  str_detect(type, "desplazamiento forzado") ~ "desplazamiento forzado", 
  str_detect(type, "colectivo amenazado") ~ "colectivo amenazado",
  str_detect(type, "confinamiento colectivo") ~ "confinamiento colectivo",
  str_detect(type, "lesion fisica") ~ "lesion fisica", 
  str_detect(type, "colectivo escudo") ~ "colectivo escudo",
  str_detect(type, "zonas humanitarias") ~ "zonas humanitarias",
  str_detect(type, "bienes civiles") ~ "bienes civiles",
  str_detect(type, "hambre como metodo de guerra") ~ "hambre como metodo de guerra",
  str_detect(type, "ataque indiscriminado") ~ "ataque indiscriminado",
  str_detect(type, "empleo ilicito de armas de uso restringido") ~ "empleo ilicito de armas de uso restringido",
  str_detect(type, "pillaje") ~ "pillaje",
  str_detect(type, "lesion a persona protegida") ~ "lesion a persona protegida",
  str_detect(type, "muerto por objetivos metodos y medios ilicitos") ~ "muerto por objetivos metodos y medios ilicitos",
  str_detect(type, "metodos y medios ilicitos") ~ "muerto por objetivos metodos y medios ilicitos",
  str_detect(type, "lesion por objetivos") ~ "lesion por objetivos",
  str_detect(type, "detencion arbitraria") ~ "detencion arbitraria",
  str_detect(type, "colectivo desplazado") ~ "colectivo desplazado",
  str_detect(type, "colectivo lesionado") ~ "colectivo lesionado",
  str_detect(type, "amenaza") ~ "amenaza",
  str_detect(type, "asesinato") ~ "asesinato", 
  str_detect(type, "secuestro") ~ "secuestro", 
  str_detect(type, "violencia sexual") ~ "violencia sexual", 
  str_detect(type, "violacion") ~ "violencia sexual", 
  str_detect(type, "abuso sexual") ~ "violencia sexual", 
  str_detect(type, "atentado") ~ "atentado", 
  str_detect(type, "tortura") ~ "tortura", 
  str_detect(type, "rapto") ~ "rapto",
  
  T ~ type
))

#pol_vio$type %>% table() %>% sort(decreasing = T) 

other_ind_viol <- c("lesion a persona protegida|lesion fisica|lesion por objetivos|detencion arbitraria|bienes civiles|rapto|tortura|violencia sexual")
coll_vio <- c("colectivo lesionado|hambre como metodo de guerra|ataque indiscriminado|colectivo escudo|colectivo desplazado|desplazamiento forzado|confinamiento colectivo|")
#Get victims indivators. Do not mix individual with collective outcomes.
pol_vio <- pol_vio %>% mutate(year = year(date), 
                                    month = month(date), 
                                    day = day(date),
                                    asis = as.integer(str_detect(type, "asesinato|ejecucion extrajudicial|homicidio")),
                                    fail_asis = as.integer(str_detect(type, "atentado")),
                                    non_leth_vio = as.integer(str_detect(type, other_ind_viol)),
                                    ind_threat = as.integer(str_detect(type, "amenaza")),
                                    collective_threat = as.integer(str_detect(type, "colectivo amenazado")),
                                    collective_violence = as.integer(str_detect(type, coll_vio)), 
                                    displacement = as.integer(str_detect(type, "desplazamiento forzado|colectivo desplazado"))
                                    )

#colSums(pol_vio %>% select(asis:displacement))

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
rownames(dist_name_matrix) <- unique(pol_vio$cod)

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

pol_vio <- pol_vio %>% mutate(report = report %>% str_replace_all("[\r\n]", "")) %>% 
  mutate(report =  report %>%  str_replace_all("\u0093", "") %>% str_replace_all("\\\\", ""))

pol_vio <- pol_vio %>% mutate(report = report %>% str_replace_all("[\r\n]", "") %>% tolower())

pol_vio <- pol_vio %>% select(-c(cod_vic_inv, cod))

# String detection patters
partidos_izq <- c("UNION PATRIOTICA|ALIANZA NACIONAL POPULAR|ANAPO|MOVIMIENTO AUTORIDADES INDIGENAS DE COLOMBIA|AICO|PARTIDO SOCIALDEMOCRATA COLOMBIANO|
MOVIMIENTO INDEPENDIENTE FRENTE DE ESPERANZA FE|MOVIMIENTO OBRERO INDEPENDIENTE REVOLUCIONARIO|MOIR|
MOVIMIENTO ALTERNATIVA DEMOCRATICA|MOVIMIENTO 19 DE ABRIL|PARTIDO UNIDAD DEMOCRATICA|MOVIMIENTO FRENTE SOCIAL Y POLITICO|
MOVIMIENTO AUTORIDADES INDIGENAS DE COLOMBIA|AICO|PARTIDO POLO DEMOCRATICO INDEPENDIENTE|POLO DEMOCRATICO ALTERNATIVO|MOVIMIENTO VAMOS IPIALES|
colombia humana|alianza social independiente|partido comunista colombiano|pacto historico") %>% tolower() 
partidos_izq <- gsub("[\r\n]", "", partidos_izq)

lider_sector <- c("lider comunitario|comunitaria|junta de accion comunal|JAC|consejo comunitario|lideresa comunitaria|defensor(a) de derechos humanos|defensor de derechos humanos|lider comunal|lider civico|lider social|lideresa")
indig_sector <- c("indigena|igena|minga|minguero|cabildo|resguardo|lidere(s) indigena(s)|embera")
campesino_sector <- c("campesino|reclamante|lider(es) campesino(s)|anuc")
politics_sector <- c("candidata|candidato|concejal|alcalde")

#sample(pol_vio$report, 3)

pol_vio <- pol_vio %>% mutate(
  lider_sector = as.integer(str_detect(report, lider_sector)), 
  campesino_sector = as.integer(str_detect(report, campesino_sector)),
  indig_sector = as.integer(str_detect(report, indig_sector)), 
  afro_sector = as.integer(str_detect(report, "afrodescen|negritud|palenque")), 
  sindical_sector = as.integer(str_detect(report, "sindic|obrer|sindicato")), 
  ambiental_sector = as.integer(str_detect(report, "ambiental|sostenible|minrero|mineria|consevacion ambiental|medio ambient|ecolog")), 
  pnis_sector = as.integer(str_detect(report, "pnis|sustitucion de cultivos")), 
  izq_sector = as.integer(str_detect(report, partidos_izq)), 
  ex_FARC = as.integer((str_detect(report, "excombatiente") & str_detect(report, "farc-ep")) | 
                         str_detect(report, "firmante|ex-farc|demovilizado")), 
  politics_sector =  as.integer(str_detect(report, politics_sector))
)

## Create RD dataset. 

pol_vio <- pol_vio %>% mutate(vio = 1, 
                              lid_asis = lider_sector*asis, 
                              lid_fail_asis = lider_sector*fail_asis, 
                              lid_threat = lider_sector*ind_threat,
                              lid_nl_vio = lider_sector*non_leth_vio,
                              )

#colSums(pol_vio %>% select(lider_sector:lid_nl_vio))

sum_cols <- colnames(pol_vio)[-c(1:13, 21, 22, 23)]
pol_vio$year <- as.character(pol_vio$year)
pol_vio_RD <- summarize_data_count(pol_vio, sum_cols, sum_cols)

write.csv(pol_vio_RD, "D:/Documents/GitHub/Thesis/Data/Final_data/pol_vio_SIVel_RD.csv")

## Create dataset for DiD setting. 


## Graphs

rm(pol_vio_try, pol_vio_c,mult_to_un, dist, dist_mult, dist_unique, sum_cols)
g1 <- pol_vio_RD %>% group_by(year) %>% summarise(lid = sum(lid_asis, na.rm = T)) %>% ungroup()
ggplot(data = g1) + geom_line(aes(x = year ,y = lid, group = 1))
