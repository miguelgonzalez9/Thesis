## Load data

vict_inv <- read.csv("D:/Documents/GitHub/Thesis/Data/Final_data/SISFUT_Final/SISFUT_fin_2015-2020.csv")
lideres <- read.csv("D:/Documents/GitHub/Thesis/Data/Conflicto Armado/Lideres_sociales/Listado_2016-2023.csv", sep = ";")
lideres_sd <- read.csv("D:/Documents/GitHub/Thesis/Data/Conflicto Armado/Lideres_sociales/casos-somosdefensores_2000_2023.csv")
mun_char <- read_dta("D:/Documents/GitHub/Thesis/Data/CEDE/PANEL_CARACTERISTICAS_GENERALES(2022).dta")


# Clean Data Leaders INDEPAZ ----------------------------------


colnames(lideres) <- c("No", "nombre", "sex", "date", "mun", "dpto", "respon", "organization", "social_sector", "organization_cacep")
lideres <- lideres %>% mutate(date = dmy(date), 
                              year = year(date), 
                              month = month(date), 
                              day = day(date),
                              lid_assas = 1, 
                              cod = paste0(mun,year,dpto), 
                              respon = stri_trans_general(respon, id = "Latin-ASCII") %>% 
                                str_replace_all(pattern = "//n", " ") %>% tolower())

## Clean respon variable--------------------------------

lideres["respon"][lideres["respon"] == "desconocido s"] <- "unknown"
lideres["respon"][lideres["respon"] == "desconocido s"] <- "unknown"
lideres["respon"][lideres["respon"] == "paramilitare s"] <- "paramilitary"
lideres["respon"][lideres["respon"] == "policia"] <- "policia nacional"
lideres["respon"][lideres["respon"] == "disidencia"] <- "disidencias"
lideres["respon"][lideres["respon"] == "desconocido"] <- "unknown"
lideres["respon"][lideres["respon"] == "omar marrugo villadiego - excompanero sentimental"] <- "remove"
lideres["respon"][lideres["respon"] == ""] <- NA
lideres["social_sector"][lideres["social_sector"] == ""] <- NA
lideres["organization_cacep"][lideres["organization_cacep"] == ""] <- NA
lideres["organization"][lideres["organization"] == ""] <- NA

insuregency <- c("eln", "epl", "disidencias")
paramilitary <- c("paramilitary", "agc", "seguridad privada", "augc", "acg", 
                  "paramilitares - clan de sinaloa", "clan del golfo")
criminal_bands <- c("urabenos", "gaor", "aguilas negras", "caparrapos", 
                    "ejercito - gaor")
gov <- c("policia - ejercito nacional", "ejercito", "policia nacional", 
         "ejercito colombiano - xi brigada", "policia - ejercito", "ejercito nacional", 
         "esmad", "ejercito - gaor")

lideres <- lideres %>% mutate(
  insurgent_respon = as.integer(respon %in% insuregency), 
  paramilitary_respon = as.integer(respon %in% paramilitary), 
  bacrim_respon = as.integer(respon %in% criminal_bands), 
  gov_respon = as.integer(respon %in% gov)
)

## Clean municipality code. ----------------------

lideres <- lideres %>% mutate(mun = stri_trans_general(mun, id = "Latin-ASCII") %>% 
                                str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                str_replace_all(" ", ""), 
                              dpto = stri_trans_general(dpto, id = "Latin-ASCII") %>% 
                                str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                str_replace_all(" ", ""))

lideres <- lideres %>% mutate(cod = stri_trans_general(paste0(mun, dpto), id = "Latin-ASCII") %>% 
                                                str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                                str_replace_all(" ", "")) %>% 
  filter(mun != "")

mnames <- mun_char %>% select(municipio, depto, codmpio) %>% distinct() %>% 
  mutate(cod = stri_trans_general(paste0(municipio, depto), id = "Latin-ASCII") %>% 
                                  str_remove(pattern = "//n|//r|-|") %>% tolower()%>% 
                                  str_replace_all(" ", ""))

dist_name_matrix <- adist(unique(lideres$cod), unique(mnames$cod), partial = TRUE, ignore.case = TRUE)
colnames(dist_name_matrix) <- unique(mnames$cod)
rownames(dist_name_matrix) <- unique(lideres$cod)

dist_df <- dist_name_matrix %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(., "cod_inv") %>% 
  pivot_longer(cols = 2:last_col(), names_to = "cod_lid", values_to = "dist") %>% 
  filter(dist < 3)


### Probabilistic merge ----------------

min_dist_str <- function(x,y){
  dist <- stringdist(x,  y, method = "lv")
  m_dist <- min(dist)
  match_n <- y[dist == m_dist]
  return(match_n)
}

dist_unique <- data.frame()
dist_mult <- data.frame()
for(i in 1:length(unique(lideres$cod))) {
  x <- min_dist_str(unique(lideres$cod)[i], unique(mnames$cod))
  print(x)
  if(length(x) == 1){
    dist_unique <- rbind(dist_unique, c(x, unique(lideres$cod)[i]))
  }
  if (length(x) > 1){
    dist_mult <- rbind(dist_mult, c(paste(x, collapse = "-----"), unique(lideres$cod)[i]))
  }
}

mult_to_un <- matrix(c("riosuciochoco", "pedeguitachoco", "calivalledelcauca", "santiagodecalivalledelcauca",
                       "puracecauca", "paletaracauca", "guadalajaradebugavalledelcauca", "bugavalledelcauca",
                       "patiacauca", "elbordocauca", "laplayanortedesantander", "playadebelennortedesantander",
                       "cacotanortedesantander", "lejiaarauca", "apartadoantioquia", "sanjosedeapartadocordoba", 
                       "sanjosedeurecordoba", "sanjosedeurebolivar","sanbenitoabadsucre", "sanbasiliosucre", 
                       "eltambocauca", "mosqueracauca", "fortularauca", "fortulcasanare"), ncol = 2, byrow = T) %>% as.data.frame()
### Merge-----------------
colnames(dist_unique) <- c("cod_vic_inv", "cod_lid")
colnames(mult_to_un) <- c("cod_vic_inv", "cod_lid")
dist <- rbind(dist_unique,mult_to_un)
dist <- dist %>% left_join(mnames %>% select(-c(depto, municipio)) %>% unique(), by = (c("cod_vic_inv" = "cod")))
lideres <- lideres %>% left_join(dist, by = c("cod" = "cod_lid"))
if (dim(lideres[is.na(lideres$codmpio),])[1] != 0){
  warning("NAs in merge")
}
### 5 Digit code ---------------
lideres$codmpio <- as.character(lideres$codmpio)
lideres <- lideres %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
))

## Clean organization variables.Following Ávila (2019) p.46 taxonomy of leaders -------------------------

lideres$organization %>% unique()
lideres$social_sector %>% unique()

# Social sector. 
lideres <- lideres %>% mutate(organization = stri_trans_general(organization, id = "Latin-ASCII") %>% 
                                str_remove(pattern = "//n|//r|-|") %>% tolower(), 
                              social_sector = stri_trans_general(social_sector, id = "Latin-ASCII") %>% 
                                str_remove(pattern = "//n|//r|-|") %>% tolower() 
                              )


lideres <- lideres %>% mutate(
  comunal_sector = as.integer(str_detect(social_sector, "comunal|comunitaria")), 
  lgbt_sector = as.integer(str_detect(social_sector, "lgtb|lgbt")), 
  campesino_sector = as.integer(str_detect(social_sector, "campesin|tierras")),
  indigenas_sector = as.integer(str_detect(social_sector, "indigena|igena")), 
  afrodesendiente_sector = as.integer(str_detect(social_sector, "afrodescen|igena")), 
  sindical_sector = as.integer(str_detect(social_sector, "sindic")), 
  ambiental_sector = as.integer(str_detect(social_sector, "ambiental")), 
  victimas_sector = as.integer(str_detect(social_sector, "victima")), 
  pnis_sector = as.integer(str_detect(social_sector, "pnis"))
)


# Clean Leaders Somos Defensores ----------------

lideres_sd <- read.csv("D:/Documents/GitHub/Thesis/Data/Conflicto Armado/Lideres_sociales/casos-somosdefensores_2000_2023.csv")
colnames(lideres_sd) <- c("id", "date", "location", "victims", "respon",  "type", "report")
lideres_sd$date <- as.Date(lideres_sd$date)

## Expand multi value cells --------------
lideres_sd <- lideres_sd %>% filter(location != "") %>% distinct()
lideres_sd <- lideres_sd %>% mutate(across(where(is.character), ~ as.integer(str_detect(.x, ",")), .names = "multi_{col}")) %>% 
  select(-multi_report) 

lideres_sd <- split_multicell(lideres_sd, "victims", ", ")
  # Multi cell indicator
lideres_sd <- lideres_sd %>% group_by(id) %>% mutate(dup = n() - 1) %>% ungroup()

## Extract muns and dptos -------------------------
loc <- str_split_fixed(lideres_sd$location, pattern = "/", n = 2)
lideres_sd$dpto <- loc[,1]
lideres_sd$mun <- loc[,2]
# Get rid of leaders without municipality. 
lideres_sd <- lideres_sd %>%  select(-location) %>% filter(mun != "")
lideres_sd <- lideres_sd %>% mutate(mun = case_when(
  victims == "ELISA  MORA UNCACIA" ~ "Saravena", 
  victims == "JAIME  REYES SAMPIER" ~ "Tame",
  T ~ mun
))

## Clean string variables --------------------------
#View(lideres_sd %>% select(mun, dpto) %>% unique())

lideres_sd <- lideres_sd %>% mutate(across(where(is.character), clean_string)) %>% 
  mutate(across(where(is.character), ~ stri_trans_general(.x, id = "Latin-ASCII"))) %>% 
  mutate(across(where(is.character), ~ gsub(pattern = "-",replacement = "", x = .x, ))) %>% 
  mutate(across(where(is.character),  trimws))
lideres_sd["mun"][lideres_sd["mun"] == "norosi,  /"] <- "norosi"
lideres_sd <- lideres_sd %>% filter(mun != "")

lideres_sd <- lideres_sd %>% mutate(year = year(date), 
                              month = month(date), 
                              day = day(date),
                              lid_assas = 1, 
                              cod = paste0(mun,year,dpto))

## Clean respon variable--------------------------------
#View(lideres_sd %>% filter(respon %in% c("sin informacion, otros, disidencia farc")))
#lideres_sd %>% filter(id %in% c("130328"))


lideres_sd["respon"][lideres_sd["victims"] == "hernan emilio  quitumbo"] <- "farc"
lideres_sd["respon"][lideres_sd["victims"] == "fabio  zapata"] <- "farc"
lideres_sd["respon"][lideres_sd["victims"] == "maria zuleima  coicue"] <- "sin informacion"
lideres_sd["respon"][lideres_sd["id"] == "130328"] <- "disidencia farc"
lideres_sd["respon"][lideres_sd["victims"] == "edilma rosa cuevas"] <- "bandas criminales"

lideres_sd <- lideres_sd %>% mutate(respon = case_when(
  respon == "otros, otros" ~ "unknown", 
  respon == "desconocido" ~ "unknown",
  respon == "sin informacion" ~ "unknown",
  respon == "otros, otros" ~ "unknown",
  respon == "desconocidos" ~ "unknown",
  respon == "desconocido" ~ "unknown",
  respon == "otros" ~ "unknown",
  
  respon == "farcep" ~ "farc",
  T ~ respon
))

unique(lideres_sd$respon)
insuregency <- c("eln|epl|disidencia farc|farc|grupos postacuerdo de paz|guerrilla|farc-ep|ejercito de liberacion nacional|
                 ")
paramilitary <- c("paramilitares|auc|milicias")
criminal_bands <- c("bandas criminales|alianzas criminales")
gov <- c("fuerza publica|ejercito|fuerza aerea|policia|ejercito|estado colombiano|ejercito|agentes estado")

## Create dummy variables -----------------------------------

lideres_sd <- lideres_sd %>% mutate(
  insurgent_respon = as.integer(str_detect(respon, insuregency) | str_detect(report, insuregency)),
  param_respon = as.integer(str_detect(respon, paramilitary) | str_detect(report, paramilitary)), 
  bacrim_respon = as.integer(str_detect(respon, criminal_bands) | str_detect(report, criminal_bands)), 
  gov_respon =as.integer(str_detect(respon, gov) | str_detect(report, gov))
)

## Clean municipality code. ------------------------------

lideres_sd <- lideres_sd %>% mutate(mun = stri_trans_general(mun, id = "Latin-ASCII") %>% 
                                str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                str_replace_all(" ", ""), 
                              dpto = stri_trans_general(dpto, id = "Latin-ASCII") %>% 
                                str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                str_replace_all(" ", ""))

lideres_sd <- lideres_sd %>% mutate(cod = stri_trans_general(paste0(mun, dpto), id = "Latin-ASCII") %>% 
                                str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                str_replace_all(" ", "")) %>% 
  filter(mun != "")

mnames <- mun_char %>% select(municipio, depto, codmpio) %>% distinct() %>% 
  mutate(cod = stri_trans_general(paste0(municipio, depto), id = "Latin-ASCII") %>% 
           str_remove(pattern = "//n|//r|-|") %>% tolower()%>% 
           str_replace_all(" ", ""))

dist_name_matrix <- adist(unique(lideres_sd$cod), unique(mnames$cod), partial = TRUE, ignore.case = TRUE)
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
for(i in 1:length(unique(lideres_sd$cod))) {
  x <- min_dist_str(unique(lideres_sd$cod)[i], unique(mnames$cod))
  print(x)
  if(length(x) == 1){
    dist_unique <- rbind(dist_unique, c(x, unique(lideres_sd$cod)[i]))
  }
  if (length(x) > 1){
    dist_mult <- rbind(dist_mult, c(paste(x, collapse = "-----"), unique(lideres_sd$cod)[i]))
  }
}

mult_to_un <- matrix(c("guadalajaradebugavalledelcauca", "bugavalledelcauca"), ncol = 2, byrow = T) %>% as.data.frame()
# Merge
colnames(dist_unique) <- c("cod_vic_inv", "cod_lid")
colnames(mult_to_un) <- c("cod_vic_inv", "cod_lid")
dist <- rbind(dist_unique,mult_to_un)
dist <- dist %>% left_join(mnames %>% select(-c(depto, municipio)) %>% unique(), by = (c("cod_vic_inv" = "cod")))
lideres_sd <- lideres_sd %>% left_join(dist, by = c("cod" = "cod_lid"))
if (dim(lideres_sd[is.na(lideres_sd$codmpio),])[1] != 0){
  warning("NAs in merge")
}

### 5 Digit code ------------------------
lideres_sd$codmpio <- as.character(lideres_sd$codmpio)
lideres_sd <- lideres_sd %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
  ))

## Clean organization variables. Following Ávila (2019) p.46 taxonomy of leaders -------------------------

lideres_sd <- lideres_sd %>% mutate(report = report %>% str_replace_all("[\r\n]", ""))

lideres_sd <- lideres_sd %>% select(-c(cod_vic_inv, cod))
partidos_izq <- c("UNION PATRIOTICA|ALIANZA NACIONAL POPULAR|ANAPO|MOVIMIENTO AUTORIDADES INDIGENAS DE COLOMBIA|AICO|PARTIDO SOCIALDEMOCRATA COLOMBIANO|
MOVIMIENTO INDEPENDIENTE FRENTE DE ESPERANZA FE|MOVIMIENTO OBRERO INDEPENDIENTE REVOLUCIONARIO|MOIR|
MOVIMIENTO ALTERNATIVA DEMOCRATICA|MOVIMIENTO 19 DE ABRIL|PARTIDO UNIDAD DEMOCRATICA|MOVIMIENTO FRENTE SOCIAL Y POLITICO|
MOVIMIENTO AUTORIDADES INDIGENAS DE COLOMBIA|AICO|PARTIDO POLO DEMOCRATICO INDEPENDIENTE|POLO DEMOCRATICO ALTERNATIVO|MOVIMIENTO VAMOS IPIALES|
colombia humana|alianza social independiente|partido comunista colombiano|gustavo petro|pacto historico") %>% tolower() %>% str_replace("\n", "")
lideres_sd <- lideres_sd %>% mutate(
  comunal_sector = as.integer(str_detect(report, "comunal|comunitaria|junta de accion comunal|JAC|consejo comunitario")), 
  lgbt_sector = as.integer(str_detect(report, "lgtb|lgbt")), 
  campesino_sector = as.integer(str_detect(report, "campesin|tierras|reclamante")),
  indig_sector = as.integer(str_detect(report, "indigena|igena|minga|minguero|cabildo")), 
  afro_sector = as.integer(str_detect(report, "afrodescen|negritud|palenque")), 
  sindical_sector = as.integer(str_detect(report, "sindic|obrer")), 
  ambiental_sector = as.integer(str_detect(report, "ambiental|sostenible|minrero|mineria|consevacion|medio ambient|ecolog")), 
  victim_sector = as.integer(str_detect(report, "victima|desplazad")), 
  pnis_sector = as.integer(str_detect(report, "pnis|sustitucion de cultivos")), 
  izq_sector = as.integer(str_detect(report, partidos_izq)), 
  mujer_secor = as.integer(str_detect(report, "femenina|feminismo|feminista"))
)

## Extract age ------------------------------------ 
lideres_sd$age <- NA
for (i in 1:nrow(lideres_sd)) {
  pat <- paste0("(?<=", str_c(str_split_1(lideres_sd$victims[i], "[:blank:]+"), collapse = "."),".de.)[:digit:]+")
  a <- str_extract_all(lideres_sd$report[i],  pattern = "[:digit:]+(?=[:blank:]+anos)") %>% unlist()
  if (length(unique(a)) == 1) {
    lideres_sd$age[i] <- unique(a)
  }
  if (length(unique(a)) > 1){
    lideres_sd$age[i] <- str_c(unique(a), collapse = "---")
  }
  
}

# RD leaders -----------------------------------------------
lideres_RD <- lideres %>% group_by(year, codmpio) %>% summarise(
  lid = n()) %>% ungroup() %>% mutate(source_lid = "INDEPAZ")

lideres_RD_sd <- lideres_sd %>% group_by(year, codmpio) %>% summarise(
  lid = n()) %>% mutate(source_lid = "SD")

## Graphs ---------------------------

lideres_g <- rbind(lideres_RD,lideres_RD_sd)
lideres_g <- lideres_g %>% group_by(year, source_lid) %>% summarise(lid = sum(lid, na.rm = T)) %>% 
  filter(year != "2023")

ggplot(lideres_g, aes(x = year, y =lid, color = source_lid)) + geom_line()

lideres_RD <- lideres %>% group_by(year, codmpio) %>% summarise(
  lid_INDEPAZ = n()) %>% ungroup() 
lideres_RD_sd <- lideres_sd %>% group_by(year, codmpio) %>% summarise(
  lid_SD = n()) %>% ungroup()
lideres_RD <- full_join(lideres_RD, lideres_RD_sd) %>% complete(year, codmpio,  fill = list(lid_SD = 0, lid_INDEPAZ = 0))

## Subpop variables --------------------------

subpop_lid <- function(df, var_group, group, num_cond = T){
  if(!is.numeric(group) & num_cond){
    stop("Argument not numeric")
  }
  df_temp <- lideres_sd[lideres_sd[var_group] == group,]
  
  df_temp <- df_temp %>% group_by(year, codmpio) %>% summarise(lid = n()) %>% ungroup()
  colnames(df_temp)[3] <- paste0(var_group, "_lid")
  return(df_temp)

}

spec_subpop = c("param_respon", "insurgent_respon","gov_respon", "comunal_sector", "izq_sector", "pnis_sector")

for (i in spec_subpop) {
  lideres_RD <- lideres_RD %>% left_join(subpop_lid(lideres_sd, i, 1, T), by = c("year", "codmpio"))
}

lideres_RD <- lideres_RD %>% 
  mutate(across(param_respon_lid:pnis_sector_lid, 
                ~ ifelse(is.na(.x), 0, .x)))


