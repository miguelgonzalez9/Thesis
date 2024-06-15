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
lideres$respon <- gsub("[\r\n]", "", lideres$respon)
lideres$social_sector <- gsub("[\r\n]", "", lideres$social_sector)
lideres$organization <- gsub("[\r\n]", "", lideres$organization)


lideres["respon"][lideres["respon"] == "desconocidos"] <- "unknown"
lideres["respon"][lideres["respon"] == "desconocido s"] <- "unknown"
lideres["respon"][lideres["respon"] == "desconocido"] <- "unknown"
lideres["respon"][lideres["respon"] == "desocnocido"] <- "unknown"
lideres["respon"][lideres["respon"] == "paramilitare s"] <- "paramilitary"
lideres["respon"][lideres["respon"] == "policia"] <- "policia nacional"
lideres["respon"][lideres["respon"] == "disidencia"] <- "disidencias"
lideres["respon"][lideres["respon"] == "desconocido"] <- "unknown"
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
         "esmad", "ejercito - gaor", "esmad", "ejercitonacional")

lideres <- lideres %>% mutate(
  insurgent_respon = as.integer(respon %in% insuregency), 
  paramilitary_respon = as.integer(respon %in% paramilitary), 
  bacrim_respon = as.integer(respon %in% criminal_bands), 
  gov_respon = as.integer(respon %in% gov),
  unkown_respon = as.integer(respon == "unknown")
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
                                str_remove(pattern = "//n|//r|-|") %>% tolower() %>% str_replace_all(" ", ""), 
                              social_sector = stri_trans_general(social_sector, id = "Latin-ASCII") %>% 
                                str_remove(pattern = "//n|//r|-|") %>% tolower() %>% str_replace_all(" ", "")
                              )

lideres <- lideres %>% mutate(
  comunal_sector = as.integer(str_detect(social_sector, "comunal|comunitaria")), 
  campesino_sector = as.integer(str_detect(social_sector, "campesin|tierras")),
  indigenas_sector = as.integer(str_detect(social_sector, "indigena|igena")), 
  afrodesendiente_sector = as.integer(str_detect(social_sector, "afrodescen")), 
  sindical_sector = as.integer(str_detect(social_sector, "sindic")), 
  ambiental_sector = as.integer(str_detect(social_sector, "ambiental")), 
  victimas_sector = as.integer(str_detect(social_sector, "victima")), 
  pnis_sector = as.integer(str_detect(social_sector, "pnis"))
)


# Clean Leaders Somos Defensores ----------------

lideres_sd <- read.csv("D:/Documents/GitHub/Thesis/Data/Conflicto Armado/Lideres_sociales/casos-somosdefensores_2000_2024.csv")
colnames(lideres_sd) <- c("id", "date", "location", "victims", "respon",  "type", "report")
lideres_sd$date <- as.Date(lideres_sd$date)

## Expand multi value cells --------------
lideres_sd <- lideres_sd %>% filter(location != "") %>% distinct()
lideres_sd <- lideres_sd %>% mutate(across(where(is.character), ~ as.integer(str_detect(.x, ",")), .names = "multi_{col}")) %>% 
  select(-multi_report) 

lideres_sd <- lideres_sd %>% mutate(type = case_when(
  str_detect(type, "MUERTO POR OBJETIVOS,") ~ "MUERTO POR OBJETIVOS METODOS Y MEDIOS ILICITOS", 
  T ~ type
))

# Keep order of type, victims. Do not add more variables to split. 
lideres_sd$non_match_comma <- NA
dim(lideres_sd) #1423
lideres_sd <- split_multicell(lideres_sd, c("victims", "type"), ", ")
# 1581

# Manually code non-matching observations
mlt_lid <- lideres_sd %>% filter(non_match_comma == 1) %>%  split_multicell(c("victims"), ", ")
t <- c("A:1:10 EJECUCIÃN EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA", 
       "A:1:16 ATENTADO", 
       
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA", 
       "A:1:16 ATENTADO", 
       
       "A:1:13 LESIÃN FÃSICA, D:4:702 LESIÃN A PERSONA PROTEGIDA", 
       "A:1:10 EJECUCIÃN EXTRAJUDICIAL,  HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       " A:1:13 LESION FÃSICA, D:4:702 LESION A PERSONA PROTEGIDA", 
       
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       " A:1:13 LESION FÃSICA, D:4:702 LESION A PERSONA PROTEGIDA",
       
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA",
       "A:1:10 EJECUCION EXTRAJUDICIAL, D:4:701 HOMICIDIO INTENCIONAL DE PERSONA PROTEGIDA")
if(length(t) == dim(mlt_lid)[1]){
  mlt_lid$type <- t
} else {
  warning("Manual match fail")
}
mlt_lid <- mlt_lid %>%  split_multicell(c("type"), ", ")
lideres_sd <- lideres_sd %>% filter(is.na(non_match_comma) | non_match_comma == 0) %>% 
  rbind(mlt_lid) %>% select(-non_match_comma)

dim(lideres_sd) # 1624

rm(mlt_lid, t)

  # Multi cell indicator
lideres_sd <- lideres_sd %>% group_by(id) %>% mutate(victs = n()) %>% 
  ungroup() %>% mutate(mult_type = as.integer(str_detect(type, ",")))

lideres_sd %>% filter(mult_type == 1) %>% pull(type) %>% unique()

## Extract muns and dptos -------------------------
loc <- str_split_fixed(lideres_sd$location, pattern = "/", n = 2)
lideres_sd$dpto <- loc[,1]
lideres_sd$mun <- loc[,2]
# Get rid of leaders without municipality. 
lideres_sd <- lideres_sd %>%  select(-location) %>% filter(mun != " ")
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
  str_detect(respon, "otro") ~ "unknown",
  str_detect(respon, "desconocido") ~  "unknown", 
  str_detect(respon, "sin informacion") ~ "unknown", 
  T ~ respon
))

unique(lideres_sd$respon)
insuregency <- c("eln|epl|disidencia farc|farc|grupos postacuerdo de paz|guerrilla|farc-ep|ejercito de liberacion nacional")
paramilitary <- c("paramilitares|auc|milicias")
criminal_bands <- c("bandas criminales|alianzas criminales")
gov <- c("fuerza publica|ejercito|fuerza aerea|policia|ejercito|estado colombiano|ejercito|agentes estado")

## Create dummy variables -----------------------------------

lideres_sd <- lideres_sd %>% mutate(
  insurgent_respon = as.integer(str_detect(respon, insuregency)),
  param_respon = as.integer(str_detect(respon, paramilitary)), 
  bacrim_respon = as.integer(str_detect(respon, criminal_bands)), 
  gov_respon =as.integer(str_detect(respon, gov)),
  unkown_respon = as.integer(str_detect(respon, "unknown")), 
  indiv_respon = as.integer(str_detect(respon, "autores individuales"))
)



lideres_sd <- lideres_sd %>% mutate(no_cat = case_when(
  insurgent_respon == 0 & param_respon == 0 & bacrim_respon == 0 & 
    gov_respon == 0 & unkown_respon == 0 & indiv_respon == 0 ~ 1, 
  T ~ 0))

if(any(lideres_sd$no_cat == 1)){
  warning("Missing categories")
}

lideres_sd <- lideres_sd %>% select(-no_cat)

## Clean municipality code. ------------------------------

lideres_sd <- lideres_sd %>% mutate(mun = stri_trans_general(mun, id = "Latin-ASCII") %>% 
                                str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                str_replace_all(" ", ""), 
                              dpto = stri_trans_general(dpto, id = "Latin-ASCII") %>% 
                                str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                str_replace_all(" ", ""))

lideres_sd <- lideres_sd %>% mutate(cod = stri_trans_general(paste0(mun, dpto), id = "Latin-ASCII") %>% 
                                str_remove(pattern = "//n|//r|-|") %>% tolower() %>% 
                                str_replace_all(" ", ""))

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


#table(lideres_sd$respon, lideres_sd$year)

## Clean organization variables. Following Ávila (2019) p.46 taxonomy of leaders -------------------------

lideres_sd <- lideres_sd %>% mutate(report = report %>% str_replace_all("[\r\n]", ""))

lideres_sd <- lideres_sd %>% select(-c(cod_vic_inv, cod))

partidos_izq <- c("UNION PATRIOTICA|ALIANZA NACIONAL POPULAR|ANAPO|MOVIMIENTO AUTORIDADES INDIGENAS DE COLOMBIA|AICO|PARTIDO SOCIALDEMOCRATA COLOMBIANO|
MOVIMIENTO INDEPENDIENTE FRENTE DE ESPERANZA FE|MOVIMIENTO OBRERO INDEPENDIENTE REVOLUCIONARIO|MOIR|
MOVIMIENTO ALTERNATIVA DEMOCRATICA|MOVIMIENTO 19 DE ABRIL|PARTIDO UNIDAD DEMOCRATICA|MOVIMIENTO FRENTE SOCIAL Y POLITICO|
MOVIMIENTO AUTORIDADES INDIGENAS DE COLOMBIA|AICO|PARTIDO POLO DEMOCRATICO INDEPENDIENTE|POLO DEMOCRATICO ALTERNATIVO|MOVIMIENTO VAMOS IPIALES|
colombia humana|alianza social independiente|partido comunista colombiano|pacto historico") %>% tolower() 
partidos_izq <- gsub("[\r\n]", "", partidos_izq)
indig_sector <- c("indigena|igena|minga|minguero|cabildo|resguardo|lidere(s) indigena(s)|embera")
campesino_sector <- c("campesino|reclamante|lider(es) campesino(s)|anuc")
politics_sector <- c("candidata|candidato|concejal|alcalde")


lideres_sd <- lideres_sd %>% mutate(
  comunal_sector = as.integer(str_detect(report, "comunal|comunitaria|junta de accion comunal|JAC|consejo comunitario|defensa derechos humanos|defensor derechos humanos")),
  campesino_sector = as.integer(str_detect(report, campesino_sector)),
  indig_sector = as.integer(str_detect(report, indig_sector)), 
  afro_sector = as.integer(str_detect(report, "afrodescen|negritud|palenque")), 
  sindical_sector = as.integer(str_detect(report, "sindic|obrer|sindicato")), 
  ambiental_sector = as.integer(str_detect(report, "ambiental|sostenible|minrero|mineria|consevacion|medio ambient|ecolog")),
  pnis_sector = as.integer(str_detect(report, "pnis|sustitucion de cultivos")), 
  izq_sector = as.integer(str_detect(report, partidos_izq)),
  politics_sector =  as.integer(str_detect(report, politics_sector))
)

#colSums(lideres_sd %>% select(comunal_sector:politics_sector))
lideres_sd$type %>% unique()

# Clean type variable
lideres_sd <- lideres_sd %>% mutate(type = case_when(
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
#lideres_sd$type %>% table() %>% sort(decreasing = T)
# Asesinato 1025      
# Filter killings only
lideres_sd <- lideres_sd %>% 
  filter(type %in% c("asesinato", "ejecucion extrajudicial", "homicidio intencional de persona protegida", 
                     "muerto por objetivos metodos y medios ilicitos")) %>% 
  mutate(killing = 1)


### Graphs ---------------------------

lideres_RD <- lideres %>% group_by(year, codmpio) %>% summarise(
  lid = n()) %>% ungroup() %>% mutate(source_lid = "INDEPAZ")

lideres_RD_sd <- lideres_sd %>% group_by(year, codmpio) %>% summarise(
  lid = n()) %>% mutate(source_lid = "SD")


lideres_g <- rbind(lideres_RD,lideres_RD_sd)
lideres_g <- lideres_g %>% group_by(year, source_lid) %>% summarise(lid = sum(lid, na.rm = T))

ggplot(lideres_g, aes(x = year, y =lid, color = source_lid)) + geom_line() + 
  theme_bw() + ylab("Leader Killings") + xlab("Year") +scale_colour_grey() +
  labs(color = "Source")


ggplot(lideres_g %>% filter(source_lid == "INDEPAZ"), aes(x = year, y =lid)) + geom_line() + 
  theme_bw() + ylab("Leader Killings") + xlab("Year") + scale_colour_grey() +
  labs(color = "Source")

lideres_INDEPAZ_month <- lideres %>% group_by(year, month) %>% summarise(
  lid = n()) %>% ungroup() %>% mutate(date = ym(paste0(year, "-",month))) %>% 
  filter(year != 2024)



fg_1 <- ggplot(lideres_INDEPAZ_month, aes(x = date, y =lid)) + geom_line() + 
  theme_bw() + ylab("Leader Killings") + xlab("Year") + scale_colour_grey() 

ggsave(filename = "Figure1A.pdf", plot = fg_1, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")

# RD datasets -------------------------
## Indepaz


colnames(lideres)[str_detect(colnames(lideres), "(?<!social)_sector$|_respon$")] <- paste0("lid_", colnames(lideres)[str_detect(colnames(lideres), "(?<!social)_sector$|_respon$")])
sum_cols <- colnames(lideres)[str_detect(colnames(lideres), "(?<!social)_sector$|_respon$|lid_assas")]
lideres_RD <- summarize_data_count(lideres, sum_cols, sum_cols)
sum_cols_sd <- colnames(lideres_sd)[-c(1:17, 24)]
lideres_RD_sd <- summarize_data_count(lideres_sd,sum_cols_sd,paste0(sum_cols_sd, "_sd"))
lideres_RD <- full_join(lideres_RD, lideres_RD_sd) %>% complete(year, codmpio) %>% 
  mutate(across(!year & !codmpio, ~ replace_na(.x, 0)))
lideres_RD <- lideres_RD %>% select(-c(bacrim_respon_sd, indiv_respon_sd, izq_sector_sd))
#colSums(lideres_RD %>% select(-c(year, codmpio)))
# Key variables lid_assas and killing_sd. Look at 
#: comunal_sector_sd,  lid_comunal_sector, indig_sector_sd lid_indigenas_sector, 
#lid_campesino_sector campesino_sector_sd. 

write.csv(lideres_RD, "D:/Documents/GitHub/Thesis/Data/Final_data/leaders_RD.csv")
