library(lubridate)
library(dplyr)
library(ggplot2)
library(fuzzyjoin)
library(tidyr)
library(stringi)
library(stringr)
library(stringdist)
# Load data -----------------------------------------------------
vict_inv <- read.csv("D:/Documents/Semestre 2023-II/Honors/Conflicto Armado/SISFUT_Final/SISFUT_fin_2015-2020.csv")
lideres <- read.csv("D:/Documents/Semestre 2023-II/Honors/Conflicto Armado/Líderes sociales/Listado_2016-2023.csv", sep = ";")

# Transform Data ----------------------------------

## RDD Treatment rule: two months before july --> 1; two month after --> 0. RDD -------------------
colnames(lideres) <- c("No", "nombre", "sexo", "fecha", "mun", "dpto", "respon", "organizacion", "sector_social", "organi_cacep")
lideres <- lideres %>% mutate(fecha = dmy(fecha), 
                              treat = case_when(
                                month(fecha) <= 6 ~ 1,
                                month(fecha) >= 8 ~ 0,
                                month(fecha) == 7 ~ NA), 
                              year = year(fecha), 
                              month = month(fecha), 
                              lid_assas = 1, 
                              cod = paste0(mun,year,dpto))
lideres <- lideres %>% spread(key = respon, value)


lideres_mun_RD <- lideres %>% group_by(dpto, mun, year) %>% summarise(treated = ifelse(any(treat == 1), 1, 0),
                                                                   treat_inten = sum(treat),
                                                             respon = paste(unique(respon), collapse = "\n"), 
                                                             organizacion = paste(c(unique(organizacion), unique(organi_cacep)), collapse = "\n"))

table(lideres_mun_RD$treated, lideres_mun$year)

## DID. Treatment rule: social leader killing on that year. ------------------


### Rule -------------------------
lideres <- lideres %>% mutate(mun = stri_trans_general(mun, id = "Latin-ASCII") %>% 
                              str_remove(pattern = "\\n|\\r|-|") %>% tolower() %>% 
                              str_replace_all(" ", ""), 
                            dpto = stri_trans_general(dpto, id = "Latin-ASCII") %>% 
                              str_remove(pattern = "\\n|\\r|-|") %>% tolower() %>% 
                              str_replace_all(" ", ""))

lideres_mun_DID <- lideres %>% group_by(dpto, mun, year) %>% summarise(treated = ifelse(any(lid_assas == 1), 1, 0),
                                                                       treat_inten = sum(lid_assas),
                                                                       respon = paste(unique(respon), collapse = "---"), 
                                                                       organizacion = paste(c(unique(organizacion), unique(organi_cacep)), collapse = "\n"),
                                                                       nombres = paste(nombre, collapse = "---"))

### Clear identifyer ------------------
lideres_mun_DID <- lideres_mun_DID %>% mutate(cod = stri_trans_general(paste0(mun, dpto), id = "Latin-ASCII") %>% 
                                                str_remove(pattern = "\\n|\\r|-|") %>% tolower() %>% 
                                                str_replace_all(" ", "")) %>% 
  filter(mun != "")

vict_inv <- vict_inv %>% mutate(cod = stri_trans_general(paste0(mun, dpto), id = "Latin-ASCII") %>% 
                                  str_remove(pattern = "\\n|\\r|-|") %>% tolower()%>% 
                                  str_replace_all(" ", ""))

### Distance matrix---------------------------
dist_name_matrix <- adist(unique(lideres_mun_DID$cod), unique(vict_inv$cod), partial = TRUE, ignore.case = TRUE)
colnames(dist_name_matrix) <- unique(vict_inv$cod)
rownames(dist_name_matrix) <- unique(lideres_mun_DID$cod)

dist_df <- dist_name_matrix %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(., "cod_inv") %>% 
  pivot_longer(cols = 2:last_col(), names_to = "cod_lid", values_to = "dist") %>% 
  filter(dist < 3)

cod_inv_dup <- c("mapiripiboyaca", "cartagena del cahiracaqueta", "riacurtenarino", )

### Probabilistic merge ----------------

min_dist_str <- function(x,y){
  dist <- stringdist(x,  y, method = "lv")
  m_dist <- min(dist)
  match_n <- y[dist == m_dist]
  return(match_n)
}

dist_unique <- data.frame()
dist_mult <- data.frame()
for(i in 1:length(unique(lideres_mun_DID$cod))) {
  x <- min_dist_str(unique(lideres_mun_DID$cod)[i], unique(vict_inv$cod))
  print(x)
  if(length(x) == 1){
    dist_unique <- rbind(dist_unique, c(x, unique(lideres_mun_DID$cod)[i]))
  }
  if (length(x) > 1){
    dist_mult <- rbind(dist_mult, c(paste(x, collapse = "-----"), unique(lideres_mun_DID$cod)[i]))
  }
}

mult_to_un <- matrix(c("sanjosedeurecordoba", "sanjosedeurebolivar", "patiacauca", "elbordocauca", 
                           "eltambocauca", "mosqueracauca", "puracecauca", "paletaracauca", 
                           "riosuciochoco", "pedeguitachoco", "fortularauca", "fortulcasanare", 
                           "magaoeinarino", "maguipayannarino", "laplayanortedesantander", "playadebelennortedesantander",
                           "puertosantandernortedesantander","puertosantandersantander", "mahatesbolivar", "sanbasiliosucre",
                           "buenaventuravalledelcauca", "buenaventuravalle", "guadalajaradebugavalledelcauca", "bugavalledelcauca",
                           "calivalledelcauca", "santiagodecalivalledelcauca"), ncol = 2, byrow = T) %>% as.data.frame()
colnames(dist_unique) <- c("cod_vic_inv", "cod_lid")
colnames(mult_to_un) <- c("cod_vic_inv", "cod_lid")
dist <- rbind(dist_unique,mult_to_un)

### Merge ----------------
full_d_DID <- vict_inv %>% 
  left_join(., dist, by = c("cod" = "cod_vic_inv")) %>% 
  left_join(., lideres_mun_DID, by = c("cod_lid" = "cod", "year" = "year"))

full_d_DID <- full_d_DID %>% mutate(
  treated = case_when(
  is.na(treated) ~ 0, 
  T ~ treated),
  treat_inten = case_when(
    is.na(treat_inten) ~ 0,
    T ~ treat_inten), 
  mun.x = stri_trans_general(mun.x, id = "Latin-ASCII") %>% 
    str_remove(pattern = "\\n|\\r|-|") %>% tolower()%>% 
    str_replace_all(" ", ""))

# Capital cities
full_d_DID <- full_d_DID %>% mutate(cap = case_when(
  str_detect(as.character(cod_mun), "001$") ~ 1, 
  T~0
))


# Analize DID data-------------------------
## General investment
gen_inv <- full_d_DID %>% filter(cod_concepto == "V" & !is.na(mun.x) & cap ==0) %>%
  select(cod_dpto, cod_mun, cod_hecho_vict, cod_fuente_finan,
         treated, treat_inten, pagos, year, mun.x, concepto, hecho_vict)
gen_inv <- gen_inv %>% mutate(time_treat = 0)
for (j in gen_inv$cod_mun) {
  x <- gen_inv[gen_inv$cod_mun == j, c("year", "treated", "time_treat")]
  x <- x %>% arrange(year)
  for (i in 1:dim(x)[1]) {
    if(x$treated[i] == 1){
      x$treated[i:length(x$treated)] <- 1
      x$time_treat[i] <- 0
      x$time_treat[(i+1):length(x$treated)] <- 1:(length(x$treated) - (i+1))
      x$time_treat[1:(i-1)] <- -1*c((i-1):1)
      gen_inv[gen_inv$cod_mun == j, c("year", "treated", "time_treat")] <- x
      break
    }
  }
}
gen_inv <- gen_inv %>% mutate(time_treat = case_when(
  
))

gen_inv <- gen_inv %>% group_by(cod_mun, year) %>% mutate(treated = )
