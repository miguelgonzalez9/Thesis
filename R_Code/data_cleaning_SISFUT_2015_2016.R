path <- "D:/Documents/GitHub/Thesis/Data/Conflicto Armado/SISFUT_Inversion_victimas_2015_2016"
setwd(path)
files<-list.files()
i <- 0
for (file in files) {
  if(i == 0){
    d<<-read_xlsx(paste0(path, "/", file))
    if(str_detect(file, pattern = "//(1//)")){
      d <<- d %>% mutate(year = 2015) 
    }
    else{
      d <<- d %>% mutate(year = 2016)
    }
  }
  else{
    d1 <- read_xlsx(paste0(path, "/", file))
    if(str_detect(file, pattern = "//(1//)")){
      d1 <- d1 %>% mutate(year = 2015) 
    }
    else{
      d1 <- d1 %>% mutate(year = 2016)
    }
    d <<- rbind(d,d1) 
  }
  i <<- i + 1
  
}

s <- read.csv("D:/Documents/GitHub/Thesis/Data/Final_data/SISFUT_Final/final_SISFUT_vict.csv")
s <- s %>% select(-c(trim, X, dpto))
cname <- c("cod_FUT","entidad", "cod_dpto","dpto" ,"cod_mun",        
           "mun" ,  "cod_concepto", "concepto" ,"cod_hecho_vict" , "hecho_vict",
           "cod_fuente_finan", "fuente_finan",  "presupuesto_inicial",  "presupuesto_defin",
           "compromisos" , "obligaciones", "pagos"  , "year")
colnames(d) <- cname
colnames(s) <- cname
data_SISFUT <- rbind(s, d)

write.csv(data_SISFUT,"D:/Documents/GitHub/Thesis/Data/Final_data/SISFUT_Final/SISFUT_fin_2015-2020.csv")
path <- "D:/Documents/GitHub/Thesis/R_Code"
setwd(path)
