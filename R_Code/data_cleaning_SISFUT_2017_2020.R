path <- "D:/Documents/GitHub/Thesis/Data/Conflicto Armado/SISFUT_Inversion_victimas_2017_2020"
setwd(path)
files<-file.info(list.files()[-c(1,2)])
files<-arrange(files, mtime)

sis<-read_xlsx("D:/Documents/GitHub/Thesis/Data/Conflicto Armado/SISFUT.xlsx")

dptos<-sis$Departamento 
year <- sis$Año[!is.na(sis$Año)]
trim <- sis$Trimestre[!is.na(sis$Trimestre)]

length(year)*length(dptos)*length(trim) == dim(files)[1]

counter = 0
i = 0
j = 0
k = 0
for (dp in sis$Departamento){
  i = i + 1
  
  for (y in year) {
    
    j = j + 1
    
    for (t in trim) {
      #Counters
      k = k + 1
      counter = counter + 1
      #First trim year and dpto
      if(k == 1 & j == 1 & i == 1){
        
        d <- read_excel(paste0(path, "/", rownames(files)[counter]))
        d <- d %>% mutate(trim = rep(t, dim(d)[1]), year = rep(y, dim(d)[1]),
                          dpto = rep(dp, dim(d)[1]))
      }
      # Other trimesters
      else{
        
        d1 <- read_excel(paste0(path, "/", rownames(files)[counter]))
        d1 <- d1 %>% mutate(trim = rep(t, dim(d1)[1]), year = rep(y, dim(d1)[1]),
                            dpto = rep(dp, dim(d1)[1]))
        d <<- rbind(d, d1)
        
      }
    }
  }
}

d_distinct <- d %>% distinct(across(-c(`Código FUT`, trim)), .keep_all = T)
dim(d)
dim(d_distinct)

write.csv(d_distinct,"D:/Documents/GitHub/Thesis/Data/Final_data/SISFUT_Final/final_SISFUT_vict.csv")

path <- "D:/Documents/GitHub/Thesis/R_Code"
setwd(path)