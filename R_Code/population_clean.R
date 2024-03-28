#################################
# Population Data
# Data taken from https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion
###################################

pop <- read_xlsx("~/GitHub/Thesis/Data/population/population_2005_2030.xlsx",col_names = T)
pop <- pop %>% select(codmpio, ano, area, population) %>% 
  mutate(total = population*as.integer(area == "Total"), 
         rural = population*as.integer(area == "Centros Poblados y Rural Disperso"))

pop <- pop %>% group_by(ano, codmpio) %>% summarise(
  population = sum(total), 
  ind_rural = sum(rural)/sum(total)
)
