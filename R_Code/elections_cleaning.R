### Elections data 2015 and 2019. 

elec_2015 <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/Alcaldia/2015_alcaldia.dta") 
elec_2019 <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/Alcaldia/2019_alcaldia.dta")
pol_ideol <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/CEDE/Microdatos/clasificacion_partidos_v1.dta")

elec_2015 <- elec_2015 %>% left_join(pol_ideol, by = c("codigo_partido"))
elec_2019 <- elec_2019 %>% left_join(pol_ideol, by = c("codigo_partido"))


# Filter only elections where right wing either first or second
elec_2015 <- elec_2015 %>% group_by(codmpio) %>% 
  mutate(vote_rank = rank(-votos, ties.method = "first")) %>% 
  filter(vote_rank <= 2.5) %>% group_by(codmpio) %>% filter(length(which(ideologia == 2)) == 1) %>% 
  mutate(year = 2015)

if (any(table(elec_2015$codmpio) != 2)){
  warning("Municipalities with more than two candidates in 2015")
}

elec_2019 <- elec_2019 %>% group_by(codmpio)  %>% 
  mutate(vote_rank = rank(-votos, ties.method = "first")) %>% 
  filter(vote_rank <= 2.5) %>% group_by(codmpio) %>% filter(length(which(ideologia == 2)) == 1) %>% 
  mutate(year = 2019)

if (any(table(elec_2019$codmpio) != 2)){
  warning("Municipalities with more than two candidates in 2019")
}

# Select relevant variables.
elections <- rbind(elec_2015, elec_2019)
elections <- elections %>% select(year, fecha_eleccion, coddpto, codmpio, circunscripcion, 
                                  votos, censoe_total, tradicional, temporalidad, 
                                  ideologia, paso_ideologia, vote_rank)
# Running variable
## Total votes nd vote share
elections <- elections %>% group_by(year, codmpio) %>% 
  mutate(total_votes = sum(votos)) %>% ungroup()  %>% 
  mutate(vote_share = votos/total_votes, 
         right_ideol = case_when(ideologia == 2 ~ 1, 
                                  ideologia != 2 ~ 0, 
                                  is.na(ideologia) ~ 0))
## Running variable
elections <- elections  %>%  pivot_wider(
  id_cols = year:circunscripcion,
  names_from = right_ideol, 
  values_from = c(votos, temporalidad, ideologia, paso_ideologia, vote_share, vote_rank)
  )
elections <- elections %>% mutate(X_it = vote_share_1-vote_share_0)

