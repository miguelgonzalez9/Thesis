#####################
# Elections cleaning 
# manual
#########################

# Manual () ----------------------
# Filter first two candidates
elec <- elec %>% group_by(codmpio, ano) %>% 
  mutate(vote_rank = rank(-votos, ties.method = "first")) %>% 
  filter(vote_rank <= 2.5) 
# Filter elections with only one candidate for specific ideol.

elec <- elec %>% group_by(codmpio, ano) %>% 
  filter(length(which(ideologia == 2)) == 1)

mayor <- elec %>% filter(curules == 1) %>% 
  select(codmpio, tradicional, ideologia, ano) %>% 
  rename(
    tradicional_incum = "tradicional",
    ideol_incum = "ideologia") %>% 
  # Select only 2011 and 2015
  filter(ano %in% (c(2015,2019) -4)) %>% 
  # Set next election year to merge. 
  mutate(ano = ano + 4)

# Join and clean
elec <- elec %>% left_join(mayor, by = c("codmpio", "ano")) %>% 
  filter(ano %in% c(2015,2019)) %>% 
  select(ano, coddpto, codmpio, circunscripcion, 
         votos, censoe_total, tradicional, temporalidad, tradicional,
         ideologia, paso_ideologia, vote_rank, ideol_incum, 
         tradicional_incum, part_camara, part_concejo, part_gobernacion, 
         part_senado)

# Create running variable. 
elec <- elec %>% group_by(ano, codmpio) %>% 
  mutate(total_votes = sum(votos)) %>% ungroup()%>% 
  mutate(vote_share = votos/total_votes, 
         right_ideol = case_when(ideologia == 2 ~ 1, 
                                 ideologia != 2 ~ 0, 
                                 is.na(ideologia) ~ 0))
## Running variable
elec <- elec  %>%  pivot_wider(
  id_cols = ano:circunscripcion,
  names_from = right_ideol, 
  values_from = c(votos, ideologia, paso_ideologia, vote_share, vote_rank, 
                  tradicional, ideol_incum, tradicional_incum, part_camara, 
                  part_concejo, part_gobernacion, part_senado)
)
print(head(elec %>% select(vote_share_1, vote_share_0)))
var_name <- paste0("share_diff", as.character(2))
elec <- elec %>% mutate(!!(var_name) := vote_share_1-vote_share_0)
df <- df %>% select(-c(tradicional_incum_0, ideol_incum_0)) %>% 
  rename(
    ideol_incum="ideol_incum_1", 
    tradicional_incum = "tradicional_incum_1")



# Filter only elections where right wing either first or second
elec_2019 <- elec_2019 %>% group_by(codmpio)  %>% 
  mutate(vote_rank = rank(-votos, ties.method = "first")) %>% 
  filter(vote_rank <= 2.5) %>% group_by(codmpio) %>% filter(length(which(ideologia == 2)) == 1) %>% 
  mutate(year = 2019)
mayor_2015 <- elec_2015 %>% filter(curules == 1) %>% select(codmpio, tradicional, ideologia)
mayor_2015 <- mayor_2015 %>% rename(
  tradicional_incum = "tradicional",
  ideol_incum = "ideologia"
)

elec_2019 <- elec_2019 %>% left_join(mayor_2015, by = c("codmpio"))


if (any(table(elec_2019$codmpio) != 2)){
  warning("Municipalities with more than two candidates in 2019")
}

# Calculate share of votes. 

elec_2015 <- elec_2015 %>% group_by(codmpio) %>% 
  mutate(vote_rank = rank(-votos, ties.method = "first")) %>% 
  filter(vote_rank <= 2.5) %>% group_by(codmpio) %>% filter(length(which(ideologia == 2)) == 1) %>% 
  mutate(year = 2015)

mayor_2011 <- elec_2011 %>% filter(curules == 1) %>% select(codmpio, tradicional, ideologia)
mayor_2011 <- mayor_2011 %>% rename(
  tradicional_incum = "tradicional",
  ideol_incum = "ideologia"
)

elec_2015 <- elec_2015 %>% left_join(mayor_2011, by = c("codmpio"))

if (any(table(elec_2015$codmpio) != 2)){
  warning("Municipalities with more than two candidates in 2015")
}

# Select relevant variables.
elections <- rbind(elec_2015, elec_2019)
elections <- elections %>% select(year, fecha_eleccion, coddpto, codmpio, circunscripcion, 
                                  votos, censoe_total, tradicional, temporalidad, 
                                  ideologia, paso_ideologia, vote_rank, ideol_incum, 
                                  tradicional_incum, part_camara, part_concejo, part_gobernacion, 
                                  part_senado)
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
  values_from = c(votos, temporalidad, ideologia, paso_ideologia, vote_share, vote_rank, 
                  tradicional, ideol_incum, tradicional_incum, part_camara, 
                  part_concejo, part_gobernacion, part_senado)
)
elections <- elections %>% mutate(X_it = vote_share_1-vote_share_0)
elections <- elections %>% select(-c(tradicional_incum_0, ideol_incum_0)) %>% 
  rename(
    ideol_incum="ideol_incum_1", 
    tradicional_incum = "tradicional_incum_1"
  )
