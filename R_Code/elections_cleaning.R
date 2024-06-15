### Elections data 2015 and 2019. 

elec_2015 <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/Alcaldia/2015_alcaldia.dta") %>% 
  filter(!(primer_apellido %in% c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS")))
elec_2019 <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/Alcaldia/2019_alcaldia.dta")  %>% 
  filter(!(primer_apellido %in% c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS")))
elec_2011 <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/Alcaldia/2011_alcaldia.dta")  %>% 
  filter(!(primer_apellido %in% c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS")))


pol_ideol <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/CEDE/Microdatos/clasificacion_partidos_v1.dta")
# Save pol_ideol as excel
#write_xlsx(pol_ideol, "D:/Documents/GitHub/Thesis/Data/Elections/CEDE/Microdatos/clasificacion_partidos_v1.xlsx")

elec_2011 <- elec_2011 %>% left_join(pol_ideol, by = c("codigo_partido"))
elec_2015 <- elec_2015 %>% left_join(pol_ideol, by = c("codigo_partido"))
elec_2019 <- elec_2019 %>% left_join_rep(pol_ideol, by = c("codigo_partido"))

# Only five condidates with no political party match. 
missing_parties <- elec_2011 %>% filter(is.na(ideologia)) %>% select(codigo_partido, primer_apellido, segundo_apellido ,nombres) %>% 
  rbind(elec_2015 %>% filter(is.na(ideologia)) %>% select(codigo_partido, primer_apellido, segundo_apellido ,nombres)) %>% 
  rbind(elec_2019 %>% filter(is.na(ideologia)) %>% select(codigo_partido, primer_apellido, segundo_apellido ,nombres) )

elec <- rbind(elec_2011, elec_2015) %>% rbind(elec_2019)

elec$codmpio <- as.character(elec$codmpio)
elec <- elec %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
  ))

# Clean elec
elec <- elec %>% filter(!is.na(ideologia))


elect_right <- election_share(elec, 2, c(2015, 2019))
elect_left <- election_share(elec, 1, c(2015, 2019)) 

elect_left <- elect_left %>% rename(year = "ano") %>% select(-coddpto)
elect_right <- elect_right %>% rename(year = "ano") %>% select(-coddpto)


colnames(elect_right)[-c(1:2)] <- paste0(colnames(elect_right)[-c(1:2)], "_r")
colnames(elect_left)[-c(1:2)] <- paste0(colnames(elect_left)[-c(1:2)], "_l")

# DiD elections 2019. ------------------
elec_did <- elec_2019  %>% filter(curules == 1)
elec_did <- elec_did %>% select(codmpio, ideologia, tradicional, part_camara,
                                  part_concejo) %>% 
  mutate(treat_r = as.integer(ideologia == 2), 
         treat_l = as.integer(ideologia == 1), 
         treat_c = as.integer(ideologia == 3),
         treat_trad = as.integer(tradicional == 3)) %>% codmpio_clean()

# Add incumbency. 
incum <- elec_2015 %>% filter(curules == 1) %>% select(codmpio, ideologia, tradicional) %>% 
  mutate(incum_r = as.integer(ideologia == 2), 
         incum_l = as.integer(ideologia == 1), 
         incum_c = as.integer(ideologia == 3), 
         incum_trad = tradicional
         ) %>% rename(ideol_incum = "ideologia", 
                      ideol_trad = "tradicional")
incum <- incum %>%  codmpio_clean()

elec_did <- elec_did %>% left_join_rep(incum, by = c("codmpio"))

# Number of parties in mayoral elections during 2019 and 2015. 
x <- pol_ideol %>% filter(coalicion == 0 & part_alcaldia == 1) %>% select(ideologia, codigo_partido, temporalidad, nombre)  %>% distinct()
x <- x %>% mutate(end = as.numeric(stri_extract_all(temporalidad, regex = "(?<=-).*")))
x <- x %>% filter(end == 2019 | end == 2015)
table(x$ideologia)
