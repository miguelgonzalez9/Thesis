### Elections data 2015 and 2019. 

elec_2015 <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/Alcaldia/2015_alcaldia.dta") %>% 
  filter(!(primer_apellido %in% c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS")))
elec_2019 <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/Alcaldia/2019_alcaldia.dta")  %>% 
  filter(!(primer_apellido %in% c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS")))
elec_2011 <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/Alcaldia/2011_alcaldia.dta")  %>% 
  filter(!(primer_apellido %in% c("VOTOS EN BLANCO", "VOTOS NULOS", "TARJETAS NO MARCADAS")))


pol_ideol <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/CEDE/Microdatos/clasificacion_partidos_v1.dta")
# Save pol_ideol as excel
write_xlsx(pol_ideol, "D:/Documents/GitHub/Thesis/Data/Elections/CEDE/Microdatos/clasificacion_partidos_v1.xlsx")

elec_2011 <- elec_2011 %>% left_join(pol_ideol, by = c("codigo_partido"))
elec_2015 <- elec_2015 %>% left_join(pol_ideol, by = c("codigo_partido"))
elec_2019 <- elec_2019 %>% left_join(pol_ideol, by = c("codigo_partido"))

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

ggplot() + geom_density(data = elect_right, aes(x = share_diff2_r), color = "red") + 
  geom_density(data = elect_left, aes(x = share_diff1_l), color = "blue")

