#########################
# SIEVAC data
##########################
SIEVAC <- read_xlsx("D:/Documents/GitHub/Thesis/Data/Conflicto Armado/SIEVcAC_CNMH/CasosAB_202309.xlsx", col_names = T)
vict_AB <- read_xlsx("D:/Documents/GitHub/Thesis/Data/Conflicto Armado/SIEVcAC_CNMH/VictimasAB_202403.xlsx", col_names = T)
vict_AS <- read_xlsx("D:/Documents/GitHub/Thesis/Data/Conflicto Armado/SIEVcAC_CNMH/VictimasAS_202403.xlsx", col_names = T)
vict_AP <- read_xlsx("D:/Documents/GitHub/Thesis/Data/Conflicto Armado/SIEVcAC_CNMH/VictimasAP_202403.xlsx", col_names = T)


colnames(SIEVAC) <- c("id_case", "id_rel_case", "year", "month", "day", 
                       "codmpio","mpio", "dpto","region", "type", "initiative", 
                      "attacked_unit", "ag_1", "ag_1_desc", "ag_2","ag_2_desc", "ag_3",
                      "ag_3_desc", "civil_wounded", "captured_ag", "captured", "combat_wound",
                      "military", "police", "other_state", "state_no_info", "tot_cobat_state",
                      "guerrilla", "paramilitary", "disidencias", "combat_no_info", "other_ag",
                      "tot_combat_ag", "tot_combat", "no_info","tot_civil", "military_adv",
                      "tot_victims", "lat", "long")

SIEVAC <- SIEVAC %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
  ))

SIEVAC$year <- as.numeric(SIEVAC$year)
SIEVAC <- SIEVAC %>% select(year, month, day, codmpio, type, initiative, 
                            ag_1, ag_2, ag_3, military_adv, tot_cobat_state, tot_combat_ag,
                            tot_combat, tot_civil, tot_victims)
SIEVAC$ag_1 %>% unique()
SIEVAC$ag_2 %>% unique()
SIEVAC$ag_3 %>% unique()

SIEVAC <- SIEVAC %>% filter(year %in% c(start_time:end_time)) 
SIEVAC <- SIEVAC %>% mutate(ag = paste0(ag_1, ";", ag_2,";", ag_3),
                            param_respon_ab = as.integer(str_detect(ag, "PARAMILITAR")), 
                            insurg_respon_ab = as.integer(str_detect(ag, "POSDESMOVILIZACIÓN|GUERRILLA")), 
                            state_respon_ab = as.integer(str_detect(ag, "AGENTE DEL ESTADO")), 
                            bacrim_respon_ab = as.integer(str_detect(ag, "BANDOLERISMO|CRIMEN ORGANIZADO")), 
                            respon_unknown_ab = as.integer(str_detect(ag, "GRUPO ARMADO NO IDENTIFICADO|GRUPO ARMADO NO DIRIMIDO")), 
                            adv_state_ab =  as.integer(str_detect(military_adv, "FUERZAS ARMADAS ESTATALES")), 
                            adv_ga_ab = as.integer(str_detect(military_adv, "GRUPOS ARMADOS ORGANIZADOS")), 
                            belic_action = 1
                            )
SIEVAC <- SIEVAC %>% select(c(tot_cobat_state:tot_victims) | c(param_respon_ab:belic_action) | c(codmpio,year)) %>%  
  group_by(codmpio, year) %>% 
  summarise(across(everything(), ~ sum(.x, na.rm = T))) 
SIEVAC <- SIEVAC %>% filter(codmpio != "00000") %>% select(-param_respon_ab)
write.csv(SIEVAC,  "D:/Documents/GitHub/Thesis/Data/Final_data/SIEVAC_RD.csv")

# Victims masacres. 
vict_MA <- read_xlsx("D:/Documents/GitHub/Thesis/Data/Conflicto Armado/SIEVcAC_CNMH/VictimasMA_202309.xlsx", col_names = T)
vict_MA <- vict_MA %>% filter(`Calidad de la Víctima o la Baja` != "COMBATIENTE")
vict_MA <- vict_MA %>% select(-c(`Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`, 
                                 `Descripción Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
                                 `Calidad de la Víctima o la Baja`))
colnames(vict_MA) <- c("id_case","codmpio", "mpio", "dpto", "year", "month", "day",
                       "id_person", "sex", "ethinicity", "ocupation", "type_victim",
                       "pol_party", "age", "lat", "long")
vict_MA <- vict_MA %>% group_by(codmpio, year) %>% select(codmpio, year) %>% 
  summarise(mas_kill = n()) %>% 
  filter(year != "0000" & !(codmpio %in% c("00000", "EXPAN", "EXVEN")))
write.csv(vict_MA,  "D:/Documents/GitHub/Thesis/Data/Final_data/SIEVAC_MA_RD.csv")
