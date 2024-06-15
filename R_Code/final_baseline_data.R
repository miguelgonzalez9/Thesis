
# Select relevant baseline characteristics: 1) Time invariant 2) socioeconomic at 2005. 3) Agriculture average between 2011 -2015. 

# Municipality characteristics -----------------------------------
mun_char <- read_dta("D:/Documents/GitHub/Thesis/Data/CEDE/PANEL_CARACTERISTICAS_GENERALES(2022).dta") %>% 
  select(c(coddepto, codprovincia, codmpio, depto, provincia, municipio, ano, ao_crea, altura,
           areaoficialkm2, discapital, dismdo, disbogota, IPM, nbi, pobreza, gini, pib_percapita,
           gcaribe, gpacifica, gorinoquia, gamazonia)) %>% 
  filter(ano == 2005)

mun_agr <- read_dta("D:/Documents/GitHub/Thesis/Data/CEDE/PANEL_AGRICULTURA_Y_TIERRA(2021).dta") %>% 
  select(c(codmpio, ano, nuf_peq_productor, vrf_peq_productor)) %>% filter(ano <= 2015) %>% 
  group_by(codmpio) %>% summarise(across(-ano, ~ mean(.x, na.rm = T)))

# Get total demobilized per municipality.
mun_vio <- read_dta("D:/Documents/GitHub/Thesis/Data/CEDE/PANEL_CONFLICTO_Y_VIOLENCIA(2021).dta") %>% 
  select(c(Violencia_48_a_53, ano, codmpio, conflicto, poblind_1535_1540, d_refuerzo, desmov_AUC,
           desmov_FARC, desmovilizados)) %>% filter(ano <= 2015) %>% 
  group_by(codmpio,Violencia_48_a_53, conflicto, poblind_1535_1540) %>% 
  summarise(across(c(desmov_AUC:desmovilizados), ~ sum(.x, na.rm = T))) %>% ungroup()

# Outcome variables between 2013-2020
mun_vio_out <- read_dta("D:/Documents/GitHub/Thesis/Data/CEDE/PANEL_CONFLICTO_Y_VIOLENCIA(2021).dta") %>% 
  select(c( ano, codmpio, o_amenazas, o_homic, o_desplaza, e_amenaza, e_homic, e_desplaza)) %>% 
  filter(ano %in% c(start_time:end_time))

# Join, municipality characteristics before treatment occurs. Municipality-level variables. 

mun_char <- mun_char %>% left_join_rep(mun_agr, by = c("codmpio")) %>% 
  left_join_rep(mun_vio, by = c("codmpio")) %>% select(-ano)

mun_char <- mun_char %>% select(-c(pobreza, gini, pib_percapita)) %>% 
  na.omit()

# Add left and right vote share presidential variables in 1994.
pres_94 <- read_dta("D:/Documents/GitHub/Thesis/Data/Elections/1994_Presidencia.dta") %>% 
  left_join(pol_ideol, by = ("codigo_partido")) %>% 
  mutate(left_votes = as.integer(ideologia == 1)*votos, 
         right_votes = as.integer(ideologia == 2)*votos, 
         trad_votes = tradicional*votos)

pres_94 <- pres_94 %>% group_by(codmpio) %>% summarise(left_share_94 = sum(left_votes, na.rm = T)/sum(votos), 
                                                       right_share_94 = sum(right_votes, na.rm = T)/sum(votos), 
                                                       trad_share_94 = sum(trad_votes, na.rm = T)/sum(votos))
pres_94$codmpio <- as.character(pres_94$codmpio)
pres_94 <- pres_94 %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
  ))

mun_char$codmpio <- as.character(mun_char$codmpio)
mun_char <- mun_char %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
  ))

mun_char <- mun_char %>% left_join_rep(pres_94, by = "codmpio")

control_vars <- colnames(mun_char)[-c(1:6)]

# Construct armed-conflict panel. Use SIVel pol_vio_RD, leaders data, SIEVAC and VIPAA. --------------------
# Issue, CEDE data until 2021 --> No match with 2022-2023 violence data. Issue with population.
# Get population from DANE.


## SIVel -----------------------------------
years <- c(start_time:end_time)
# Expand years X Codmpio. 
# All muns from mun_char are in m_shape.
#unique(m_shape$codmpio) %in% unique(mun_char$codmpio) %>% table()
#mun_char$codmpio %in% m_shape$codmpio %>% table()
#lideres_RD$codmpio %>% unique() %in% m_shape$codmpio %>% table()
#lideres_RD$codmpio %>% unique() %in% mun_char$codmpio %>% table()

m_shape <- read_sf("D:/Documents/GitHub/Thesis/Data/Shapes/mun_shape/mpio.shp") %>% rename(codmpio = "MPIOS") %>% codmpio_clean()
conflict_panel <- expand.grid(years, m_shape$codmpio %>% unique())
colnames(conflict_panel) <- c("year", "codmpio")
conflict_panel <- conflict_panel %>% left_join_rep(mun_char, by = c("codmpio"))



# Merge
conflict_vars <- colnames(pol_vio_RD)[-c(1,2)]
pol_vio_RD$year <- as.integer(pol_vio_RD$year)
conflict_panel <- conflict_panel %>% left_join_rep(pol_vio_RD, by = c("year", "codmpio")) #7038 non-match master 

## Leaders somos defensores, INDEPAZ ------------------------------ 
lideres_RD$year <- as.numeric(lideres_RD$year)
conflict_panel <- conflict_panel %>% left_join_rep(lideres_RD %>% filter(year %in% years), #8112  non-match master
                                                   by = c("year", "codmpio"))
conflict_vars <- c(conflict_vars, colnames(lideres_RD)[-c(1,2)])

## SIEVCAC data on belic actions.and massacers -------------------------------
SIEVAC <- read.csv("D:/Documents/GitHub/Thesis/Data/Final_data/SIEVAC_RD.csv") %>% select(-X) %>% 
  filter(!(codmpio %in% c("EXVEN", "EXPER", "EXECU", "EXPAN")))

conflict_panel <- conflict_panel %>% left_join_rep(SIEVAC, by = c("codmpio", "year")) #11829  non-matched
conflict_vars <- c(conflict_vars, colnames(SIEVAC)[-c(1,2)])

MA <- read.csv("D:/Documents/GitHub/Thesis/Data/Final_data/SIEVAC_MA_RD.csv") %>% select(-X) %>% 
  codmpio_clean() %>% filter(year %in% start_time:end_time)

conflict_panel <- conflict_panel %>% left_join_rep(MA, by = c("codmpio", "year")) #13306   non-matched
conflict_vars <- c(conflict_vars, colnames(MA)[-c(1,2)])

#SIEVAC[!(SIEVAC$codmpio %in% conflict_panel$codmpio),] #departments data not matched.

## MOE 
path_moe <- "D:/Documents/GitHub/Thesis/Data/MOE/"
lid_MOE <- read_xlsx(paste0(path_moe, "MOE_lideres.xlsx"))
lid_MOE <- lid_MOE %>%
  mutate(partido = partido %>% tolower() %>% stri_trans_general(id = "Latin-ASCII"))

MOE_ideol <- read_xlsx(paste0(path_moe, "MOE_ideologia.xlsx"))

lid_MOE <- lid_MOE %>% 
  left_join_rep(MOE_ideol %>% distinct(),
                "partido")
lid_MOE <- lid_MOE %>% mutate(ideologia = case_when(
  str_detect(partido, "gsc") ~ 1, 
  is.na(ideologia) ~ 4, 
  T ~ ideologia
))

colnames(lid_MOE) <- c("codmpio", "code_rnec", "dpto", "mun", "date",
                       "gender", "victim", "sector", "party", "minority",
                       "type", "rurality", "PDET", "year", "month", "ideol")

# 5-digit codmpio
lid_MOE$codmpio <- as.character(lid_MOE$codmpio)
lid_MOE <- lid_MOE %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
  ))
# Change category names
lid_MOE <- lid_MOE %>% mutate(
  sector = case_when(
    sector == "Política" ~ "politic", 
    sector == "Comunal" ~ "comunal", 
    sector == "Social" ~ "social",
    T ~ sector
  ), 
  type = case_when(
    type == "Asesinato" ~ "kill", 
    type == "Amenaza" ~ "threat", 
    type == "Desaparición" ~ "disappearance", 
    type == "Atentado" ~ "kill_fail", 
    type == "Secuestro" ~ "kidnap",
    type == "VCMP" ~ "gen_vio"
  )
)

# Dummies. Change if more variables are needed.
lid_MOE <- lid_MOE %>% 
  dummy_columns(select_columns = c("sector", "type", "ideol"),remove_selected_columns = T) %>% 
  mutate(moe_vio = 1, 
         moe_kill = type_kill,
         moe_kill_fail = type_kill_fail,
         moe_threat = type_threat,
         moe_politic = sector_comunal,
         moe_comunal = sector_politic,
         moe_social = sector_social, 
         moe_pol_kill = type_kill*sector_politic,
         moe_soc_kill = type_kill*sector_social,
         moe_com_kill = type_kill*sector_comunal,
         
         moe_pol_threat =  type_threat*sector_politic, 
         moe_soc_threat =type_threat*sector_social, 
         moe_com_threat = type_threat*sector_comunal,
         
         moe_left_kill = type_kill*ideol_1,
         moe_center_kill = type_kill*ideol_3,
         moe_right_kill = type_kill*ideol_2,
         moe_other_kill = type_kill*ideol_4,
         
         moe_left_threat = type_threat*ideol_1,
         moe_center_threat = type_threat*ideol_3,
         moe_right_threat = type_threat*ideol_2,
         oe_other_threat = type_threat*ideol_4,
         
         moe_left_kill_fail = type_kill_fail*ideol_1,
         moe_center_kill_fail = type_kill_fail*ideol_3,
         moe_right_kill_fail = type_kill_fail*ideol_2,
         moe_other_kill_fail = type_kill_fail*ideol_4,
         
         
         moe_left_vio = ideol_1, 
         moe_nright_threat = as.integer(type_threat*ideol_1 == 1 | 
                                      type_threat*ideol_3 == 1| 
                                      type_threat*ideol_4 == 1), 
         moe_nright_kill = as.integer(type_kill*ideol_1 == 1 | 
                                    type_kill*ideol_3 == 1| 
                                    type_kill*ideol_4 == 1),
         moe_nright_vio = as.integer(ideol_1 == 1 | 
                                   ideol_3 == 1| 
                                   ideol_4 == 1),
         moe_nright_kill_fail = as.integer(type_kill_fail*ideol_1 == 1 | 
                                            type_kill_fail*ideol_3 == 1| 
                                            type_kill_fail*ideol_4 == 1), 
         moe_right_kill_fail = type_kill_fail*ideol_2 == 1,
         
         )

#colSums(lid_MOE %>% select(where(is.numeric)))

sum_cols <- colnames(lid_MOE)[str_detect(colnames(lid_MOE), "moe")]
lid_MOE <- lid_MOE %>% summarize_data_count(sum_cols, sum_cols) %>% 
  filter(year != 2024)

#colSums(lid_MOE %>% select(-c(1:2))) #3944 

conflict_vars <- c(conflict_vars, sum_cols)
### Add to conflict panel. 5 muns not in panel
conflict_panel <- conflict_panel %>% left_join_rep(lid_MOE, c("codmpio", "year"))  
#dim(conflict_panel)
## Add armed groups precense variables ViPAA 1998 2019. 
path_moe <- "D:/Documents/GitHub/Thesis/Data/Conflicto Armado/VIPAA_1998_2019/"
vipa <- read_dta(paste0(path_moe, "VIPAA_v1.3.dta"))
vipa <- vipa %>% rename(codmpio = "mun") %>% 
  dummy_columns(select_columns = "actor_main", remove_selected_columns = T)

vipa <- vipa %>% filter(year <= 2023 & year >= 2016) %>% group_by(codmpio) %>% summarise(
  across(starts_with("actor_main"), ~ sum(.x, na.rm = T))
)
# At least one armed act since 2016 after peace treaty. 
vipa <- vipa %>% mutate(
  across(starts_with("actor_main"), ~ as.integer(.x > 0), .names = "{.col}_pres")
)
colnames(vipa) <- c("codmpio", "gov_acts",
                    "insurg_acts", "param_acts","crimorg_acts","other_acts", 
                    "gov_pres","insurg_pres", "param_pres",
                    "crimorg_pres", "other_pres")
vipa <- vipa %>% codmpio_clean()
conflict_panel <- conflict_panel %>% left_join_rep(vipa, by = c("codmpio"))
## Replace NAs for zeros in conflict vars. 
conflict_panel <- conflict_panel %>% mutate(across(all_of(c(conflict_vars, colnames(vipa)[-1])), ~ ifelse(is.na(.x), 0, .x)))
summary(conflict_panel)

## Population ----------------------
colnames(pop) <- c("year", "codmpio", "population", "ind_rural")
conflict_panel <- conflict_panel %>% left_join_rep(pop %>% filter(year %in% years), by = c("codmpio", "year"))


### Normalize conflict variables by 100000 persons.
conflict_panel <- conflict_panel %>% 
  mutate(population = case_when(population == 0 ~ NA, T ~ population)) %>% 
  mutate(across(all_of(conflict_vars), 
                ~ 100000*.x/population, 
                .names = "pop_{col}"))

## Lags and leads of normalized and not-normalized conflict vars  -------------
conflict_panel <- conflict_panel %>% group_by(codmpio) %>% 
  mutate(across(c(paste0("pop_",conflict_vars),conflict_vars), list(
    lag1 = ~lag(.x, n = 1, order_by = year), 
    lag2 = ~lag(.x, n = 2, order_by = year), 
    lag3 = ~lag(.x, n = 3, order_by = year), 
    lag4 = ~lag(.x, n = 4, order_by = year), 
    lead1 = ~lead(.x, n = 1, order_by = year),
    lead2 = ~lead(.x, n = 2, order_by = year),
    lead3 = ~lead(.x, n = 3, order_by = year),
    lead4 = ~lead(.x, n = 4, order_by = year)),
    .names = "{.fn}_{.col}"))

for (i in conflict_vars) {
  conflict_panel[paste0("lead12_",i)] <- conflict_panel[paste0("lead1_",i)] + conflict_panel[paste0("lead2_",i)]
  conflict_panel[paste0("lead123_",i)] <- conflict_panel[paste0("lead1_",i)] + conflict_panel[paste0("lead2_",i)] + conflict_panel[paste0("lead3_",i)]
  conflict_panel[paste0("lead1234_",i)] <- conflict_panel[paste0("lead1_",i)] + conflict_panel[paste0("lead2_",i)] + conflict_panel[paste0("lead3_",i)] + conflict_panel[paste0("lead4_",i)]
  conflict_panel[paste0("lead23_",i)] <- conflict_panel[paste0("lead2_",i)] + conflict_panel[paste0("lead3_",i)]
  conflict_panel[paste0("lead234_",i)] <- conflict_panel[paste0("lead2_",i)] + conflict_panel[paste0("lead3_",i)] +conflict_panel[paste0("lead4_",i)]
  
  
  
  conflict_panel[paste0("lag12_",i)] <- conflict_panel[paste0("lag1_",i)] + conflict_panel[paste0("lag2_",i)]
  conflict_panel[paste0("lag123_",i)] <- conflict_panel[paste0("lag1_",i)] + conflict_panel[paste0("lag2_",i)] + conflict_panel[paste0("lag3_",i)]
}

# Normalize using population at t. 
conflict_panel <- conflict_panel %>% ungroup() %>% 
  mutate(population = case_when(population == 0 ~ NA, T ~ population)) %>% 
  mutate(across(starts_with("lead12")| starts_with("lead123") | starts_with("lead1234")|
                  starts_with("lag12")| starts_with("lag123") |starts_with("lead23") |
                  starts_with("lead234"), 
                ~ 100000*.x/population, 
                .names = "pop_{col}"))


# Add PDET municipalities. 
pdet <- read_xlsx("D:/Documents/GitHub/Thesis/Data/other/pdet_mpios.xlsx")
pdet <- pdet %>% rename(codmpio = "Código DANE Municipio") %>% mutate(
  pdet = 1
) %>% codmpio_clean()
pdet <- pdet %>% select(codmpio, pdet) %>% distinct()
conflict_panel <- conflict_panel %>% left_join_rep(pdet, by = "codmpio") %>% 
  mutate(pdet = case_when(
    is.na(pdet) ~ 0,
    T ~ pdet
  ))

write.csv(conflict_panel, "D:/Documents/GitHub/Thesis/Data/Final_data/conflict_panel.csv")
### Filter municipalities average pop < 300.000
conflict_panel <- conflict_panel %>% group_by(codmpio) %>% filter(mean(population) <= 300000) %>% ungroup()
#length(conflict_panel$codmpio %>% unique()) #1095

# Spending data-------------------------------------------
# Data from 2015-2020. Only needed for RD analysis. 
# Mun code
spend_data <- spend_data_baseline_V %>% rename(codmpio = "cod_mun")
spend_data$codmpio <- as.character(spend_data$codmpio)

spend_vars <- colnames(spend_data)[-c(1,2)]

spend_data <- spend_data %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
  ))

spend_data <- spend_data %>%  mutate(
  # First year of government
  V_payment_diff1 = V_ln_payments_lead1 - V_ln_payments,
  lag1_V_payment_diff1 = V_ln_payments - V_ln_payments_lag1,
  lag2_V_payment_diff1 = V_ln_payments_lag1 - V_ln_payments_lag2,
  # Second year of government
  V_payment_diff2 = V_ln_payments_lead2 - V_ln_payments,
  
  # First year of government
  V1_payment_diff1 = V1_ln_payments_lead1 - V1_ln_payments,
  lag1_V1_payment_diff1 = V1_ln_payments - V1_ln_payments_lag1,
  lag2_V1_payment_diff1 = V1_ln_payments_lag1 - V1_ln_payments_lag2,
  # Second year of government
  V1_payment_diff2 = V1_ln_payments_lead2 - V1_ln_payments,
) 



# Elections --> RDD -----------------------------

RD_baseline <- conflict_panel %>% filter(year %in% c(2015,2019)) %>% 
  left_join_rep(elect_left, by = c("codmpio", "year"))
RD_baseline <- RD_baseline %>% left_join_rep(elect_right, by = c("codmpio", "year"))

RD_baseline <- RD_baseline %>% left_join_rep(spend_data,  by = c("codmpio", "year"))
#View(RD_baseline %>% filter(is.na(coddepto)) %>% distinct())


# Incumency variables
RD_baseline <- RD_baseline %>% mutate(l_ideol_incum_l = case_when(
  ideol_incum_l != 1 | is.na(ideol_incum_l) ~ 0, 
  ideol_incum_l == 1 ~ 1
), 
r_ideol_incum_l = case_when(
  ideol_incum_l != 2 | is.na(ideol_incum_l) ~ 0, 
  ideol_incum_l == 2 ~ 1
),
l_ideol_incum_r = case_when(
  ideol_incum_r != 1 | is.na(ideol_incum_r) ~ 0, 
  ideol_incum_r == 1 ~ 1),
r_ideol_incum_r = case_when(
  ideol_incum_r != 2 | is.na(ideol_incum_r) ~ 0, 
  ideol_incum_r == 2 ~ 1
), 
nr_ideol_incum_r = case_when(
  ideol_incum_r == 2 | is.na(ideol_incum_r) ~ 0, 
  ideol_incum_r != 2 ~ 1
)
)

write.csv(RD_baseline, "D:/Documents/GitHub/Thesis/Data/Final_data/RD_data.csv")
saveRDS(list(conflict_vars,spend_vars,control_vars), file="D:/Documents/GitHub/Thesis/Data/Final_data/vnames.RData")

# DiD baseline data -----------------------------------------------

temp <- elect_right %>% filter(year == 2019)
did_baseline <- conflict_panel %>% 
  left_join_rep(temp %>% select(-year), by = c("codmpio"))


## Subset 1. -------------------
# filter only municipalities with races where right wing first or second and not both. 

did_baseline <- did_baseline %>% mutate(Tr = as.integer(share_diff2_r > 0), 
                                        time = year - 2020, 
                                        post = as.integer(time >= 0), 
                                        d = Tr*post) 
# 2020 is base time.

did_baseline <- did_baseline %>% filter(time %in% c(-4:3))
did_baseline <- did_baseline %>% select(!contains("lag") & !contains("lead")) %>% 
  mutate(across(contains("pop"), ~ as.integer(.x > 0), .names = "{.col}_dum"))

# Monthly leaders DiD. 
sum_cols <- colnames(lideres)[str_detect(colnames(lideres), "(?<!social)_sector$|_respon$|lid_assas")]
m_lid <- summarize_data_count_2(lideres, sum_cols, sum_cols) %>% 
  mutate(across(!c(year:day), ~ ifelse(is.na(.x), 0 , .x)))
m_lid <- m_lid %>% mutate(date = ymd(paste0(year,"-", month, "-", day))) %>% 
  filter(!is.na(date)) %>% select(-c(day,month,year))


sum_cols_sd <- colnames(lideres_sd)[-c(1:17, 24)]
m_lid_sd <- summarize_data_count_2(lideres_sd,sum_cols_sd,paste0(sum_cols_sd, "_sd"))
m_lid_sd <- m_lid_sd %>% mutate(across(!c(year:day), ~ ifelse(is.na(.x), 0 , .x)))
m_lid_sd <- m_lid_sd %>% mutate(date = ymd(paste0(year,"-", month, "-", day))) %>% 
  filter(!is.na(date)) %>% select(-c(day,month,year))


did_baseline_month <- expand.grid(years, unique(m_shape$codmpio), c(1:12), 
                                  c(1:31))
colnames(did_baseline_month) <- c("year", "codmpio", "month", "day")
did_baseline_month <- did_baseline_month %>% 
  mutate(date = ymd(paste0(year,"-", month, "-", day))) %>% 
  filter(!is.na(date))

did_baseline_month <- did_baseline_month  %>% 
  left_join_rep(m_lid_sd, by = c("codmpio", "date")) %>% 
  left_join_rep(m_lid,  by = c("codmpio", "date"))

did_baseline_month <- did_baseline_month %>% select(-day)%>%  group_by(year, codmpio, month) %>% 
  summarise(across(!date, ~ sum(.x, na.rm = T)))
  
did_baseline_month <- did_baseline_month %>% mutate(date = ym(paste0(year, "-", month))) %>% 
  left_join_rep(temp %>% select(-year), by = c("codmpio"))

did_baseline_month <-did_baseline_month %>% ungroup() %>% 
  mutate(across(!c(codmpio, year, month, date), ~ as.integer(.x > 0), .names = "{.col}_dum")) %>% 
  mutate(Tr = as.integer(share_diff2_r > 0), 
         time = date - ym("2019-10"), 
         post = as.integer(time >= 0), 
         d = Tr*post)

## Subset 2. -------------------
# Define treatment as right-wing elected.
elec_did <- elec_did 
#unique(elec_2019 %>% codmpio_clean() %>% pull(codmpio)) %in% unique(conflict_panel$codmpio) %>% table()
did_baseline_2 <- conflict_panel %>% select(!contains("lead"))%>% select(!contains("lag"))
did_baseline_2 <- did_baseline_2 %>% cbind(conflict_panel %>% select(contains("lead1234") | contains("lag123")))
did_baseline_2 <- did_baseline_2 %>% left_join_rep(elec_did, by = c("codmpio"))


did_baseline_2 <- did_baseline_2 %>% mutate(time = year - 2020, 
                                        post = as.integer(time >= 0), 
                                        d_r = treat_r*post, 
                                        d_l = treat_l*post, 
                                        d_c = treat_c*post, 
                                        d_t = treat_trad*post)  %>% 
  mutate(across(contains("pop"), ~ as.integer(.x > 0), .names = "{.col}_dum"))

did_baseline_2 <- did_baseline_2 %>% filter(time %in% c(-4:3))

# Two periods DiD. 
did_baseline_2periods <- did_baseline_2 %>% 
  select(c("codmpio", "year", contains("pop_lead1234"), contains("pop_lag123"))) %>%
  filter(year == 2019)

did_baseline_2periods <- did_baseline_2periods %>% 
  pivot_longer(
    cols = -c(codmpio, year), 
    names_to = c(".value", "time"), 
    names_pattern ="(pop_lag\\d+|pop_lead\\d+)_(.*)"
    ) 
did_baseline_2periods <- did_baseline_2periods %>% 
  pivot_longer(
    cols = c("pop_lead1234", "pop_lag123"), 
    names_to = "time_1"
  )
did_baseline_2periods <- did_baseline_2periods %>% 
  pivot_wider(
    names_from = time,
    values_from = value
  )

did_baseline_2periods <- did_baseline_2periods %>% 
  rename(post = "time_1") %>% mutate(post = case_when(
    post == "pop_lead1234" ~ 1, 
    post == "pop_lag123" ~ 0,
    T ~ NA
  ))

did_baseline_2periods <- did_baseline_2periods %>% 
  left_join_rep(elec_did, by = c("codmpio"))
    
