
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
# Expand years X Codmpio
conflict_panel <- expand.grid(years, mun_char$codmpio)
colnames(conflict_panel) <- c("year", "codmpio")
conflict_panel <- conflict_panel %>% left_join_rep(mun_char, by = c("codmpio"))

# Merge
conflict_vars <- colnames(pol_vio_RD)[-c(1,2)]
pol_vio_RD$year <- as.integer(pol_vio_RD$year)
conflict_panel <- conflict_panel %>% left_join_rep(pol_vio_RD, by = c("year", "codmpio")) #6808 non-match master 

## Leaders somos defensores, INDEPAZ ------------------------------ 
lideres_RD$year <- as.numeric(lideres_RD$year)
conflict_panel <- conflict_panel %>% left_join_rep(lideres_RD %>% filter(year %in% years), #7872 non-match master
                                                   by = c("year", "codmpio"))
conflict_vars <- c(conflict_vars, colnames(lideres_RD)[-c(1,2)])

## SIEVCAC data on belic actions. -------------------------------
SIEVAC <- read.csv("D:/Documents/GitHub/Thesis/Data/Final_data/SIEVAC_RD.csv")
conflict_panel <- conflict_panel %>% left_join_rep(SIEVAC, by = c("codmpio", "year")) #11579 non-matched
conflict_vars <- c(conflict_vars, colnames(SIEVAC)[-c(1,2,3)])
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
  mutate(poli_vio = 1, 
         pol_kill = type_kill*sector_politic, 
         left_kill = type_kill*ideol_1,
         left_threat = type_threat*ideol_1)


sum_cols <- c("pol_kill", "left_kill", "left_threat", 
              "poli_vio")
lid_MOE <- lid_MOE %>% summarize_data_count(sum_cols, sum_cols) %>% 
  filter(year != 2024)

conflict_vars <- c(conflict_vars, sum_cols)
### Add to conflict panel. 5 muns not in panel
conflict_panel <- conflict_panel %>% left_join_rep(lid_MOE, c("codmpio", "year"))


## Replace NAs for zeros in conflict vars. 
conflict_panel <- conflict_panel %>% mutate(across(all_of(conflict_vars), ~ ifelse(is.na(.x), 0, .x)))
summary(conflict_panel)

## Population ----------------------
colnames(pop) <- c("year", "codmpio", "population", "ind_rural")
conflict_panel <- conflict_panel %>% left_join_rep(pop %>% filter(year %in% years), by = c("codmpio", "year"))


### Normalize conflict variables by 1000 persons.
conflict_panel <- conflict_panel %>% 
  mutate(population = case_when(population == 0 ~ NA, T ~ population)) %>% 
  mutate(across(all_of(conflict_vars), 
                ~ 100000*.x/population, 
                .names = "pop_{col}"))

## Lags and leads of normalized and not-normalized conflict vars  -------------
conflict_panel <- conflict_panel %>% 
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
  
  conflict_panel[paste0("lag12_",i)] <- conflict_panel[paste0("lag1_",i)] + conflict_panel[paste0("lag2_",i)]
  conflict_panel[paste0("lag123_",i)] <- conflict_panel[paste0("lag1_",i)] + conflict_panel[paste0("lag2_",i)] + conflict_panel[paste0("lag3_",i)]
}

# Normalize using population at t. 
conflict_panel <- conflict_panel %>% 
  mutate(population = case_when(population == 0 ~ NA, T ~ population)) %>% 
  mutate(across(starts_with("lead12")| starts_with("lead123") | starts_with("lead1234")|
                  starts_with("lag12")| starts_with("lag123"), 
                ~ 100000*.x/population, 
                .names = "pop_{col}"))


### Filter municipalities average pop < 300.000
conflict_panel <- conflict_panel %>% group_by(codmpio) %>% filter(mean(population) <= 300000) %>% ungroup()
#dim(conflict_panel) 12924  
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



write.csv(RD_baseline, "D:/Documents/GitHub/Thesis/Data/Final_data/RD_data.csv")
saveRDS(list(conflict_vars,spend_vars,control_vars), file="D:/Documents/GitHub/Thesis/Data/Final_data/vnames.RData")
