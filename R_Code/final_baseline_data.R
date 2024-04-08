
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
pol_vio_RD$year <- as.numeric(pol_vio_RD$year) 
conflict_vars <- colnames(pol_vio_RD)[-c(1,2)]
conflict_panel <- conflict_panel %>% left_join_rep(pol_vio_RD, by = c("year", "codmpio"))

## Leaders somos defensores, INDEPAZ ------------------------------ 
lideres_RD$year <- as.numeric(lideres_RD$year)
conflict_panel <- conflict_panel %>% left_join_rep(lideres_RD %>% filter(year %in% years), 
                                                   by = c("year", "codmpio"))
conflict_vars <- c(conflict_vars, colnames(lideres_RD)[-c(1,2)])

## SIEVCAC data on belic actions. -------------------------------

conflict_panel <- conflict_panel %>% left_join_rep(SIEVAC, by = c("codmpio", "year"))
conflict_vars <- c(conflict_vars, colnames(SIEVAC)[-c(1,2)])
# SIEVAC[!(SIEVAC$codmpio %in% conflict_panel$codmpio),] departments data not matched.


## Replace NAs for zeros in conflict vars. 
conflict_panel <- conflict_panel %>% mutate(across(all_of(conflict_vars), ~ ifelse(is.na(.x), 0, .x)))
summary(conflict_panel)

## Population ----------------------
colnames(pop) <- c("year", "codmpio", "population", "ind_rural")
conflict_panel <- conflict_panel %>% left_join_rep(pop %>% filter(year %in% years), by = c("codmpio", "year"))


### Normalize conflict variables by 1000 persons.
conflict_panel <- conflict_panel %>% 
  mutate(population = case_when(population == 0 ~ NA, T ~ population)) %>% 
  mutate(across(conflict_vars, 
                ~ 100000*.x/population, 
                .names = "pop_{col}"))

## Lags and leads of normalized and not-normalized conflict vars  -------------
conflict_panel <- conflict_panel %>% 
  mutate(across(c(paste0("pop_",conflict_vars),conflict_vars), list(
    lag1 = ~lag(.x, n = 1, order_by = year), 
    lag2 = ~lag(.x, n = 2, order_by = year), 
    lead1 = ~lead(.x, n = 1, order_by = year),
    lead2 = ~lead(.x, n = 2, order_by = year),
    lead3 = ~lead(.x, n = 3, order_by = year),
    lead4 = ~lead(.x, n = 4, order_by = year)),
    .names = "{.fn}_{.col}"))

for (i in conflict_vars) {
  conflict_panel[paste0("lead12_",i)] <- conflict_panel[paste0("lead1_",i)] + conflict_panel[paste0("lead2_",i)]
  conflict_panel[paste0("lead123_",i)] <- conflict_panel[paste0("lead1_",i)] + conflict_panel[paste0("lead2_",i)] + conflict_panel[paste0("lead3_",i)]
  conflict_panel[paste0("lead1234_",i)] <- conflict_panel[paste0("lead1_",i)] + conflict_panel[paste0("lead2_",i)] + conflict_panel[paste0("lead3_",i)] + conflict_panel[paste0("lead4_",i)]
}
# Normalize using population at t. 
conflict_panel <- conflict_panel %>% 
  mutate(population = case_when(population == 0 ~ NA, T ~ population)) %>% 
  mutate(across(starts_with("lead12")| starts_with("lead123") | starts_with("lead1234"), 
                ~ 100000*.x/population, 
                .names = "pop_{col}"))


### Filter municipalities average pop < 300.000
conflict_panel <- conflict_panel %>% group_by(codmpio) %>% filter(mean(population) <= 300000) %>% ungroup()

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
  # Second year of government
  V_payment_diff2 = V_ln_payments_lead2 - V_ln_payments,
  
  # First year of government
  V1_payment_diff1 = V1_ln_payments_lead1 - V1_ln_payments,
  # Second year of government
  V1_payment_diff2 = V1_ln_payments_lead2 - V1_ln_payments
) 



# Elections --> RDD -----------------------------

RD_baseline <- conflict_panel %>% filter(year %in% c(2015,2019)) %>% 
  left_join_rep(elect_left, by = c("codmpio", "year"))
RD_baseline <- RD_baseline %>% left_join_rep(elect_right, by = c("codmpio", "year"))
RD_baseline <- RD_baseline %>% left_join_rep(spend_data,  by = c("codmpio", "year"))
# Conflict vars lead1 + lead2
RD_baseline <- RD_baseline %>% mutate(across())
conflict_vars


#write_dta(RD_baseline, "D:/Documents/GitHub/Thesis/Data/Final_data/RD_data.dta")

