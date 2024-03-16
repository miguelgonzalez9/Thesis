mun_char <- read_dta("D:/Documents/GitHub/Thesis/Data/CEDE/PANEL_CARACTERISTICAS_GENERALES(2022).dta") %>% 
  select(c(coddepto, codprovincia, codmpio, depto, provincia, municipio, ano, ao_crea, indrural, altura, minorias, 
           parques, religioso, estado, otras) | starts_with("IPM") | starts_with("ipm") |
           starts_with("g") | starts_with("dis") |  starts_with("pobreza") | 
           starts_with("retro") | starts_with("pobl") | starts_with("area"))
mun_agr <- read_dta("D:/Documents/GitHub/Thesis/Data/CEDE/PANEL_AGRICULTURA_Y_TIERRA(2021).dta") %>% 
  select(starts_with("vrf") | c(codmpio, ano) | starts_with("nuf"))
# 
mun_conf_vio <- read_dta("D:/Documents/GitHub/Thesis/Data/CEDE/PANEL_CONFLICTO_Y_VIOLENCIA(2021).dta") %>% 
  select(starts_with("tpobc") |starts_with("asalt")| starts_with("incup") | 
           c(codmpio, ano, terrorismot, homic_vict, homic_caso, homicidios, desmovilizados, lotes_coca, coca,
             desplazados_expulsion, desplazados_recepcion, H_coca) |
           starts_with("terro") | starts_with("tactof") |starts_with("apoli") | starts_with("homip") | 
           starts_with("homic_caso") | starts_with("errad") | starts_with("o_") ) 

mun_conf_vio <- mun_conf_vio %>% 
  mutate(across(!codmpio & !ano, list(
    lag1 = ~lag(.x, n = 1, order_by = ano), 
    lag2 = ~lag(.x, n = 2, order_by = ano), 
    lead1 = ~lead(.x, n = 1, order_by = ano),
    lead2 = ~lead(.x, n = 2, order_by = ano)
  )))
mun_char <- mun_char %>% left_join(mun_agr, by = c("codmpio", "ano")) %>% left_join(mun_conf_vio, by = c("codmpio", "ano"))

# Mun code
spend_data_baseline_V <- spend_data_baseline_V %>% rename(codmpio = "cod_mun")
mun_char$codmpio <- as.character(mun_char$codmpio)
spend_data_baseline_V$codmpio <- as.character(spend_data_baseline_V$codmpio)
elections$codmpio <- as.character(elections$codmpio)

mun_char <- mun_char %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
  ))

spend_data_baseline_V <- spend_data_baseline_V %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
  ))

elections <- elections %>% 
  mutate(codmpio = case_when(
    str_count(codmpio) == 4 ~ paste0("0", codmpio), 
    T ~ codmpio
  ))

### Merging. 
merged_base_V <- left_join(elections, spend_data_baseline_V, by = c("year", "codmpio")) 
merged_base_V <- merged_base_V %>% 
  left_join(mun_char, by = c("codmpio" = "codmpio", "year" = "ano"))

# Cleaning
merged_base_V <- merged_base_V %>% filter(pobl_tot < 300000) %>% 
  mutate(
    # First year of government
  payment_diff1 = ln_payments_1 - ln_payments,
  # Second year of government
  payment_diff2 = ln_payments_2 - ln_payments
) 

## McCarry test. 
DCdensity(merged_base_V$X_it)


## Baseline results 1. Investment in victims. 
est_1 <- rdd::RDestimate(payment_diff1 ~ X_it, data = merged_base_V)
est_2 <- rdd::RDestimate(payment_diff2 ~ X_it, data = merged_base_V)
summary(est_1)
# Merge with leaders data
merged_base_V <- merged_base_V %>% left_join(lideres_RD)
merged_base_V <- merged_base_V %>% mutate(Tr = as.integer(X_it > 0))
write_dta(merged_base_V, "D:/Documents/GitHub/Thesis/Data/Final_data/baseline_data.dta")



# Not enough match observations. Try pooling following years

lideres_RD <- lideres_RD %>% 
  mutate(lid_2016_2019 = lid2016 + lid2017 + lid2018 + lid2019, 
         lid_2020_2023 = lid2020 + lid2021 + lid2022 + lid2023)
lideres_RD <- lideres_RD %>% select(-c(lid2016:lid2023)) 
lideres_RD <- lideres_RD %>% 
  pivot_longer(cols = c(lid_2016_2019, lid_2020_2023), 
               names_to = "year") %>% mutate(
                 year = case_when(year == "lid_2016_2019" ~ 2015, 
                                  year == "lid_2020_2023" ~ 2019)
               )
merged_base_V <- merged_base_V %>% 
  left_join(lideres_RD, by = c("codmpio", "year"))

merged_base_V$lid_k[is.na(merged_base_V$lid_k)] <- 0
merged_base_V <- merged_base_V %>% mutate(lid_k_ext = as.integer(lid_k > 0), 
                                          lid_k_pop = lid_k/pobl_tot)
# Delete muns with more than 300,000 
merged_base_V <- merged_base_V %>% filter(pobl_tot < 300000)
est_3 <- rdd::RDestimate(lid_k ~ X_it, data = merged_base_V)
summary(est_3)
