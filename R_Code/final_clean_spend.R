## Finalizing cleaning spending data
spend_data <- read.csv("D:/Documents/GitHub/Thesis/Data/Final_data/SISFUT_Final/SISFUT_fin_2015-2020.csv")

spend_data <- spend_data %>% filter(!is.na(cod_mun))
spend_data <- spend_data %>% filter(str_detect(cod_concepto, "V.1|V.6") | cod_concepto == "V")
spend_data <- spend_data %>% select(cod_dpto, cod_mun, cod_concepto, concepto, 
                                    cod_hecho_vict, hecho_vict, presupuesto_inicial, presupuesto_defin,
                                    compromisos, obligaciones, pagos, year)


spend_data <- spend_data %>% rename(
  initial_budget = "presupuesto_inicial",
  final_budget = "presupuesto_defin",
  compromises = "compromisos",
  obligations = "obligaciones",
  payments = "pagos"
)

# Log-transfornmation
bud_vars <- c("initial_budget", "final_budget", "compromises", "obligations", "payments")

spend_data <- spend_data %>% group_by(cod_concepto, cod_mun, cod_hecho_vict, concepto, year) %>% 
  summarise(across(bud_vars, ~ sum(.x, na.rm = T))) %>% 
  mutate(across(.cols =all_of(bud_vars), 
                .fns = list(ln = function(x) log(x + 1), 
                            ln_lag = function(x) lag(log(x + 1), 1, order_by = year), 
                            ln_diff = function(x) log(x + 1) - lag(log(x + 1), 1, order_by = year)), 
                .names = "{.col}_{.fn}"))

write.csv(spend_data, "D:/Documents/GitHub/Thesis/Data/Final_data/SISFUT_Final/exp_data_2016-2021.csv")
write_dta(spend_data, "D:/Documents/GitHub/Thesis/Data/Final_data/SISFUT_Final/exp_data_2016-2021.dta")
