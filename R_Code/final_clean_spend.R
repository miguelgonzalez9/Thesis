## Finalizing cleaning spending data
spend_data <- read.csv("D:/Documents/Semestre_2023-II/Thesis/Conflicto Armado/SISFUT_Final/SISFUT_fin_2015-2020.csv")
ipc <- read.csv("~/GitHub/Thesis/Data/other/ipc_colombia.csv", sep = ";")
colnames(ipc) <- c("year", "IPC")
spend_vars <- c("initial_budget", "final_budget", "compromises", "obligations", "payments")
spend_data <- spend_data %>% left_join(ipc, by = c("year")) %>%
  mutate(across(spend_vars, ~ .x/(IPC/100)))
summary(spend_data)

# Baseline spending data  --------------------------------------
spend_data_baseline <- spend_data %>% ungroup() %>% 
  select(cod_concepto, cod_mun,year,payments) %>% group_by(cod_concepto, cod_mun) %>% 
  # Create log version of payments and lags: 2 posterior 1 prior to election year. 
  mutate(ln_payments = log(payments + 1),
         ln_payments_lead1 = lead(ln_payments, 1, order_by = year), 
         ln_payments_lead2 = lead(ln_payments, 2, order_by = year), 
         ln_payments_lag1 = lag(ln_payments, 1, order_by = year),
         ln_payments_lag2 = lag(ln_payments, 2, order_by = year)
         )

#  
spend_data_baseline_V <- spend_data_baseline %>% 
  filter(cod_concepto == "V") %>% ungroup() %>%  select(-cod_concepto) 
x <- colnames(spend_data_baseline_V)[!(colnames(spend_data_baseline_V) %in% c("cod_mun", "year"))]
colnames(spend_data_baseline_V)[!(colnames(spend_data_baseline_V) %in% c("cod_mun", "year"))] <- paste0("V_",x)

spend_data_baseline_V1 <- spend_data_baseline %>% 
  filter(cod_concepto == "V.1") %>% ungroup() %>% select(-cod_concepto) 
x <- colnames(spend_data_baseline_V1)[!(colnames(spend_data_baseline_V1) %in% c("cod_mun", "year"))]
colnames(spend_data_baseline_V1)[!(colnames(spend_data_baseline_V1) %in% c("cod_mun", "year"))] <- paste0("V1_",x)

spend_data_baseline_V <- spend_data_baseline_V %>% left_join(spend_data_baseline_V1, by = c("cod_mun", "year"))

#mutate(across(.cols =all_of(bud_vars), 
#              .fns = list(ln = function(x) log(x + 1), 
 #                         ln_lag = function(x) lag(log(x + 1), 1, order_by = year), 
  #                        ln_diff = function(x) log(x + 1) - lag(log(x + 1), 1, order_by = year)), 
   #           .names = "{.col}_{.fn}"))

