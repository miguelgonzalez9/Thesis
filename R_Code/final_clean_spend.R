## Finalizing cleaning spending data
spend_data <- read.csv("D:/Documents/Semestre_2023-II/Thesis/Conflicto Armado/SISFUT_Final/SISFUT_fin_2015-2020.csv")

# Baseline spending data  --------------------------------------
spend_data_baseline <- spend_data %>% ungroup() %>% 
  select(cod_concepto, cod_mun,year,payments) %>% group_by(cod_concepto, cod_mun) %>% 
  # Create log version of payments and lags: 2 posterior 1 prior to election year. 
  mutate(ln_payments = log(payments + 1),
         ln_payments_1 = lead(ln_payments, 1, order_by = year), 
         ln_payments_2 = lead(ln_payments, 2, order_by = year), 
         ln_payments_1m = lag(ln_payments, 1, order_by = year),
         ln_payments_2m = lag(ln_payments, 3, order_by = year)
         )

#  
spend_data_baseline_V <- spend_data_baseline %>% 
  filter(cod_concepto == "V")
spend_data_baseline_V1 <- spend_data_baseline %>% 
  filter(cod_concepto == "V.1")


#mutate(across(.cols =all_of(bud_vars), 
#              .fns = list(ln = function(x) log(x + 1), 
 #                         ln_lag = function(x) lag(log(x + 1), 1, order_by = year), 
  #                        ln_diff = function(x) log(x + 1) - lag(log(x + 1), 1, order_by = year)), 
   #           .names = "{.col}_{.fn}"))

