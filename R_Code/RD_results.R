#################################
# RDD results
#################################

pop_conflict_vars <- paste0("pop_",conflict_vars)[c(1:2, 7, 11, 13, 14, 16,29,31,49)]
conflict_vars_rd <- conflict_vars[c(1:2, 7, 11, 13, 14, 16,29,31,49)]
lead1_spending_vars <- c(spend_vars[str_detect(spend_vars, "lead1")], "V1_payment_diff1", "V_payment_diff1")
lead2_spending_vars <- c(spend_vars[str_detect(spend_vars, "lead2")], "V1_payment_diff2", "V_payment_diff2")

geo_controls <- control_vars[c(2:6, 9:12)]
soc_controls <- control_vars[c(13:20, 7, 8)]
running_vars <- c("share_diff2_r", "share_diff1_l")
controls_list <- list(c0 = "",c1 = geo_controls, c2 = soc_controls, c3 =  c(geo_controls, soc_controls))


# 1. Right wing mayor elected --------------------------
X_it <- running_vars[1]
## 1.1 Violence variables -----------------
rnames <- c("Killing", "Threat", "Failed killing", "Insurgent Violent Act", 
            "Criminal Band Violent Act", "State Forces Violent Act", 
            "Violent Act Against Community Leader", "Violent Act", "Killing Leader (Somos Defensores)", 
            "Belic Action")
# Lead 1
r_res_1 <- reg_tab(out_vars = paste0("lead1_",pop_conflict_vars), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)
# Lead 2
r_res_2 <- reg_tab(out_vars = paste0("lead2_",pop_conflict_vars), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)
# Lead 3
r_res_3 <- reg_tab(out_vars = paste0("lead3_",pop_conflict_vars), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)

# Lead 4
r_res_4 <- reg_tab(out_vars = paste0("lead4_",pop_conflict_vars), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)


# Lead 1 + Lead 2
r_res_12 <- reg_tab(out_vars = paste0("lead12_",conflict_vars_rd), controls_list = controls_list, 
                    running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                    digits = 5, out_vars_names = rnames)

# Lead 1 + Lead 2 + Lead 3
r_res_123 <- reg_tab(out_vars = paste0("lead123_",conflict_vars_rd), controls_list = controls_list, 
                     running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                     digits = 5, out_vars_names = rnames)
# Lead 1 + Lead 2 + Lead 3 + Lead 4
r_res_1234 <- reg_tab(out_vars = paste0("pop_lead1234_",conflict_vars_rd), controls_list = controls_list, 
                      running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                      digits = 5, out_vars_names = rnames)

## 1.2 Spending variables.
rnames <- c("Spending on Armed Conflict Victims (log)", "Spending on Armed Conflict Victims' Security (log)", 
            "Spending on Armed Conflict Victims' Security (% change)", "Spending on Armed Conflict Victims (% change)")
r_res_spend_1 <- reg_tab(out_vars =lead1_spending_vars, controls_list = controls_list, 
                         running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                         digits = 5, out_vars_names = rnames)

r_res_spend_1_2015 <- reg_tab(out_vars =lead1_spending_vars, controls_list = controls_list, 
                              running_var = X_it, subset_logic = RD_baseline$year == 2015, 
                              digits = 5, out_vars_names = rnames)

r_res_spend_2_2015 <- reg_tab(out_vars =lead2_spending_vars, controls_list = controls_list, 
                              running_var = X_it, subset_logic = RD_baseline$year == 2015, 
                              digits = 5, out_vars_names = rnames)

r_res_spend_1_all <- reg_tab(out_vars =lead1_spending_vars, controls_list = controls_list, 
                             running_var = X_it, subset_logic = RD_baseline$year %in% c(2015, 2019), 
                             digits = 5, out_vars_names = rnames)

# Table 1a. Non-paremetric estimates. Violence leads1-4 and spending lead1 2019. 
rnames <- c(c("Overall spending (log-scale)", "Security spending (log-scale)", 
              "Security spending (% change)", "Overall spending (% change)"), 
            c("Killing", "Threat", "Failed killing", "Insurgent", 
              "Criminal Band", "State Forces", 
              "Community Leader", "Violent Act", "Leader killing (SD)", 
              "Belic Action"))
rnames_1 <- rnames
out_num <- 1:length(rnames)
rnames_1[2*out_num] <- ""
rnames_1[2*out_num - 1] <- rnames



table_1 <- reg_tab(out_vars =c(lead1_spending_vars,paste0("pop_lead1234_",conflict_vars_rd)), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)


tab_1 <- kbl(table_1, booktabs = T, align = "c", format = "latex",longtable = T, 
             caption = "Effect of right-wing mayor election violence and spending on victim's compensation") %>% 
  add_header_above(c("","Linear Polynomial" = 4, "Quadratic Polynomial" = 4, "")) %>% 
  add_header_above(c("Panel A. Non-parametric Estiamtes" = 10)) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Panel A.1 Spending on armed conflict victims in the following year after election. Constant colombian pesos", 1, 8, bold = T) %>% 
  pack_rows("Panel A.2 Violence in the following 4 years after election normalized by population.", 9, 27, bold = T) %>% 
  pack_rows("Specification details", 28, 30, bold = T) %>% 
  footnote(general = "This table presents baseline results on the election of a right-wing candidate
           on violence in the following four years, and spending on armed conflic victims' compensation and secutrity.
           Robust standard erros are used following (cite cattolino), across different specification varying controls
           and polynomial degree. Geographical controls include distance variables to main economic centers, region dummies, 
           altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
           presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015, 
           poverty during 2005, and loans to small firms before 2015. ",
           number = c("Spending variables are deflated using Colombia's CPI. Variables in changes, are 
                      specificed as the difference in logs between period of election and following period", 
                      "All violence variables are normalized for every 100.000 municipality inhabitants.")
  )

writeLines(tab_1, "D:/Documents/GitHub/Thesis/Tables_tex/table1A.tex")


# 2. Left wing election. 
X_it <- running_vars[2]

## 1.1 Violence variables -----------------
rnames <- c("Killing", "Threat", "Failed killing", "Insurgent", 
            "Criminal Band", "State Forces", 
            "Community Leader", "Violent Act", "Killing Leader (SD)", 
            "Belic Action")
# Lead 1
l_res_1 <- reg_tab(out_vars = paste0("lead1_",pop_conflict_vars), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)
# Lead 2
l_res_2 <- reg_tab(out_vars = paste0("lead2_",pop_conflict_vars), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)
# Lead 3
l_res_3 <- reg_tab(out_vars = paste0("lead3_",pop_conflict_vars), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)

# Lead 4
l_res_4 <- reg_tab(out_vars = paste0("lead4_",pop_conflict_vars), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)


# Lead 1 + Lead 2
l_res_12 <- reg_tab(out_vars = paste0("lead12_",conflict_vars_rd), controls_list = controls_list, 
                    running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                    digits = 5, out_vars_names = rnames)

# Lead 1 + Lead 2 + Lead 3
l_res_123 <- reg_tab(out_vars = paste0("lead123_",conflict_vars_rd), controls_list = controls_list, 
                     running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                     digits = 5, out_vars_names = rnames)
# Lead 1 + Lead 2 + Lead 3 + Lead 4
l_res_1234 <- reg_tab(out_vars = paste0("pop_lead1234_",conflict_vars_rd), controls_list = controls_list, 
                      running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                      digits = 5, out_vars_names = rnames)

# 2015  --> Lower violence when left wing elected. Contradicts theory of Kalyvas 
# and results from Fergusson et al. 
# When 
l_res_1234_2015 <- reg_tab(out_vars = paste0("pop_lead1234_",conflict_vars_rd), controls_list = controls_list, 
                           running_var = X_it, subset_logic = RD_baseline$year == 2015, 
                           digits = 5, out_vars_names = rnames)
l_res_12_2015 <- reg_tab(out_vars = paste0("pop_lead12_",conflict_vars_rd), controls_list = controls_list, 
                         running_var = X_it, subset_logic = RD_baseline$year == 2015, 
                         digits = 5, out_vars_names = rnames)


## 1.2 Spending variables.
rnames <- c("Spending on Armed Conflict Victims (log)", "Spending on Armed Conflict Victims' Security (log)", 
            "Spending on Armed Conflict Victims' Security (% change)", "Spending on Armed Conflict Victims (% change)")
l_res_spend_1 <- reg_tab(out_vars =lead1_spending_vars, controls_list = controls_list, 
                         running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                         digits = 5, out_vars_names = rnames)

l_res_spend_1_2015 <- reg_tab(out_vars =lead1_spending_vars, controls_list = controls_list, 
                              running_var = X_it, subset_logic = RD_baseline$year == 2015, 
                              digits = 5, out_vars_names = rnames)

l_res_spend_2_2015 <- reg_tab(out_vars =lead2_spending_vars, controls_list = controls_list, 
                              running_var = X_it, subset_logic = RD_baseline$year == 2015, 
                              digits = 5, out_vars_names = rnames)

l_res_spend_1_all <- reg_tab(out_vars =lead1_spending_vars, controls_list = controls_list, 
                             running_var = X_it, subset_logic = RD_baseline$year %in% c(2015, 2019), 
                             digits = 5, out_vars_names = rnames)


table_2 <- reg_tab(out_vars =c(lead1_spending_vars,paste0("pop_lead1234_",conflict_vars_rd)), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)


tab_2 <- kbl(table_2, booktabs = T, align = "c", format = "latex",longtable = T, 
             caption = "Effect of left-wing mayor election violence and spending on victim's compensation during 2019") %>% 
  add_header_above(c("","Linear Polynomial" = 4, "Quadratic Polynomial" = 4, "")) %>% 
  add_header_above(c("Panel A. Non-parametric Estiamtes" = 10)) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Panel A.1 Spending on armed conflict victims in the following year after election. Constant colombian pesos", 1, 8, bold = T) %>% 
  pack_rows("Panel A.2 Violence in the following 4 years after election normalized by population.", 9, 27, bold = T) %>% 
  pack_rows("Specification details", 28, 30, bold = T) %>% 
  footnote(general = "This table presents baseline results on the election of a right-wing candidate
           on violence in the following four years, and spending on armed conflic victims' compensation and secutrity.
           Robust standard erros are used following (cite cattolino), across different specification varying controls
           and polynomial degree. Geographical controls include distance variables to main economic centers, region dummies, 
           altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
           presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015, 
           poverty during 2005, and loans to small firms before 2015. ",
           number = c("Spending variables are deflated using Colombia's CPI. Variables in changes, are 
                      specificed as the difference in logs between period of election and following period", 
                      "All violence variables are normalized for every 100.000 municipality inhabitants.")
  )

writeLines(tab_2, "D:/Documents/GitHub/Thesis/Tables_tex/table2A.tex")