#################################
# RDD results
#################################

# 1. Right wing mayor elected --------------------------
X_it <- running_vars[1]

## 1.1 Violence variables -----------------

# Lead 1 + Lead 2 + Lead 3 + Lead 4
r_res_1234 <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1])),
                      out_vars = paste0("pop_lead1234_",conflict_vars_rd), 
                      controls_list = controls_list_r, 
                      running_var = running_vars[1], subset_logic = T, 
                      digits = 5, out_vars_names = paste0("pop_lead1234_",conflict_vars_rd))

r_res_ln_1234 <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1])), 
                               out_vars = c(paste0("ln_pop_lead1234_",c(conflict_vars_rd))), 
                               controls_list = controls_list_r, 
                               running_var = X_it, 
                               subset_logic = T, 
                               digits = 5, out_vars_names = c(paste0("ln_pop_lead1234_",conflict_vars_rd)))



# Positive effect on comunal leader sector
# Explore effect on leads. 
 
r_res_leads <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & year == 2019), 
                   out_vars = c(paste0("pop_lead1234_",conflict_vars_rd),
                                paste0("lead1_pop_",conflict_vars_rd),
                                paste0("lead2_pop_",conflict_vars_rd),
                                paste0("lead3_pop_",conflict_vars_rd),
                                paste0("lead4_pop_",conflict_vars_rd)), controls_list = controls_list_r, 
                   running_var = X_it, subset_logic = T, 
                   digits = 5, out_vars_names = c(paste0("lead1234_",conflict_vars_rd),
                                                  paste0("lead1_",conflict_vars_rd),
                                                  paste0("lead2_",conflict_vars_rd),
                                                  paste0("lead3_",conflict_vars_rd),
                                                  paste0("lead4_",conflict_vars_rd)))
# Lead 1 No significant effects.
# Lead 2 More MOE (non-right) kills and (less) non-right threats. 
# Lead 3 No signifincat effects. 
# Lead 4. Lower general violence against leaders: campesinos and non leathal vio. 
# Lower victims from belic actions. 
# Lower lead4_moe_social very significant. 

# Time Heterogenoity, across communal sector.

# Lead 1,2,3,4. 
# Estimates are positive across all years but imprecise. 
x <- rdbwselect(RD_baseline$pop_lead1234_lid_comunal_sector, x = RD_baseline$share_diff2_r, 
                covs =RD_baseline[controls_list_r[[3]]], subset = RD_baseline$year == 2019)
h <- x$bws[[4]]

x <- rdbwselect(RD_baseline$pop_lead1234_lid_comunal_sector, x = RD_baseline$share_diff2_r, 
                covs =RD_baseline[controls_list_r[[3]]])
h_pool <- x$bws[[4]]
r_res_comu <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & year == 2019 &
                                               population < 250000), 
                      out_vars = c("pop_lead1234_lid_comunal_sector",
                                   "lead1_pop_lid_comunal_sector",
                                   "lead2_pop_lid_comunal_sector",
                                   "lead3_pop_lid_comunal_sector",
                                   "lead4_pop_lid_comunal_sector"), controls_list = controls_list_r, 
                      running_var = X_it, subset_logic = T, 
                      digits = 5, out_vars_names =c("pop_lead1234_lid_comunal_sector",
                                                    "lead1_pop_lid_comunal_sector",
                                                    "lead2_pop_lid_comunal_sector",
                                                    "lead3_pop_lid_comunal_sector",
                                                    "lead4_pop_lid_comunal_sector"), 
                      h = c(h,h))
# Lead 1,2,3,4. Pool 2019 and 2015 more sample size.
# Same patterns as in 2019 
r_res_comu_pool <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1])), 
                      out_vars = c("pop_lead1234_lid_comunal_sector",
                                   "lead1_pop_lid_comunal_sector",
                                   "lead2_pop_lid_comunal_sector",
                                   "lead3_pop_lid_comunal_sector",
                                   "lead4_pop_lid_comunal_sector"), controls_list = controls_list_r, 
                      running_var = X_it, subset_logic = T, 
                      digits = 5, out_vars_names =c("pop_lead1234_lid_comunal_sector",
                                                    "lead1_pop_lid_comunal_sector",
                                                    "lead2_pop_lid_comunal_sector",
                                                    "lead3_pop_lid_comunal_sector",
                                                    "lead4_pop_lid_comunal_sector"),
                      h = c(h_pool,h_pool))
# Heterogeneity across PDET municipalities
# *******
# Highly signifincant and possitive: issue very small sample size.
# Corroborate with DID. 
x <- rdbwselect(RD_baseline$pop_lead1234_lid_comunal_sector, x = RD_baseline$share_diff2_r, 
                covs =RD_baseline[controls_list_r[[3]]], subset = RD_baseline$year == 2019 & RD_baseline$pdet == 1)
h <- x$bws[[4]]
x <- rdbwselect(RD_baseline$pop_lead1234_lid_comunal_sector, x = RD_baseline$share_diff2_r, 
                covs =RD_baseline[controls_list_r[[3]]], subset = RD_baseline$pdet == 1)
h_pool <- x$bws[[4]]
r_res_comu_pdet <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & pdet == 1 & year == 2019), 
                      out_vars = c("pop_lead1234_lid_comunal_sector",
                                   "lead1_pop_lid_comunal_sector",
                                   "lead2_pop_lid_comunal_sector",
                                   "lead3_pop_lid_comunal_sector",
                                   "lead4_pop_lid_comunal_sector"), controls_list = controls_list_r, 
                      running_var = X_it, subset_logic = T, 
                      digits = 5, out_vars_names =c("pop_lead1234_lid_comunal_sector",
                                                    "lead1_pop_lid_comunal_sector",
                                                    "lead2_pop_lid_comunal_sector",
                                                    "lead3_pop_lid_comunal_sector",
                                                    "lead4_pop_lid_comunal_sector"),
                      h = c(2.5, 2.5))

r_res_comu_pdet_pool <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & pdet == 1 & year == 2019), 
                           out_vars = c("pop_lead1234_lid_comunal_sector",
                                        "lead1_pop_lid_comunal_sector",
                                        "lead2_pop_lid_comunal_sector",
                                        "lead3_pop_lid_comunal_sector",
                                        "lead4_pop_lid_comunal_sector"), controls_list = controls_list_r, 
                           running_var = X_it, subset_logic = T, 
                           digits = 5, out_vars_names =c("pop_lead1234_lid_comunal_sector",
                                                         "lead1_pop_lid_comunal_sector",
                                                         "lead2_pop_lid_comunal_sector",
                                                         "lead3_pop_lid_comunal_sector",
                                                         "lead4_pop_lid_comunal_sector"),
                           h = c(h,h))


# Intensive margin. Very small sample size 50 municipalities left.
r_res_comu_inten <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & lead1234_lid_comunal_sector > 0), 
                           out_vars = c("pop_lead1234_lid_comunal_sector",
                                        "lead1_pop_lid_comunal_sector",
                                        "lead2_pop_lid_comunal_sector",
                                        "lead3_pop_lid_comunal_sector",
                                        "lead4_pop_lid_comunal_sector"), controls_list = controls_list_r, 
                           running_var = X_it, subset_logic = T, 
                           digits = 5, out_vars_names =c("pop_lead1234_lid_comunal_sector",
                                                         "lead1_pop_lid_comunal_sector",
                                                         "lead2_pop_lid_comunal_sector",
                                                         "lead3_pop_lid_comunal_sector",
                                                         "lead4_pop_lid_comunal_sector"))
# Killings PDET.
r_res_pdet <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & year == 2019 & pdet == 1), 
                   out_vars = c(paste0("pop_lead1234_",conflict_vars_rd),
                                paste0("lead1_pop_",conflict_vars_rd),
                                paste0("lead2_pop_",conflict_vars_rd),
                                paste0("lead3_pop_",conflict_vars_rd[-30]),
                                paste0("lead4_pop_",conflict_vars_rd[-c(18,19)])), controls_list = controls_list_r, 
                   running_var = X_it, subset_logic = T, 
                   digits = 5, out_vars_names = c(paste0("lead1234_",conflict_vars_rd),
                                                  paste0("lead1_",conflict_vars_rd),
                                                  paste0("lead2_",conflict_vars_rd),
                                                  paste0("lead3_",conflict_vars_rd[-30]),
                                                  paste0("lead4_",conflict_vars_rd[-c(18:19)])))

# Killings municipalitites with AG presence. 
# Finding *** Very significant and positive results over different types of violence. 
# Specially political violence variables, collective and non-lethal violence, 
# community leader killings, 

temp <- RD_baseline %>% filter(param_pres == 1 & !is.na(running_vars[1]))
x <- rdrobust(y = temp[["pop_lead1234_non_leth_vio"]], x = temp[[running_vars[1]]], covs = temp[controls_list_r[[3]]], 
              p = 2, all = T, subset = T)

rdplot(y = temp[["pop_lead1234_moe_kill"]], x = temp[[running_vars[1]]], covs = temp[controls_list_r[[3]]], 
       p = 1)

rdplot(y = temp[["pop_lead1234_non_leth_vio"]], x = temp[[running_vars[1]]], covs = temp[controls_list_r[[3]]], 
       p = 1)

r_res_kill_param <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                            out_vars = c(paste0("pop_lead1234_",c(conflict_vars_rd))), 
                            controls_list = controls_list_r, 
                            running_var = X_it, 
                            subset_logic = T, 
                            digits = 5, out_vars_names = c(paste0("pop_lead1234_",conflict_vars_rd)))


r_res_kill_ln_param <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                            out_vars = c(paste0("ln_pop_lead1234_",c(conflict_vars_rd))), 
                            controls_list = controls_list_r, 
                            running_var = X_it, 
                            subset_logic = T, 
                            digits = 5, out_vars_names = c(paste0("ln_pop_lead1234_",conflict_vars_rd)))

r_res_kill_noparam <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 0), 
                            out_vars = c(paste0("pop_lead1234_",conflict_vars_rd)), controls_list = controls_list_r, 
                            running_var = X_it, subset_logic = T, 
                            digits = 5, out_vars_names = c(paste0("lead1234_",conflict_vars_rd)))


# Heterogeneity across interactions of insurgency. 
r_res_kill_param_insurg <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1 & insurg_pres == 1), 
                            out_vars = c(paste0("pop_lead1234_",conflict_vars_rd)), controls_list = controls_list_r, 
                            running_var = X_it, subset_logic = T, 
                            digits = 5, out_vars_names = c(paste0("lead1234_",conflict_vars_rd)))

r_res_kill_param_noinsurg <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1 & insurg_pres == 0), 
                                   out_vars = c(paste0("pop_lead1234_",conflict_vars_rd)), controls_list = controls_list_r, 
                                   running_var = X_it, subset_logic = T, 
                                   digits = 5, out_vars_names = c(paste0("lead1234_",conflict_vars_rd)))

# Bacrim presence. Very small sample size, largly varying across specifications. 
r_res_kill_bacrim <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & crimorg_pres == 1), 
                            out_vars = c("pop_lead1234_killing_sd",
                                         "pop_lead1234_lid_assas",
                                         "lead1_pop_killing_sd",
                                         "lead1_pop_lid_assas",
                                         "lead2_pop_killing_sd",
                                         "lead2_pop_lid_assas",
                                         "lead3_pop_killing_sd",
                                         "lead3_pop_lid_assas",
                                         "lead4_pop_killing_sd",
                                         "lead4_pop_lid_assas"), controls_list = controls_list_r, 
                            running_var = X_it, subset_logic = T, 
                            digits = 5, out_vars_names =c("pop_lead1234_killing_sd",
                                                          "pop_lead1234_lid_assas",
                                                          "lead1_pop_killing_sd",
                                                          "lead1_pop_lid_assas",
                                                          "lead2_pop_killing_sd",
                                                          "lead2_pop_lid_assas",
                                                          "lead3_pop_killing_sd",
                                                          "lead3_pop_lid_assas",
                                                          "lead4_pop_killing_sd",
                                                          "lead4_pop_lid_assas"))
# Guerilla presence
r_res_kill_insurg <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & insurg_pres == 1), 
                             out_vars = c("pop_lead1234_killing_sd",
                                          "pop_lead1234_lid_assas",
                                          "lead1_pop_killing_sd",
                                          "lead1_pop_lid_assas",
                                          "lead2_pop_killing_sd",
                                          "lead2_pop_lid_assas",
                                          "lead3_pop_killing_sd",
                                          "lead3_pop_lid_assas",
                                          "lead4_pop_killing_sd",
                                          "lead4_pop_lid_assas"), controls_list = controls_list_r, 
                             running_var = X_it, subset_logic = T, 
                             digits = 5, out_vars_names =c("pop_lead1234_killing_sd",
                                                           "pop_lead1234_lid_assas",
                                                           "lead1_pop_killing_sd",
                                                           "lead1_pop_lid_assas",
                                                           "lead2_pop_killing_sd",
                                                           "lead2_pop_lid_assas",
                                                           "lead3_pop_killing_sd",
                                                           "lead3_pop_lid_assas",
                                                           "lead4_pop_killing_sd",
                                                           "lead4_pop_lid_assas"))

# Restrict to municipalities in spending bandwidth of payments. 
x <- rdbwselect(RD_baseline$V_payment_diff1, x = RD_baseline$share_diff2_r, 
                covs =RD_baseline[controls_list_r[[3]]], subset = RD_baseline$year == 2019)
bw_2019 <- x$bws[c(4)]/sd(RD_baseline %>% filter(year == 2019) %>% pull(V_payment_diff1), na.rm = T)

x_2015 <- rdbwselect(RD_baseline$V_payment_diff1, x = RD_baseline$share_diff2_r, 
                covs =RD_baseline[controls_list_r[[3]]], subset = RD_baseline$year == 2015)
bw_2015 <- x$bws[c(4)]/sd(RD_baseline %>% filter(year == 2015) %>% pull(V_payment_diff1), na.rm = T)

bw_2015_lid_assas <- bw_2015*sd(RD_baseline %>% filter(year == 2015) %>% pull(pop_lead1234_lid_assas), na.rm = T)
bw_2019_lid_assas <- bw_2019*sd(RD_baseline %>% filter(year == 2015) %>% pull(pop_lead1234_lid_assas), na.rm = T)

r_res_234_2015 <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & year == 2015), 
                           out_vars = "pop_lead234_lid_assas", 
                      controls_list = controls_list_r, 
                      running_var = running_vars[1], subset_logic =T, 
                      digits = 5, 
                      out_vars_names ="pop_lead234_lid_assas", 
                      h = c(bw_2015_lid_assas, bw_2015_lid_assas))

r_res_234_2019 <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & year == 2019), 
                          out_vars = "pop_lead234_lid_assas", 
                          controls_list = controls_list_r, 
                          running_var = running_vars[1], subset_logic =T, 
                          digits = 5, 
                          out_vars_names ="pop_lead234_lid_assas", 
                          h = c(bw_2019_lid_assas, bw_2019_lid_assas))


# Different Incumbent.
r_inc_1234 <- reg_tab(RD_baseline %>% filter(ideol_incum_r != 2)
                        ,out_vars = paste0("pop_lead1234_",conflict_vars_rd), 
                      controls_list = controls_list_r[-3], 
                      running_var = running_vars[1], subset_logic = T, 
                      digits = 5, out_vars_names = paste0("pop_lead1234_",conflict_vars_rd))
# 

r_inc_1234_2019 <- reg_tab(RD_baseline %>% filter(year == 2019 & ideol_incum_r != 2)
                      ,out_vars = paste0("pop_lead1234_",conflict_vars_rd), 
                      controls_list = controls_list_r[-3], 
                      running_var = running_vars[1], subset_logic = T, 
                      digits = 5, out_vars_names = paste0("pop_lead1234_",conflict_vars_rd))

# Only center incumbents

## 2019. More ex-Farc and comunal leaders. Lower indigenous leaders.  
r_res_1234_incum_2019 <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & year == 2019 & ideol_incum_r == 3),
                      out_vars = paste0("pop_lead1234_",conflict_vars_rd), 
                      controls_list = controls_list_r_incum, 
                      running_var = running_vars[1], subset_logic = T, 
                      digits = 5, out_vars_names = paste0("pop_lead1234_",conflict_vars_rd))


# 2015. 
r_res_1234_incum_2015 <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & year == 2015 & ideol_incum_r %in% c(3)),
                      out_vars = paste0("pop_lead1234_",conflict_vars_rd), 
                      controls_list = controls_list_r_incum, 
                      running_var = running_vars[1], subset_logic = T, 
                      digits = 5, out_vars_names = paste0("pop_lead1234_",conflict_vars_rd))

# All
r_res_1234_incum <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & ideol_incum_r %in% c(3)),
                      out_vars = paste0("pop_lead1234_",conflict_vars_rd), 
                      controls_list = controls_list_r_incum, 
                      running_var = running_vars[1], subset_logic = T, 
                      digits = 5, out_vars_names = paste0("pop_lead1234_",conflict_vars_rd))

# Placebo comunal leaders. Works.  
temp <- RD_baseline %>% filter(!is.na(running_vars[1]) & ideol_incum_r %in% c(3))
x <- rdbwselect(temp$pop_lead1234_lid_comunal_sector, x = temp$share_diff2_r, q = 2, 
           covs = temp[controls_list_r_incum[[4]]])
h <- x$bws[4]


cov_bal_incum <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & ideol_incum_r %in% c(3)),
                              out_vars = controls_list_r_incum[[4]][-c(4,6:9, 12,13,14)], 
                              controls_list = list(nc = ""), 
                              running_var = running_vars[1], subset_logic = T, 
                              digits = 5, out_vars_names = controls_list_r_incum[[4]][-c(4,6:9, 12,13,14)], 
                              h = c(h,h))


# Placebo. Year of election, stress test. Only possible for 2019. 

placebo_res <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & year == 2019 & ideol_incum_r %in% c(3)),
                      out_vars = c("pop_lid_comunal_sector",
                                   "pop_lag123_lid_comunal_sector",
                                   "pop_lid_indigenas_sector",
                                   "pop_lag123_lid_indigenas_sector"), 
                      controls_list = controls_list_r_incum, 
                      running_var = running_vars[1], subset_logic = T, 
                      digits = 5, out_vars_names = c("pop_lid_comunal_sector",
                                                     "pop_lag123_lid_comunal_sector",
                                                     "pop_lid_indigenas_sector",
                                                     "pop_lag123_lid_indigenas_sector"),
                 h = c(h,h))

# Explore heterogeneity across years. 
r_res_1234_incum_2019_het <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & year == 2019 & ideol_incum_r == 3),
                                 out_vars = paste0("pop_lead1234_",conflict_vars_rd), 
                                 controls_list = controls_list_r_incum, 
                                 running_var = running_vars[1], subset_logic = T, 
                                 digits = 5, out_vars_names = paste0("pop_lead1234_",conflict_vars_rd))

# Explore parametric estimation and heterogeneity. 

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
            c("Leader Killings", "Collective Threat", "Left-Wing Leader Killings"))

tab_1_vars <- c("pop_lead1234_lid_assas", "pop_lead1234_collective_threat", "pop_lead1234_left_kill")

table_1 <- reg_tab(out_vars =c(lead1_spending_vars,tab_1_vars), controls_list = controls_list_r, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)

foot <- "This table presents baseline results on the election of a right-wing candidate
on violence in the following four years, and spending on armed conflic victims' compensation and secutrity.
Robust standard erros are used following (cite cattolino), across different specification varying controls
and polynomial degree. Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015, 
poverty during 2005, and loans to small firms before 2015." %>% str_replace_all("[\r\n]" , "") %>% trimws()

tab_1 <- kbl(table_1, booktabs = T, align = "c", format = "html",longtable = F,  
             caption = "Effect of right-wing mayor election violence and public spending on armed conflict victims") %>% 
  add_header_above(c("","Linear Polynomial" = ncol(table_1)/2 - 1, "Quadratic Polynomial" = ncol(table_1)/2 -1 , "")) %>% 
  add_header_above(c("Panel A. Non-parametric Estiamtes" = ncol(table_1))) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Panel A.1 Spending on armed conflict victims in the following year after election. Constant colombian pesos", 1, 8, bold = T) %>% 
  pack_rows("Panel A.2 Violence in the following 4 years after election normalized by population.", 9, 14, bold = T) %>% 
  pack_rows("Specification details", 15, 18, bold = T) %>% 
  footnote(general = foot,
           number = c("Spending variables are deflated using Colombia's CPI. Variables in changes, are specificed as the difference in logs between period of election and following period", 
                      "All violence variables are normalized for every 100.000 municipality inhabitants.")
  )

writeLines(tab_1, "D:/Documents/GitHub/Thesis/Tables_tex/table1A.tex")

# Appendix 1A. Table. 


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


#####################################################################


