#####################################3
#  RDD Results final. 
# 30/04/2024
#######################################

# All. 
r_res_1234 <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1])),
                      out_vars = paste0("pop_lead1234_",conflict_vars_rd), 
                      controls_list = controls_list_r, 
                      running_var = running_vars[1], subset_logic = T, 
                      digits = 5, out_vars_names = paste0("pop_lead1234_",conflict_vars_rd))

r_res_1234_param <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1),
                      out_vars = paste0("pop_lead1234_",conflict_vars_rd), 
                      controls_list = controls_list_r[-1], 
                      running_var = running_vars[1], subset_logic = T, 
                      digits = 5, out_vars_names = paste0("pop_lead1234_",conflict_vars_rd))
# Logarithms 
r_res_1234_ln <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1])),
                      out_vars = paste0("ln_pop_lead1234_",conflict_vars_rd), 
                      controls_list = controls_list_r, 
                      running_var = running_vars[1], subset_logic = T, 
                      digits = 5, out_vars_names = paste0("pop_lead1234_",conflict_vars_rd))

r_res_1234_param_ln <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1),
                            out_vars = paste0("ln_pop_lead1234_",conflict_vars_rd), 
                            controls_list = controls_list_r[-1], 
                            running_var = running_vars[1], subset_logic = T, 
                            digits = 5, out_vars_names = paste0("pop_lead1234_",conflict_vars_rd))

# Results table 1. 
panel_a_vars <- c("pop_lead1234_lid_assas", "pop_lead1234_moe_pol_kill")
panel_b_vars <- c("pop_lead1234_lid_comunal_sector", "pop_lead1234_lid_campesino_sector", 
                  "pop_lead1234_lid_indigenas_sector")
panel_c_vars <- c(panel_a_vars, panel_b_vars)
hetero_vars <- c("pop_lead1234_moe_pol_kill", "")

rnames <- c("Social Leader", "Political Leader", 
              "Communal leader", "Peasent Leader", "Indigenous Leader")

table_r1ab <-  reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1])), 
                      out_vars = c(panel_a_vars, panel_b_vars), 
                      controls_list = controls_list_r[-1], 
                      running_var = X_it, 
                      subset_logic = T, 
                      digits = 3, out_vars_names = rnames)


foot <- "Robust standard errors in parenthesis. *** p\<0.01, ** p\<0.05, * p\<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression.
Voilence outcomes correspond to the number of victims in the four years following electoral year, normalized for every 100.000 municipality inhabitants.Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015 and multidimensional 
poverty during 2005." %>% str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")

tab_1 <- kbl(table_r1ab, booktabs = T, escape = F ,align = "c", format = "latex",longtable = F,  
             caption = "Effect of right-wing mayor election on social and political leader killings") %>% 
  add_header_above(c("","Linear Polynomial" = 2, "Quadratic Polynomial" = 2)) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Panel A Social and political leaders", 1, 8, bold = T) %>% 
  pack_rows("Panel B Disagregation of social leaders", 9, 20, bold = T) %>% 
  pack_rows("Specification details", 21, 24, bold = T) %>% 
  footnote(general = foot, threeparttable = T, escape = F
  )

writeLines(tab_1, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table1.tex")

# Table 2. Paramilitary presence

table_r2 <-  reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                       out_vars = c(panel_a_vars, panel_b_vars), 
                       controls_list = controls_list_r[-1], 
                       running_var = X_it, 
                       subset_logic = T, 
                       digits = 3, out_vars_names = rnames)



foot <- "Robust standard errors in parenthesis. *** p<0.01, ** p<0.05, * p<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
base on the subsample of municipalities with paramilitary presence after 2016. 
Voilence outcomes correspond to the number of victims in the four years following electoral year, normalized for every 100.000 municipality inhabitants.
Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015 and multidimensional 
poverty during 2005." %>% str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")

tab_2 <- kbl(table_r2, booktabs = T, escape = F, align = "c", format = "latex",longtable = F,  
             caption = "Effect of right-wing mayor election on social and political leader killings. Paramilitary presence subsample") %>% 
  add_header_above(c("","Linear Polynomial" = 2, "Quadratic Polynomial" = 2)) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Panel A Social and political leaders", 1, 8, bold = T) %>% 
  pack_rows("Panel B Disagregation of social leaders", 9, 20, bold = T) %>% 
  pack_rows("Specification details", 21, 24, bold = T) %>% 
  footnote(general = foot, threeparttable = T, escape = F
  )

writeLines(tab_2, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table2.tex")


# McCarry test param muns
MC_test_2 <- rddensity(RD_baseline[RD_baseline$param_pres == 1,]$share_diff2_r, c = 0)
t <- MC_test_2$test$t_jk
p <- MC_test_2$test$p_jk
N <- MC_test_2$N
if (p < 0.01){
  t <- paste0(t, "***")
} else if (p < 0.05){
  t <- paste0(t, "**")
} else if (p < 0.1){
  t <- paste0(t, "*")
}



Mc_plot_2 <- rdplotdensity(MC_test_2, RD_baseline[RD_baseline$param_pres == 1,]$share_diff2_r, plotGrid = "es",
                           type = "both", CItype = "line",
                           lcol = 1, hist = T, plotN = c(10,10), plotRange = c(-0.6,0.6))

Pl_2 <- Mc_plot_2$Estplot +  
  labs(
    caption = paste0("Discontinuity estimate (p-value) = ", round(t,3), " (", round(p,3), ").",
                     "N = ",N, ".")) + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10)),
        axis.title.x = element_text(size = 12),  # Adjust size of x-axis label
        axis.title.y = element_text(size = 12)) +  # Adjust size of y-axis label)
  xlab("Relative vote share for right wing candidate") +
  ylab("Density")

ggsave(filename = "McCarry_param.pdf", plot = Pl_2, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")
# Robusrness across measurements.

panel_c_vars_1 <- str_remove(c(panel_a_vars, panel_b_vars), "pop_")

# SD 
rnames <- c("Communal Leader (CINEP)",
            "Political Leader",  "Communal Leader (INDEPAZ)", 
            "Communal Leader (CINEP)")

r_res_kill_param_rob1 <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                                 out_vars = c("pop_lead1234_comunal_sector_sd",
                                              "lead1234_moe_pol_kill",  "lead1234_lid_comunal_sector", 
                                              "lead1234_comunal_sector_sd"), 
                                 controls_list = controls_list_r[-1], 
                                 running_var = X_it, 
                                 subset_logic = T, 
                                 digits = 5, out_vars_names = rnames)

foot <- "Robust standard errors in parenthesis. *** p\<0.01, ** p\<0.05, * p\<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
base on the subsample of municipalities with paramilitary presence after 2016. 
Voilence outcomes correspond to the number of victims in the four years following electoral year, normalized for every 100.000 municipality inhabitants.Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015 and multidimensional 
poverty during 2005. Raw refers to variables not normalized by population." %>% str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")

tab_3 <- kbl(r_res_kill_param_rob1, booktabs = T, escape = F ,align = "c", format = "latex",longtable = F,  
             caption = "Measurment Robustness", label = "rob_measure") %>% 
  add_header_above(c("","Linear Polynomial" = 2, "Quadratic Polynomial" = 2)) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Alternative Measurement", 1, 4, bold = T) %>% 
  pack_rows("Raw Outcomes", 5, 16, bold = T) %>% 
  pack_rows("Specification Details", 17, 20, bold = T) %>% 
  footnote(general = foot, threeparttable = T, escape = F)

writeLines(tab_3, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table3.tex")

# Robustness across badwidths. 
r_res_kill_param_rob3 <- bdwidth_rob(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                                     out_vars = c("pop_lead1234_lid_comunal_sector", "pop_lead1234_moe_pol_kill"), 
                                     controls = controls_list_r[[3]], running_var = X_it)
pdf(file="D:/Documents/GitHub/Thesis/Figures/bandwidth_rob.pdf", width = 10, height = 5)
par( mfrow = c(1,2))
coefplot(r_res_kill_param_rob3$beta[[1]], ci_low = r_res_kill_param_rob3$ci_low[[1]], 
         ci_high = r_res_kill_param_rob3$ci_high[[1]], x = r_res_kill_param_rob3$badwidth[[1]], 
         main = "(A) Communal Leader",ylab = "Killings for every 100.000 inhabitants", 
         xlab = "Years after election")

coefplot(r_res_kill_param_rob3$beta[[2]], ci_low = r_res_kill_param_rob3$ci_low[[2]], 
         ci_high = r_res_kill_param_rob3$ci_high[[2]], x = r_res_kill_param_rob3$badwidth[[2]], 
         main = "(B) Political Leader", ylab = "Killings for every 100.000 inhabitants", 
         xlab = "Years after election")
dev.off()
#Outliers: 1) Robustness to log transformation. 

# # Robusrness droping 1% most violent muns, fail for communal leaders (SD and INDEAPZ) --< probably due to small sample size
## Check for political violence results.
## Check for non lethal violence and collective violence.

r_res_kill_param_rob4a <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1) %>% filter( 
  pop_lead1234_lid_comunal_sector < 
    quantile(pop_lead1234_lid_comunal_sector, probs = .99)), 
  out_vars = c("pop_lead1234_lid_comunal_sector"), 
  controls_list = controls_list_r, 
  running_var = X_it, 
  subset_logic = T, 
  digits = 5, out_vars_names = c("Communal Leader"))

r_res_kill_param_rob4b <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1) %>% filter( 
  pop_lead1234_lid_comunal_sector < 
    quantile(pop_lead1234_lid_comunal_sector, probs = .99)), 
  out_vars = c("pop_lead1234_moe_pol_kill"), 
  controls_list = controls_list_r, 
  running_var = X_it, 
  subset_logic = T, 
  digits = 5, out_vars_names = c("Political Leader"))

# Robustness to dummy and log specifications. 
# Dummies
r_res_kill_param_rob5 <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                                 out_vars = c("lead1234_lid_comunal_sector_dum", "lead1234_moe_pol_kill_dum",
                                              "ln_pop_lead1234_lid_comunal_sector", "ln_pop_lead1234_moe_pol_kill"),
                                 controls_list = controls_list_r[-1], 
                                 running_var = X_it, 
                                 subset_logic = T, 
                                 digits = 5, out_vars_names = c("Communal Leader", "Political Leader", 
                                                                "Communal Leader", "Political Leader"))

foot <- "Robust standard errors in parenthesis. *** p\<0.01, ** p\<0.05, * p\<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
base on the subsample of municipalities with paramilitary presence after 2016. 
Voilence outcomes correspond to the number of victims in the four years following electoral year, normalized for every 100.000 municipality inhabitants.Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015 and multidimensional 
poverty during 2005. Logarithmic specification 
is calculated by taking log(x +1)." %>% 
  str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")

tab_4 <- kbl(r_res_kill_param_rob5, booktabs = T, escape = F ,align = "c", format = "latex",longtable = F,  
             caption = "Outlier Robustness", label = "outlier_rob") %>% 
  add_header_above(c("","Linear Polynomial" = 2, "Quadratic Polynomial" = 2)) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Dummy", 1, 8, bold = T) %>% 
  pack_rows("Logarithmic", 9, 16, bold = T) %>% 
  pack_rows("Specification Details", 17, 20, bold = T) %>% 
  footnote(general = foot, threeparttable = T, escape = F)

writeLines(tab_4, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table4.tex")


# Paramilitary presence balance. Good balance almost across all covariates.
temp <- RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1)
x <- rdbwselect(temp$pop_lead1234_lid_comunal_sector, p = 1, covs = temp[controls_list_r[[3]]],x = temp$share_diff2_r)

temp_1 <- RD_baseline %>% filter(!is.na(running_vars[1]))
x_1 <- rdbwselect(temp$pop_lead1234_lid_comunal_sector, p = 1, covs = temp[controls_list_r[[3]]],x = temp$share_diff2_r)


# Covariates.
rnames <- c("Height", "Area (km2)", "Distance dpto capital", "Distance market", 
            "Distance Bogotá","Carebean region", "Pacific region", "Orinoquia region",
            "Amazon region","Indigenous population (1535-1540)", "Demobilized paramilitaries",
            "Demobilized guerrillas", "Demobilized combatants", "MPI", "UBN", "Population", 
            "Right-Wing incumbent", "Non-right-wing incumbent")



r_res_kill_param_rob6 <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1)
                    ,out_vars = c(geo_controls, soc_controls, "population", "nr_ideol_incum_r", "r_ideol_incum_r")[-c(10:13)], controls_list = list(nc = ""), 
                    running_var = running_vars[1], subset_logic = T,
                    digits = 5, out_vars_names = rnames, h = c(x$bws[[4]], x$bws[[4]]))
r_res_kill_param_rob6 <- r_res_kill_param_rob6[-c(37:40),]
r_res_kill_param_rob6$N <- NULL
r_res_kill_param_rob6 <- r_res_kill_param_rob6 %>% rbind(c("Bandwidth", round(x$bws[[4]], 3), round(x$bws[[4]], 3)))

r_res_kill_param_rob6b <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]))
                                 ,out_vars = c(geo_controls, soc_controls, "population","nr_ideol_incum_r", "r_ideol_incum_r")[-c(10:13)], controls_list = list(nc = ""), 
                                 running_var = running_vars[1], subset_logic = T,
                                 digits = 5, out_vars_names = rnames, h = c(x_1$bws[[4]], x_1$bws[[4]]))
r_res_kill_param_rob6b <- r_res_kill_param_rob6b[-c(37:40),]
r_res_kill_param_rob6b$N <- NULL
r_res_kill_param_rob6b <- r_res_kill_param_rob6b %>% rbind(c("Bandwidth", round(x_1$bws[[4]], 3), round(x_1$bws[[4]], 3)))

foot <- "Robust standard errors in parenthesis. *** p<0.01, ** p<0.05, * p<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
based on the subsample of municipalities with paramilitary presence after 2016." %>% 
  str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")

tab_5 <- kbl(r_res_kill_param_rob6, booktabs = T, escape = F ,align = "c", format = "latex",
             longtable = F,  
             caption = "Covariate balance municipalities with paramilitary presence", label = "cov_bal_param") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  footnote(general = foot,  threeparttable = T, escape = F)

writeLines(tab_5, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table5.tex")

tab_11 <- kbl(r_res_kill_param_rob6b, booktabs = T, escape = F ,align = "c", format = "latex",
             longtable = F,  
             caption = "Covariate balance", label = "cov_bal") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  footnote(general = foot,  threeparttable = T, escape = F)

writeLines(tab_11, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table11.tex")


# Pre trends. Good.
r_res_kill_param_rob7 <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                                    out_vars = c("ln_pop_comunal_sector_sd","ln_pop_lid_comunal_sector", "ln_pop_moe_pol_kill"), controls_list = controls_list_r[-1], 
                                    running_var = X_it, subset_logic = T, 
                                    digits = 5, out_vars_names = c("Communal Leader (CINEP)","Communal Leader (INDEPAZ)", "Political Leader"))

r_res_kill_param_rob7_1 <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                                   out_vars = c("comunal_sector_sd_dum","lid_comunal_sector_dum", "moe_pol_kill_dum"), controls_list = controls_list_r[-1], 
                                   running_var = X_it, subset_logic = T, 
                                   digits = 5, out_vars_names = c("Communal Leader (CINEP)","Communal Leader (INDEPAZ)", "Political Leader"))


foot <- "Robust standard errors in parenthesis. *** p\\<0.01, ** p\\<0.05, * p\\<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
base on the subsample of municipalities with paramilitary presence after 2016. 
Voilence outcomes correspond to the number of victims in the four years following electoral year, normalized for every 100.000 municipality inhabitants.Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015 and multidimensional 
poverty during 2005. Electoral years correspond to 2019 and 2015." %>% str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")

tab_6 <- kbl(r_res_kill_param_rob7, booktabs = T, escape = F ,align = "c", format = "latex",
             longtable = F,  
             caption = "Placebo test outcomes at electoral years. Log-level specification", label = "pre_trends_param") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  footnote(general = foot,  threeparttable = T, escape = F)

writeLines(tab_6, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table6.tex")

tab_6b <- kbl(r_res_kill_param_rob7_1, booktabs = T, escape = F ,align = "c", format = "latex",
             longtable = F,  
             caption = "Placebo test outcomes at electoral years. Extensive margin", label = "pre_trends_param_dum") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  footnote(general = foot,  threeparttable = T, escape = F)

writeLines(tab_6b, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table6b.tex")

################################################
# Mechanisms

# 1. Subdivisions of political vairables into party affilliation. 
v <- c("moe_left_kill", "moe_center_kill","moe_right_kill", "moe_other_kill")
foot <- "Robust standard errors in parenthesis. *** p\\<0.01, ** p\\<0.05, * p\\<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
base on the subsample of municipalities with paramilitary presence after 2016. 
Voilence outcomes correspond to the number of victims in the four years following electoral year, normalized for every 100.000 municipality inhabitants.Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015 and multidimensional 
poverty during 2005. Electoral years correspond to 2019 and 2015." %>% 
  str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")
rnames <- c("Left-Wing Leader", "Center Leader", 
            "Right-wing Leader", "Other Party Leader")
r_res_kill_param_mec1 <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                                 out_vars = c(paste0("pop_lead1234_",v)), controls_list = controls_list_r[-1], 
                                 running_var = X_it, subset_logic = T, 
                                 digits = 5, out_vars_names = rnames)

tab_7 <- kbl(r_res_kill_param_mec1, booktabs = T, escape = F ,align = "c", format = "latex",
             longtable = F,  
             caption = "Heterogeneous effects across political leaders partisanship", 
             label = "pol_party_param") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  footnote(general = foot,  threeparttable = T, escape = F)

writeLines(tab_7, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table7.tex")

# 2. Heterogeneity across time. 
v <- c("lid_comunal_sector", "moe_pol_kill")
r_res_kill_param_mec1 <- reg_tab(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                                 out_vars = c(paste0("lead1_pop_",v), 
                                              paste0("lead2_pop_",v),
                                              paste0("lead3_pop_",v),
                                              paste0("lead4_pop_",v)), controls_list = controls_list_r[-1], 
                                 running_var = X_it, subset_logic = T, 
                                 digits = 5, out_vars_names = c(paste0("pop_lead1_",v), 
                                                                paste0("pop_lead2_",v),
                                                                paste0("pop_lead3_",v),
                                                                paste0("pop_lead4_",v)))

r_res_kill_param_mec2 <- year_het(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                                    controls = controls_list_r[[3]],running_var =  X_it, 
                                  out_vars =  c("lid_comunal_sector", "moe_pol_kill"))


pdf(file="D:/Documents/GitHub/Thesis/Figures/time_het.pdf", width = 10, height = 5)
par( mfrow = c(1,2))

coefplot(r_res_kill_param_mec2$beta[[1]], ci_low = r_res_kill_param_mec2$ci_low[[1]], 
         ci_high = r_res_kill_param_mec2$ci_high[[1]], 
         main = "(A) Communal Leader", ylab = "Killings for every 100.000 inhabitants", 
         xlab = "Years after election")

coefplot(r_res_kill_param_mec2$beta[[2]], ci_low = r_res_kill_param_mec2$ci_low[[2]], 
         ci_high = r_res_kill_param_mec2$ci_high[[2]], 
         main = "(B) Political Leader", ylab = "Killings for every 100.000 inhabitants", 
         xlab = "Years after election")
dev.off()

# Non-lethal selective violence. 
p1 <- c("lid_nl_vio", "lid_threat","fail_asis", "moe_kill_fail", "moe_pol_threat")
p2 <- c("moe_left_threat","moe_center_threat", "moe_right_threat",  
        "moe_left_kill_fail","moe_center_kill_fail",  "moe_right_kill_fail")

rnames <- c("Non-lethal violence against leaders", "Leader threat", "Leader failed killing", 
            "Political leader failed killing", "Political leader threat", 
            "Left-wing leader threat", "Center leader threat", "Right-wing threat", 
            "Left-wing leader failed killing", "Center leader failed killing", "Right-wing failed killing")
r_res_kill_param_mec3 <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                                 out_vars = c(paste0("pop_lead1234_",c(p1,p2))), controls_list = controls_list_r[-1], 
                                 running_var = X_it, subset_logic = T, 
                                 digits = 5, out_vars_names =rnames)

foot <- "Robust standard errors in parenthesis. *** p\<0.01, ** p\<0.05, * p\<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
base on the subsample of municipalities with paramilitary presence after 2016. Voilence outcomes
correspond to the number of victims in the four years following electoral year, normalized for every 100.000 municipality inhabitants.
Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015 and multidimensional 
poverty during 2005. Electoral years correspond to 2019 and 2015. Threat and failed-killing victims 
are normalized for every 100.000 inhabitants." %>% 
  str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")

tab_8 <- kbl(r_res_kill_param_mec3, booktabs = T, escape = F ,align = "c", format = "latex",
             longtable = F,  
             caption = "Effect of right-wing mayor election on non-lethal violence against social leaders", 
             label = "nl_vio_param") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Leaders", 1 ,12, bold = T) %>% 
  pack_rows("Political leaders", 13 ,20, bold = T) %>% 
  pack_rows("Political leaders by partisanship", 21, 44, bold = T) %>% 
  pack_rows("Specification Details", 45, 48, bold = T) %>% 
  footnote(general = foot,  threeparttable = T, escape = F)

writeLines(tab_8, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table8.tex")

# Collective violence. 
p1 <- c("collective_violence", "collective_threat", "displacement")
rnames <- c("Collective Violence Victims", "Collective Threats", "Displacement")

r_res_kill_param_mec4 <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                                   out_vars = c(paste0("pop_lead1234_",c(p1))), controls_list = controls_list_r[-1], 
                                   running_var = X_it, subset_logic = T, 
                                   digits = 5, out_vars_names =rnames)

foot <- "Robust standard errors in parenthesis. *** p<0.01, ** p<0.05, * p<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
base on the subsample of municipalities with paramilitary presence after 2016. 
Voilence outcomes correspond to the number of victims in the four years following electoral year, normalized for every 100.000 municipality inhabitants.Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015 and multidimensional 
poverty during 2005. Electoral years correspond to 2019 and 2015. Threat and failed-killing victims 
are normalized for every 100.000 inhabitants." %>% 
  str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")


tab_9 <- kbl(r_res_kill_param_mec4, booktabs = T, escape = F ,align = "c", format = "latex",
             longtable = F,  
             caption = "Effect of right-wing mayor election on collective violence", 
             label = "col_vio_param") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  footnote(general = foot,  threeparttable = T, escape = F)

writeLines(tab_9, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table9.tex")

# Indiscriminate violence and belic actions. 
p1 <- c("belic_action", "insurg_respon_ab", 
        "state_respon_ab" ,"tot_victims","mas_kill")

rnames <- c("Belic actions", "Insurgent belic actions", 
            "State belic actions", "Total victims belic actions", 
            "Massacre victims")

r_res_kill_param_mec5 <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1), 
                                   out_vars = c(paste0("pop_lead1234_",c(p1))), controls_list = controls_list_r[-1], 
                                   running_var = X_it, subset_logic = T, 
                                   digits = 5, out_vars_names =rnames)

foot <- "Robust standard errors in parenthesis. *** p<0.01, ** p<0.05, * p<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
base on the subsample of municipalities with paramilitary presence after 2016. 
Voilence outcomes correspond to the number of victims in the four years following electoral year, normalized for every 100.000 municipality inhabitants.Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015 and multidimensional 
poverty during 2005. Electoral years correspond to 2019 and 2015. Threat and failed-killing victims 
are normalized for every 100.000 inhabitants." %>% 
  str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")


tab_10 <- kbl(r_res_kill_param_mec5, booktabs = T, escape = F ,align = "c", format = "latex",
             longtable = F,  
             caption = "Effect of right-wing mayor election on indiscriminate violence", 
             label = "col_vio_param") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  footnote(general = foot,  threeparttable = T, escape = F)

writeLines(tab_10, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table10.tex")

# No paramilitary presence
rnames <- c("Social Leader", "Political Leader", 
            "Communal leader", "Peasent Leader", "Indigenous Leader")

tab_11 <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 0),
                            out_vars = panel_c_vars, 
                            controls_list = controls_list_r[-1], 
                            running_var = running_vars[1], subset_logic = T, 
                            digits = 5, out_vars_names = rnames)

foot <- "Robust standard errors in parenthesis. *** p<0.01, ** p<0.05, * p<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
base on the subsample of municipalities without paramilitary presence after 2016. 
Violence outcomes correspond to the number of victims in the four years following electoral year, normalized for every 100.000 municipality inhabitants.
Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015 and multidimensional 
poverty during 2005." %>% str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")

table_11 <- kbl(tab_11, booktabs = T, escape = F, align = "c", format = "latex",longtable = F,  label = "no_param",
             caption = "Effect of right-wing mayor election on social and political leader killings. Municipalities without paramilitary presence") %>% 
  add_header_above(c("","Linear Polynomial" = 2, "Quadratic Polynomial" = 2)) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Panel A Social and political leaders", 1, 8, bold = T) %>% 
  pack_rows("Panel B Disagregation of social leaders", 9, 20, bold = T) %>% 
  pack_rows("Specification details", 21, 24, bold = T) %>% 
  footnote(general = foot, threeparttable = T, escape = F
  )

writeLines(table_11, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table11.tex")

# Spending variables.
tab_12a <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1) %>% 
                      filter(V_payment_diff1 < quantile(V_payment_diff1, 0.95, na.rm = T)),
                    out_vars = "V_payment_diff1", 
                    controls_list = controls_list_r[-1], 
                    running_var = running_vars[1], subset_logic = T, 
                    digits = 5, out_vars_names = "Spending on armed conflict victims (Log change)")

tab_12 <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1 & year == 2019) %>% 
                      filter(V_ln_payments_lead1 > 0),
                    out_vars = c("V_ln_payments_lead1", "V_payment_diff1"), 
                    controls_list = controls_list_r[-1], 
                    running_var = running_vars[1], subset_logic = T, 
                    digits = 5, out_vars_names = c("Spending on armed conflict victims (Log)",
                                                   "Spending on armed conflict victims (Log change)"))
tab_12b <- tab_12b[-c(5:8),]
tab_12 <- rbind(tab_12b, tab_12a)

foot <- "Robust standard errors in parenthesis. *** p<0.01, ** p<0.05, * p<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
base on the subsample of municipalities without paramilitary presence after 2016. 
Violence outcomes correspond to the number of victims in the four years following electoral year, normalized for every 100.000 municipality inhabitants.
Geographical controls include distance variables to main economic centers, region dummies, 
altitude, and area. Socioeconomic controls contain dummy on historic presence of land conflicts, 
presence of indigenous population between 1535 and 1540, number of demobilized armed group members before 2015 and multidimensional 
poverty during 2005. Spending variables are deflated using the consumer price index, and trimmed at the 95th percentile
to avoid outliers driving the results." %>% str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")

table_12 <- kbl(tab_12, booktabs = T, escape = F, align = "c", format = "latex",longtable = F,  label = "spend_param",
                caption = "Effect of right-wing mayor election on social and political leader killings. Municipalities without paramilitary presence") %>% 
  add_header_above(c("","Linear Polynomial" = 2, "Quadratic Polynomial" = 2)) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Specification details", 9, 12, bold = T) %>% 
  footnote(general = foot, threeparttable = T, escape = F
  )

writeLines(table_12, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table12.tex")

# Spending variables. Fail when exclusing muns with 0 spending. Data issues.
tab_13 <- reg_tab_1(RD_baseline %>% filter(!is.na(running_vars[1]) & param_pres == 1) %>% 
                      filter(V_ln_payments_lead1 < quantile(V_ln_payments_lead1, 0.95, na.rm = T) &
                               V_ln_payments_lead1 > 0),
                     out_vars = c("V_payment_diff1", "V_ln_payments_lead1"), 
                     controls_list = controls_list_r[-1], 
                     running_var = running_vars[1], subset_logic = T, 
                     digits = 5, out_vars_names = c("V_payment_diff1", "V_ln_payments_lead1"))


# Covariate Balance full sample
temp_1 <- RD_baseline %>% filter(!is.na(running_vars[1]))
x_1 <- rdbwselect(temp$pop_lead1234_lid_comunal_sector, p = 1, covs = temp[controls_list_r[[3]]],x = temp$share_diff2_r)
bw <- x_1$bws[[4]]
##  
rnames <- c("Height", "Area (km2)", "Distance dpto capital", "Distance market", 
            "Distance Bogotá","Carebean region", "Pacific region", "Orinoquia region",
            "Amazon region","Indigenous population (1535-1540)", "Demobilized paramilitaries",
            "Demobilized guerrillas", "Demobilized combatants", "MPI", "UBN", "Population", 
            "Right-Wing incumbent", "Non-right-wing incumbent")

foot <- "Robust standard errors in parenthesis. *** p<0.01, ** p<0.05, * p<0.1. Calonico et al. (2014) data-driven bandwidth selection 
used in all regressions. Each coefficient corresponds to a different regression. Estimates are
based on the full sample of municipalities with close races." %>% 
  str_replace_all("[\r\n]" , "") %>% trimws() %>% str_replace_all("<", "$<$")

table_3 <- reg_tab(temp_1,out_vars = c(geo_controls, soc_controls, "population","nr_ideol_incum_r", "r_ideol_incum_r")[-c(10:13)], controls_list = list(nc = ""), 
                   running_var = running_vars[1], subset_logic =T, 
                   digits = 5, out_vars_names = c(rnames), h = c(bw,bw))

tab_3 <- kbl(table_3, booktabs = T, align = "c", format = "latex",longtable = F, 
             caption = "Covariate Balance Table. Full sample") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  footnote(general = foot)

writeLines(tab_3, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table_cov_bal_full.tex")



# Figures Descriptive Statistics ------------------------------------------
plot(density(RD_baseline %>% filter(V_ln_payments_lead1 > 0) %>% pull(V_ln_payments_lead1), na.rm = T))

## Baseline RDD results
temp <- RD_baseline
temp_1 <- RD_baseline %>% filter(year == 2019)

rd_commu_g1 <- rdplot(y = temp$pop_lead1234_lid_comunal_sector, x = temp$share_diff2_r, 
                      p = 2, binselect = "qs")$rdplot+ labs(x = "Share of votes for right-wing candidate", 
                                                            y = "Communal leader killing rate", 
                                                            title = "")


rd_commu_g2 <- rdplot(y = temp$pop_lead1234_lid_comunal_sector, x = temp$share_diff2_r,
                      p = 1, binselect = "qs")$rdplot+ labs(x = "Share of votes for right-wing candidate", 
                                                            y = "", 
                                                            title = "")

rd_commu_g3 <- rdplot(y = temp$ln_pop_lead1234_lid_comunal_sector, x = temp$share_diff2_r,
                      p = 2, binselect = "qs")$rdplot+ labs(x = "Share of votes for right-wing candidate", 
                                                            y = "Communal leader killing rate (log)", 
                                                            title = "")

rd_commu_g4 <- rdplot(y = temp$ln_pop_lead1234_lid_comunal_sector, 
                      x = temp$share_diff2_r, p = 1, binselect = "qs")$rdplot + labs(x = "Share of votes for right-wing candidate", 
                                                                                     y = "Communal leader killings rate (log)", 
                                                                                     title = "") 
rd_commu <- ggarrange(rd_commu_g1,rd_commu_g2,rd_commu_g3,rd_commu_g4,
                      labels = c("Quadratic", "Linear","Quadratic", "Linear"))

ggsave(filename = "figure9.pdf", plot = rd_commu, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")



## Main RDD results. 
temp <- RD_baseline %>% filter(param_pres == 1)
temp_1 <- RD_baseline %>% filter(param_pres == 1 & year == 2019)
rd_pol_g1 <- rdplot(y = temp$pop_lead1234_moe_pol_kill, x = temp$share_diff2_r,
       p = 2, binselect = "qs")$rdplot + labs(x = "Share of votes for right-wing candidate", 
                                              y = "Political leader killings rate", 
                                              title = "")
rd_pol_g2 <- rdplot(y = temp$pop_lead1234_moe_pol_kill, x = temp$share_diff2_r, 
                    p = 1, binselect = "qs",)$rdplot + labs(x = "Share of votes for right-wing candidate", 
                                                           y = "",
                                                           title = "")

rd_pol_g3 <- rdplot(y = temp$ln_pop_lead1234_moe_pol_kill, x = temp$share_diff2_r
                    ,p = 2, binselect = "qs")$rdplot + labs(x = "Share of votes for right-wing candidate", 
                                                            y = "Political leader killing rate(log)", 
                                                            title = "")

rd_pol_g4 <- rdplot(y = temp$ln_pop_lead1234_moe_pol_kill, 
                    x = temp$share_diff2_r, p = 1, binselect = "qs")$rdplot + labs(x = "Share of votes for right-wing candidate", 
                                                                                   y = "", 
                                                                                   title = "")
rd_pol <- ggarrange(rd_pol_g1,rd_pol_g2,rd_pol_g3,rd_pol_g4,
                    labels = c("Quadratic", "Linear","Quadratic", "Linear"))
  

ggsave(filename = "figure5.pdf", plot = rd_pol, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")
# Communal leaders

rd_commu_g1 <- rdplot(y = temp$pop_lead1234_lid_comunal_sector, x = temp$share_diff2_r, 
       p = 2, binselect = "qs")$rdplot+ labs(x = "Share of votes for right-wing candidate", 
                                             y = "Communal leader killing rate", 
                                             title = "")


rd_commu_g2 <- rdplot(y = temp$pop_lead1234_lid_comunal_sector, x = temp$share_diff2_r,
       p = 1, binselect = "qs")$rdplot+ labs(x = "Share of votes for right-wing candidate", 
                                             y = "", 
                                             title = "")

rd_commu_g3 <- rdplot(y = temp$ln_pop_lead1234_lid_comunal_sector, x = temp$share_diff2_r,
       p = 2, binselect = "qs")$rdplot+ labs(x = "Share of votes for right-wing candidate", 
                                             y = "Communal leader killing rate (log)", 
                                             title = "")

rd_commu_g4 <- rdplot(y = temp$ln_pop_lead1234_lid_comunal_sector, 
                      x = temp$share_diff2_r, p = 1, binselect = "qs")$rdplot + labs(x = "Share of votes for right-wing candidate", 
                                                                                     y = "Communal leader killings rate (log)", 
                                                                                     title = "") 
rd_commu <- ggarrange(rd_commu_g1,rd_commu_g2,rd_commu_g3,rd_commu_g4,
                    labels = c("Quadratic", "Linear","Quadratic", "Linear"))

ggsave(filename = "figure6.pdf", plot = rd_commu, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")


# 2019 communal
rdplot(y = temp_1$pop_lead1234_lid_comunal_sector, x = temp_1$share_diff2_r, p = 2, binselect = "qs")
rdplot(y = temp_1$pop_lead1234_lid_comunal_sector, x = temp_1$share_diff2_r, p = 1, binselect = "qs")

rdplot(y = temp_1$ln_pop_lead1234_lid_comunal_sector, x = temp_1$share_diff2_r, p = 1, binselect = "qs")
rdplot(y = temp_1$ln_pop_lead1234_lid_comunal_sector, x = temp_1$share_diff2_r, p = 2, binselect = "qs")
