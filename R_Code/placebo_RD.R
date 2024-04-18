################################################
# Explore covariate balance and placebo tests
################################################

# McCarry tests 
## Right Figure 1
MC_test_1 <- rddensity(RD_baseline[RD_baseline$year == 2019,]$share_diff2_r, c = 0)
est <- round(MC_test_1$test$t_jk, 4)
se <- round(MC_test_1$test$p_jk,4) 
h <- MC_test_1$h
N <- MC_test_1$N
Mc_plot_1 <- rdplotdensity(MC_test_1, RD_baseline[RD_baseline$year == 2019,]$share_diff2_r, plotGrid = "es", type = "both", 
              lcol = 1, hist = F, CItype = "line")
Pl_1 <- Mc_plot_1$Estplot +  
  labs(
       caption = paste0("Discontinuity estimate (estandard error) = ", est, " (", se, ").",
                        "N = ",N, ".")) + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10)),
        axis.title.x = element_text(size = 12),  # Adjust size of x-axis label
        axis.title.y = element_text(size = 12)) +  # Adjust size of y-axis label)
  xlab("Relative vote share for right wing candidate 2019") +
  ylab("Density")

ggsave(filename = "McCarry_r.pdf", plot = Pl_1, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")

## Left Figure 2
MC_test_2 <- rddensity(RD_baseline[RD_baseline$year == 2019,]$share_diff1_l, c = 0)
est <- round(MC_test_2$test$t_jk, 4)
se <- round(MC_test_2$test$p_jk,4) 
h <- MC_test_2$h
N <- MC_test_2$N
Mc_plot_2 <- rdplotdensity(MC_test_2, RD_baseline[RD_baseline$year == 2019,]$share_diff1_l, plotGrid = "es",
                          type = "both", CItype = "line",
                           lcol = 1, hist = F)
Pl_2 <- Mc_plot_2$Estplot +  
  labs(
       caption = paste0("Discontinuity estimate (estandard error) = ", est, " (", se, ").",
                        "N = ",N, ".")) + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10)),
        axis.title.x = element_text(size = 12),  # Adjust size of x-axis label
        axis.title.y = element_text(size = 12)) +  # Adjust size of y-axis label)
  xlab("Relative vote share for left wing candidate 2019") +
  ylab("Density")

ggsave(filename = "McCarry_l.pdf", plot = Pl_2, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")


# Covariate Balance

## Table 1. Use badnwidth from main right wing result resilts. 
rnames <- c("Height", "Area (km2)", "Distance dpto capital", "Distance market", 
            "Distance Bogotá", "Number of credits", 
            "Value credits", "Demobilized Paramilitaries", "Demobilized Guerrillas", 
            "MPI", "UBN","Different incumbent", "Same incumbent")
table_3a <- reg_tab(out_vars = c(geo_controls, soc_controls, "nr_ideol_incum_r", "r_ideol_incum_r")[-c(6:9,17, 12:14)], controls_list = list(nc = ""), 
                   running_var = running_vars[1], subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = c(rnames), h = c(bw,bw))
table_3b <- reg_tab(out_vars = c(geo_controls, soc_controls, "nl_ideol_incum_l", "l_ideol_incum_l")[-c(6:9,17, 12:14)], controls_list = list(nc = ""), 
                    running_var = running_vars[2], subset_logic = RD_baseline$year == 2019, 
                    digits = 5, out_vars_names = rnames, h = c(bw,bw))
table_3 <- cbind(table_3a, table_3b[,-c(1)])
table_3 <- table_3[-c(27,28),]
rownames(table_3) <- NULL


tab_3 <- kbl(table_3, booktabs = T, align = "c", format = "latex",longtable = F, 
             caption = "Covariate Balance Table") %>% 
  add_header_above(c("","Right-Wing" = 3, "Left-Wing" = 3)) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  footnote(general = "This table presents balance results across continuous covariates. Robust standard erros are used following (cite cattolino), across different specification varying polynomial degree.",
           number = c("Credit variables refer to the yearly-average number and value of credits acquired by small firms as defined and recorded by Agronet (cite CEDE) before 2015.", 
                      "Total number of demobilized soldiers before 2015, data from (CEDE).",
                      "MPI stands for the multimentional poverty index at 2005.", 
                      "UBN stands for unsatisfied basic needs measured at 2005.")
  )

writeLines(tab_3, "D:/Documents/GitHub/Thesis/Tables_tex/table3.tex")


# Pre-trends. 
lag1234_spending_vars <- c(spend_vars[str_detect(spend_vars, "lag1234")], "lag2_V1_payment_diff1", "lag2_V_payment_diff1")
lag1234_pop_conf <- paste0("pop_lag1234_",conflict_vars_rd)

# Right wing
rnames <- c(c("Overall spending (log-scale)", "Security spending (log-scale)", 
              "Security spending (% change)", "Overall spending (% change)"), 
            c("Killing", "Threat", "Failed killing", "Insurgent", 
              "Criminal Band", "State Forces", 
              "Community Leader", "Violent Act", "Leader killing (SD)", 
              "Belic Action"))

X_it <- running_vars[1]
table_4 <- reg_tab(out_vars =c(lag1234_spending_vars, lag1234_pop_conf), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)

tab_4 <- kbl(table_4, booktabs = T, align = "c", format = "latex",longtable = T, 
             caption = "Placebo test right-wing mayor") %>% 
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

# Left wing
rnames <-  c("Killing", "Threat", "Failed killing", "Insurgent", 
              "Criminal Band", "State Forces", 
              "Community Leader", "Violent Act", "Leader killing (SD)", 
              "Belic Action")
X_it <- running_vars[2]
table_5 <- reg_tab(out_vars =c(lag1_pop_conf), controls_list = controls_list, 
                   running_var = X_it, subset_logic = RD_baseline$year == 2019, 
                   digits = 5, out_vars_names = rnames)

tab_5 <- kbl(table_5, booktabs = T, align = "c", format = "latex",longtable = T, 
             caption = "Placebo test right-wing mayor") %>% 
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



