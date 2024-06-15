##################################3
# Graphs and plots
###################################

# Map -------------------------------

conflict_panel <- read.csv("D:/Documents/GitHub/Thesis/Data/Final_data/conflict_panel.csv")

conflict_panel <- conflict_panel %>% codmpio_clean()

g1_dat <- conflict_panel %>% filter(year %in% 2016:2023) %>% group_by(codmpio) %>% 
  summarise(lid= sum(lid_assas, na.rm = T))
g1_dat <- g1_dat %>% left_join_rep(pop %>%  filter(year %in% 2016:2023) %>% 
                                     group_by(codmpio) %>%summarise(population = mean(population)), 
                                   by = c("codmpio"))
g1_dat <- g1_dat %>% mutate(
  lid_pop = 100000*lid/population
)

# Add shape
m_shape <- read_sf("D:/Documents/GitHub/Thesis/Data/Shapes/mun_shape/mpio.shp")
m_shape <- m_shape %>% rename(codmpio = "MPIOS") %>% codmpio_clean()
g1_dat <- g1_dat %>% left_join_rep(m_shape, by = c("codmpio"))

Pl_2 <- ggplot() + 
  geom_sf(data = g1_dat$geometry, aes(fill = log(g1_dat$lid_pop +1), col = log(g1_dat$lid_pop + 1))) + 
  scale_fill_viridis_c(option = "plasma", direction = 1) + 
  scale_color_viridis(option = "plasma", direction = 1) + guides(fill="none") + 
  labs(color = "Log Leaders Killed") + 
  theme_map() + theme(legend.position = "bottom")

ggsave(filename = "Figure2.pdf", plot = Pl_2, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")

# Summary table.  ---------------------------------------------
sum_data <- RD_baseline %>% filter(!is.na(share_diff2_r)) %>% 
  mutate(V1_payments_lead1 = expm1(V1_ln_payments_lead1)/1000000, 
         V_payments_lead1 = expm1(V_ln_payments_lead1)/1000000)
sum_data <- sum_data  %>% mutate(race = 1:dim(sum_data)[1])
sum_data <- sum_data %>% select(IPM, population, ind_rural, 
                                pop_lead1234_collective_violence, pop_lead1234_non_leth_vio,
                                
                    pop_lead1234_lid_assas,pop_lead1234_lid_comunal_sector, pop_lead1234_lid_campesino_sector, pop_lead1234_lid_indigenas_sector,
                    pop_lead1234_killing_sd, pop_lead1234_comunal_sector_sd, pop_lead1234_campesino_sector_sd, pop_lead1234_indig_sector_sd,
                    pop_lead1234_belic_action, pop_lead1234_tot_victims, 
                    pop_lead1234_moe_pol_kill, pop_lead1234_moe_left_kill, pop_lead1234_moe_center_kill, pop_lead1234_moe_right_kill,
                    V_ln_payments_lead1, V1_ln_payments_lead1,
                    param_acts, insurg_acts, gov_acts, crimorg_acts,
                    param_pres, insurg_pres,
                    race, codmpio, ideologia_0_r)
sum_data$ideologia_0_r <- as.factor(sum_data$ideologia_0_r)

rnames <- c("Multiminesional Poverty Index", "Population", "Rurality index", 
            "Collective violence victims", "Non lethal violence",
            "Killed leaders",   "Killed communal leaders",   "Killed peasant leaders", "Killed indigenous leaders",  # Indepaz
            "Killed leaders",   "Killed communal leaders",   "Killed peasant leaders", "Killed indigenous leaders",  # SD
            "Belic actions", "Belic actions victims",
            "Killed political leaders","Killed left-wing leaders", "Killed center leaders", "Killed right-wing leaders",
            "Victim's Spending (Log)", "Victim's Security Spending (Log)", 
            "Paramilitary acts", "Insurgent acts", "State forces acts", "Criminal organizations acts",
            "Close races", "Municipalities",
            "Paramilitary presence", "n", "Insurgent presence", "n",
            "Left wing contender","Center Contentder", "Other contender")

tbl_sum <- summary_table(sum_data, cat_vars = c("ideologia_0_r", "param_pres", "insurg_pres"), 
                         cont_vars = colnames(sum_data)[1:25], 
                         count_vars = c("race", "codmpio"), 
                         digits = 3, rnames = rnames)
tbl_sum <- tbl_sum[-c(28,30),]



foot <- "This table presents summary statistics of the main variables.
The sample consists of 1153 right-wing close races at the municipality level, 885 of which have a
center contender and 139 a left-wing contender and 129 from non-cateogrized party." %>% str_replace_all("[\r\n]" , "") %>% trimws()

tab_sum <- kbl(tbl_sum, booktabs = T, align = "c", format = "html",longtable = F,  
             caption = "Summary Statistics") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Municipality characteristics", 1, 3, bold = T, escape = F) %>%
  pack_rows("Violence variables$^{1}$", 4, 18, bold = T, escape = F) %>%
  pack_rows("INDEPAZ social leaders", 6, 9, bold = T, escape = F) %>%
  pack_rows("CINEP social leaders", 10, 13, bold = T, escape = F) %>%
  pack_rows("Belic Actions", 14, 15, bold = T,escape = F) %>% 
  pack_rows("Political Violece", 16, 18, bold = T) %>% 
  pack_rows("Spending variables$^{2}$", 19, 20, bold = T) %>%
  pack_rows("Armed group presence$^{3}$", 21, 24, bold = T) %>%
  pack_rows("Race Details$^{4}$", 27, 31, bold = T) %>% 
  footnote(general = foot,
           number = c("All violence variables are normalized for every 100.000 municipality inhabitants.", 
                      "Spending variables are deflated using Colombia's CPI. Variables in changes, are specificed as the difference in logs between period of election and following period",
                      "Number of acts by violent acts between between 2016 and 2019 across armed groups.",
                      "textcite{Fergusson2020}'s party classification was used to code ideological afiliation."), threeparttable = T, escape = F
  )

writeLines(tab_sum, "D:/Documents/GitHub/Thesis/Tables_tex/table_Descript.tex")

# Summary table paramilitary subsample -----------------
sum_data <- RD_baseline %>% filter(!is.na(share_diff2_r) & param_pres == 1) %>% 
  mutate(V1_payments_lead1 = expm1(V1_ln_payments_lead1)/1000000, 
         V_payments_lead1 = expm1(V_ln_payments_lead1)/1000000)
sum_data <- sum_data %>% mutate(race = 1:dim(sum_data)[1])
sum_data <- sum_data %>% select(IPM, population, ind_rural, 
                                pop_lead1234_collective_violence, pop_lead1234_non_leth_vio,
                                
                                pop_lead1234_lid_assas,pop_lead1234_lid_comunal_sector, pop_lead1234_lid_campesino_sector, pop_lead1234_lid_indigenas_sector,
                                pop_lead1234_killing_sd, pop_lead1234_comunal_sector_sd, pop_lead1234_campesino_sector_sd, pop_lead1234_indig_sector_sd,
                                pop_lead1234_belic_action, pop_lead1234_tot_victims, 
                                pop_lead1234_moe_pol_kill, pop_lead1234_moe_left_kill, pop_lead1234_moe_center_kill, pop_lead1234_moe_right_kill,
                                V_ln_payments_lead1, V1_ln_payments_lead1,
                                param_acts, insurg_acts, gov_acts, crimorg_acts,
                                race, codmpio, ideologia_0_r)
sum_data$ideologia_0_r <- as.factor(sum_data$ideologia_0_r)

rnames <- c("Multiminesional Poverty Index", "Population", "Rurality index", 
            "Collective violence victims", "Non lethal violence",
            "Killed leaders",   "Killed communal leaders",   "Killed peasant leaders", "Killed indigenous leaders",  # Indepaz
            "Killed leaders",   "Killed communal leaders",   "Killed peasant leaders", "Killed indigenous leaders",  # SD
            "Belic actions", "Belic actions victims",
            "Killed political leaders","Killed left-wing leaders", "Killed center leaders", "Killed right-wing leaders",
            "Victim's Spending (Log)", "Victim's Security Spending (Log)", 
            "Paramilitary acts", "Insurgent acts", "State forces acts", "Criminal organizations acts",
            "Close races", "Municipalities",
            "Left wing contender","Center Contentder", "Other contender")

tbl_sum_param <- summary_table(sum_data, cat_vars = c("ideologia_0_r"), 
                         cont_vars = colnames(sum_data)[1:25], 
                         count_vars = c("race", "codmpio"), 
                         digits = 3, rnames = rnames)

foot <- "This table presents summary statistics of the main variables at close-race municipalities with paramilitary presence.
The sample consists of 271 right-wing close races across 194 municipalities, 41 of which have a
center contender and 198 a left-wing contender and 32 from non-cateogrized party." %>% str_replace_all("[\r\n]" , "") %>% trimws()
tab_sum_param <- kbl(tbl_sum_param, booktabs = T, align = "c", format = "latex",longtable = F,  
               caption = "Summary Statistics Paramilitary Presence Subsample") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  column_spec(1, width = "4cm") %>% 
  pack_rows("Municipality characteristics", 1, 3, bold = T, escape = F) %>%
  pack_rows("Violence variables$^{1}$", 4, 19, bold = T, escape = F) %>%
  pack_rows("INDEPAZ social leaders", 6, 9, bold = T, escape = F) %>%
  pack_rows("CINEP social leaders", 10, 13, bold = T, escape = F) %>%
  pack_rows("Belic Actions", 14, 15, bold = T,escape = F) %>% 
  pack_rows("Political Violece", 16, 19, bold = T) %>% 
  pack_rows("Spending variables$^{2}$", 20, 21, bold = T) %>%
  pack_rows("Armed group presence$^{3}$", 22, 25, bold = T) %>%
  pack_rows("Race Details$^{4}$", 26, 30, bold = T) %>% 
  footnote(general = foot, 
           number = c("All violence variables are normalized for every 100.000 municipality inhabitants.", 
                      "Spending variables are deflated using Colombia's CPI. Variables in changes, are specificed as the difference in logs between period of election and following period",
                      "Number of acts by violent acts between between 2016 and 2019 across armed groups.",
                      "textcite{Fergusson2020}'s party classification was used to code ideological afiliation."), 
           threeparttable = T, escape = F
  )

writeLines(tab_sum_param, "D:/Documents/GitHub/Thesis/Tables_tex/Final/table_Descript_param.tex")


# Time trends -----------------------------------------

g1_dat <- conflict_panel %>% filter(year %in% 2017:2023) %>% 
  select(year, lid_assas, codmpio) %>% group_by(year) %>% 
  summarise(lid_assas = sum(lid_assas, na.rm = T)) %>% ungroup()

pl_1 <- ggplot() + geom_line(aes(x = year, y = lid_assas), data = g1_dat) + 
  ylab("Leader Killings") + xlab("Year") + scale_colour_grey() + theme_bw() 

ggsave(filename = "Figure1.pdf", plot = pl_1, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")

# Somos defensores

g1_dat <- conflict_panel %>% filter(year %in% 2000:2023) %>% group_by(year) %>% 
  summarise(lid_assas = sum(killing_sd, na.rm = T)) %>% ungroup()

pl_1 <- ggplot() + geom_line(aes(x = year, y = lid_assas), data = g1_dat) + 
  ylab("Leader Killings") + xlab("Year") + scale_colour_grey() + theme_bw() 

ggsave(filename = "Figure1SD.pdf", plot = pl_1, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")

# Yearly composition of responsables. 

g1_dat <- conflict_panel %>% filter(year %in% 2012:2023) %>% 
  select(year, codmpio, lid_assas, lid_comunal_sector, lid_campesino_sector, 
         lid_indigenas_sector, lid_afrodesendiente_sector, 
         lid_ambiental_sector, lid_victimas_sector, lid_pnis_sector) %>% group_by(year) %>% 
  summarise(across(starts_with("lid"), ~ sum(.x, na.rm = T))) %>% ungroup() %>% 
  mutate(across(4:11, ~ .x/lid_assas)) %>% select(-lid_assas) %>% 
  pivot_longer(cols = 3:10,names_to = "respon",  values_to = "respon_prop")

# Time trends across incumbency groups
temp <- elect_right %>% filter(year == 2019)
conflict_panel <- read.csv("~/GitHub/Thesis/Data/Final_data/conflict_panel.csv") %>% 
  codmpio_clean()
g1_dat <- conflict_panel %>% filter(year %in% 2012:2023) %>% 
  left_join_rep(temp %>% select(-year), by = c("codmpio"))
#dim(g1_dat)
g1_dat <- g1_dat |>
  zap_labels() |>
  zap_formats()

# Elections with only one right-wing in top two candidates
g1_dat <- g1_dat %>% filter(!is.na(share_diff2_r))
g1 <- g1_dat %>% group_by(year,ideol_incum_r) %>% summarize(lid_assas = sum(lid_asis, na.rm = T)) %>% 
  filter(!is.na(ideol_incum_r)) %>% mutate(ideol_incum_r = factor(ideol_incum_r))
ggplot(g1) + geom_line(aes(x = year, y =lid_assas, color = ideol_incum_r)) 

# Time trends across winning and losing. 
g2 <- g1_dat %>% filter(!is.na(share_diff2_r)) %>% 
  mutate(Tr = as.integer(share_diff2_r > 0)) %>%  
  group_by(year,Tr, ideol_incum_r) %>% summarize(lid_asis = sum(lid_assas, na.rm = T)) %>% 
  mutate(ideol_incum_r = factor(ideol_incum_r), 
         Tr = factor(Tr), 
         ideol_Tr = factor(paste0(ideol_incum_r,"_",Tr)))
  
ggplot(g2 %>% filter(ideol_incum_r == 3)) + 
  geom_line(aes(x = year, y =lid_asis, color = Tr)) + ylab("Leader Killings") + xlab("Year") + 
  scale_colour_grey() + theme_bw() + geom_vline(xintercept = 2019, linetype = "dashed")

ggplot(g2 %>% filter(ideol_incum_r == 2)) + 
  geom_line(aes(x = year, y =lid_asis, color = Tr)) + ylab("Leader Killings") + xlab("Year") + 
  scale_colour_grey() + theme_bw() + geom_vline(xintercept = 2019, linetype = "dashed")

ggplot(g2 %>% filter(ideol_incum_r == 1)) + 
  geom_line(aes(x = year, y =lid_asis, color = Tr)) + ylab("Leader Killings") + xlab("Year") + 
  scale_colour_grey() + theme_bw() + geom_vline(xintercept = 2019, linetype = "dashed")

ggplot(g2 %>% filter(ideol_incum_r == 4)) + 
  geom_line(aes(x = year, y =lid_asis, color = Tr)) + ylab("Leader Killings") + xlab("Year") + 
  scale_colour_grey() + theme_bw() + geom_vline(xintercept = 2019, linetype = "dashed")

ggplot(g2 %>% group_by(year, Tr) %>% summarise(lid_asis = sum(lid_asis))) + 
  geom_line(aes(x = year, y =lid_asis, color = Tr)) + ylab("Leader Killings") + xlab("Year") + 
  scale_colour_grey() + theme_bw() + geom_vline(xintercept = 2019, linetype = "dashed")

# Comunal sector
g2 <- g1_dat %>% mutate(Tr = as.integer(share_diff2_r > 0)) %>%  
  group_by(year,Tr, ideol_incum_r) %>% summarize(com_sect = sum(pop_lead1234_lid_comunal_sector, na.rm = T)) %>% 
  filter(!is.na(ideol_incum_r)) %>% mutate(ideol_incum_r = factor(ideol_incum_r), 
                                           Tr = factor(Tr), 
                                           ideol_Tr = factor(paste0(ideol_incum_r,"_",Tr)))

ggplot(g2) + geom_line(aes(x = year, y =com_sect, group = ideol_Tr, color = Tr,
                           linetype = ideol_incum_r))


# Time treands across treatment only
g2 <- g1_dat %>% mutate(Tr = as.integer(share_diff2_r > 0)) %>%  
  group_by(year,Tr) %>% summarize(lid_assas = sum(lid_assas, na.rm = T)) %>% 
  mutate(Tr = factor(Tr))

ggplot(g2) + geom_line(aes(x = year, y =lid_assas, color = Tr)) 

# Monthly. 
g3 <- expand.grid(years, m_shape$codmpio)
colnames(g3) <- c("year", "codmpio")
g3 <- g3 %>% left_join_rep(
  elect_right %>% filter(!is.na(share_diff2_r) & year == 2019) %>% select(-year), 
                           by = "codmpio")
l <- lideres %>% group_by(year, month, codmpio) %>% 
  summarise(lid_assas = sum(lid_assas, na.rm = T)) %>% 
  left_join_rep(g3, by = c("codmpio", "year"))

l <- l %>% filter(!is.na(share_diff2_r)) %>% 
  mutate(Tr = factor(as.integer(share_diff2_r > 0))) %>% 
  group_by(year, month, Tr) %>% summarise(lid_assas = sum(lid_assas, na.rm = T)) 

l <- l %>% mutate(date = ym(paste0(year, "-", month)))
ggplot(l) + geom_line(aes(x = date, y =lid_assas, color = Tr))


# Somos defensores -------------------------------------------
g1 <- g1_dat %>% group_by(year,ideol_incum_r) %>% summarize(lid_assas = sum(killing_sd, na.rm = T)) %>% 
  filter(!is.na(ideol_incum_r)) %>% mutate(ideol_incum_r = factor(ideol_incum_r))
ggplot(g1) + geom_line(aes(x = year, y =lid_assas, color = ideol_incum_r)) 

# Time trends across winning and losing. 
g2 <- g1_dat %>% filter(!is.na(share_diff2_r)) %>% 
  mutate(Tr = as.integer(share_diff2_r > 0)) %>%  
  group_by(year,Tr, ideol_incum_r) %>% summarize(lid_asis = sum(killing_sd, na.rm = T)) %>% 
  mutate(ideol_incum_r = factor(ideol_incum_r), 
         Tr = factor(Tr), 
         ideol_Tr = factor(paste0(ideol_incum_r,"_",Tr)))

ggplot(g2 %>% filter(ideol_incum_r == 3)) + 
  geom_line(aes(x = year, y =lid_asis, color = Tr)) + ylab("Leader Killings") + xlab("Year") + 
  scale_colour_grey() + theme_bw() + geom_vline(xintercept = 2019, linetype = "dashed")

# Losing right wing municipalities experience more killings 3 years after.  
ggplot(g2 %>% filter(ideol_incum_r == 2)) + 
  geom_line(aes(x = year, y =lid_asis, color = Tr)) + ylab("Leader Killings") + xlab("Year") + 
  scale_colour_grey() + theme_bw() + geom_vline(xintercept = 2019, linetype = "dashed")

# Very small sample size. Winning experience much more violence
ggplot(g2 %>% filter(ideol_incum_r == 1)) + 
  geom_line(aes(x = year, y =lid_asis, color = Tr)) + ylab("Leader Killings") + xlab("Year") + 
  scale_colour_grey() + theme_bw() + geom_vline(xintercept = 2019, linetype = "dashed")
# No clear trend
ggplot(g2 %>% filter(ideol_incum_r == 4)) + 
  geom_line(aes(x = year, y =lid_asis, color = Tr)) + ylab("Leader Killings") + xlab("Year") + 
  scale_colour_grey() + theme_bw() + geom_vline(xintercept = 2019, linetype = "dashed")
# Agregate. No clear effect. 
ggplot(g2 %>% group_by(year, Tr) %>% summarise(lid_asis = sum(lid_asis))) + 
  geom_line(aes(x = year, y =lid_asis, color = Tr)) + ylab("Leader Killings") + xlab("Year") + 
  scale_colour_grey() + theme_bw() + geom_vline(xintercept = 2019, linetype = "dashed")


# DiD subsample 2. -------------------------------------

g_trend <- function(data_did, var, treat, g_var){
  temp <- data_did  %>% group_by(year,.data[[treat]], .data[[g_var]])  %>% 
    summarise(outcome = sum(.data[[var]], na.rm = T)) 
  g <- ggplot(temp) + 
    geom_line(aes(x = year, y =outcome, color = factor(.data[[treat]]), linetype = factor(.data[[g_var]]))) + 
    ylab("Outcome") + xlab("Year") + theme_bw() + geom_vline(xintercept = 2019, linetype = "dashed")
  print(g)
  return(g)
}
did_baseline_2 <- did_baseline_2 %>% mutate(b_group = "1")

# Right treat 
g1 <- g_trend(data_did = did_baseline_2, var = "comunal_sector_sd", 
              treat = "treat_r", g_var = "b_group")
# Right treat incum center. 
g2 <- g_trend(data_did = did_baseline_2, var = "comunal_sector_sd", 
              treat = "treat_r", g_var = "incum_c")
# Right treat incum right 
g3 <- g_trend(data_did = did_baseline_2, var = "comunal_sector_sd", 
              treat = "treat_r", g_var = "incum_r")
# Right treat incum left. Try DiD when left incumbent. *** 
g4 <- g_trend(data_did = did_baseline_2, var = "comunal_sector_sd", 
              treat = "treat_r", g_var = "incum_l")

## Time trend in socia leader killings vs general violence. 
h <- read.csv("D:/Documents/GitHub/Thesis/Data/other/homic_colombia.csv", header = F)
colnames(h) <- h[3,]
h <- h[49,]
h <- h %>% pivot_longer(cols = c(5:68), names_to = "year", values_to = "homicide_rate"
)
h <- h %>% select(year, homicide_rate) %>% filter(!is.na(homicide_rate))
h$year <- as.numeric(h$year)
pop_year <- pop %>% group_by(year) %>% summarise(pop = sum(population ,na.rm = T))
graphs_data <- conflict_panel %>% select(codmpio, year, lid_assas, killing_sd, 
                                         lid_comunal_sector, lid_campesino_sector, 
                                         lid_afrodesendiente_sector, comunal_sector_sd, 
                                         afro_sector_sd, campesino_sector_sd)

g1_dat <- graphs_data %>% select(killing_sd, year, codmpio) %>% group_by(year) %>% 
  summarise(killing_sd = sum(killing_sd, na.rm = T)) %>% left_join_rep(h, by = c("year")) %>% 
  filter(year < 2023) %>% left_join(pop_year, by = c("year"))


g1_dat$homicide_rate <- as.numeric(g1_dat$homicide_rate)
g1_dat <- g1_dat %>% mutate(pop_killing_sd = 100000*killing_sd/pop)
scale = 0.01
g1_plot <- ggplot(g1_dat) +
  geom_line(aes(x = year, y = pop_killing_sd, color = "Social Leader Killings Rate (CINEP)")) +
  geom_line(aes(x = year, y = homicide_rate*scale, color = "Homicide Rate (World Bank)")) + # Removed * scale here
  scale_y_continuous(sec.axis = sec_axis(~ ./scale, name = "Homicide Rate (World Bank)")) +
  scale_x_continuous(breaks = seq(min(g1_dat$year), max(g1_dat$year), by = 1)) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  labs(y = "Social Leader Killings Rate (CINEP)") +  # Add y-axis label for primary axis
  theme(legend.position = "bottom") +   # Adjust legend position if needed
  guides(color = guide_legend(title = NULL))

ggsave(filename = "Figure1.pdf", plot = g1_plot, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")

# Development across sectors.
g2_dat <- graphs_data %>% select(killing_sd, comunal_sector_sd, 
                                 afro_sector_sd, campesino_sector_sd, year) %>% 
  group_by(year) %>% summarise(across(everything(), ~ sum(.x, na.rm = T)))
g2_dat <- g2_dat %>% filter(year < 2023) %>% left_join(pop_year, by = c("year")) %>% 
  mutate(across(-year, ~ 100000*.x/pop))
g2_dat <- g2_dat %>% pivot_longer(cols = c("comunal_sector_sd", "afro_sector_sd", "campesino_sector_sd"),
                                  values_to = "killing_subpop", names_to = "type")
my_colors <- c("Blue" = "#1f77b4", "Green" = "#2ca02c", "Red" = "#d62728", "Purple" = "#9467bd")
g2_plot <- ggplot(g2_dat, aes(x = year)) +  
  geom_line(aes(y = killing_subpop, color = type)) + 
  geom_line(aes(y = killing_sd, color = "Total Killings")) +
  labs(y = "Social Leader Killings Rate (CINEP)", x = "Year") +
  scale_x_continuous(breaks = seq(min(g2_dat$year), max(g2_dat$year), by = 1)) +  # Set x-axis ticks to whole numbers of years
  scale_color_manual(values = c("#1f77b4", "#2ca02c", "#d62728", "black"), 
                     labels = c("Afrocolombian Leader", "Peasant Leader", "Communal Leader", "Total"),  # Change legend labels
                     name = "Leader Type")  # Change legend title
  
ggsave(filename = "Figure3.pdf", plot = g2_plot, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")

# Compare CINEP and INDEPAZ.
# Development across sectors.
g3_dat <- graphs_data %>% select(killing_sd, lid_assas,year) %>% 
  group_by(year) %>% summarise(across(everything(), ~ sum(.x, na.rm = T)))
g3_dat <- g2_dat %>% filter(year < 2023) %>% mutate(lid_assas = case_when(
  year <= 2015 ~ NA,
  T ~ lid_assas
))

g3_plot <- ggplot(g3_dat, aes(x = year)) +  
  geom_line(aes(y = killing_sd, color = "CINEP")) + 
  geom_line(aes(y = lid_assas, color = "INDEPAZ")) +
  labs(y = "Social Leader Killings", x = "Year") +
  scale_x_continuous(breaks = seq(min(g2_dat$year), max(g2_dat$year), by = 1)) +  # Set x-axis ticks to whole numbers of years
  scale_color_manual(values = c("#1f77b4", "#2ca02c"), 
                     labels = c("CINEP", "INDEPAZ"),  # Change legend labels
                     name = "Source")  # Change legend title

ggsave(filename = "Figure4.pdf", plot = g3_plot, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 8, height = 6, units = "in")
