##################################################
# Event study design: Political violence Colombia  
# Author: Miguel Gonzalez Lugo
###################################################

# Using data on close races --> Get similar municipalities to RDD.  -----------------
# Comunity leaders. 
est_1 <- feols(pop_lid_comunal_sector~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline
)

est_2 <- feols(pop_comunal_sector_sd~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline
)
iplot(list(ip = est_1, sd = est_2))
# Community leaders intensive margin. 
est_1 <- feols(pop_lid_comunal_sector~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline, 
               subset = did_baseline$pop_lid_comunal_sector > 0
)

est_2 <- feols(pop_comunal_sector_sd~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline,
               subset =did_baseline$pop_lid_comunal_sector > 0
)
iplot(list(ip = est_1, sd = est_2))


# Community leaders PDET.
est_1 <- feols(pop_lid_comunal_sector~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline, 
               subset = did_baseline$pdet == 1 &
                 did_baseline$ideol_incum_r == 3
)

est_2 <- feols(pop_comunal_sector_sd~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline,
               subset = did_baseline$pdet == 1 &
                 did_baseline$ideol_incum_r == 3
)
iplot(list(ip = est_1, sd = est_2))

# Use RDD badwidth.
x <- rdbwselect(RD_baseline$lead3_pop_lid_comunal_sector, x = RD_baseline$share_diff2_r, 
                covs =RD_baseline[controls_list_r[[3]]], subset = RD_baseline$year == 2019 & RD_baseline$pdet == 1)
h <- x$bws[[4]]
est_1 <- feols(pop_lid_comunal_sector~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline, 
               subset = did_baseline$share_diff2_r < h & 
                 did_baseline$share_diff2_r > -h
)

est_2 <- feols(pop_comunal_sector_sd~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline,
               subset = did_baseline$share_diff2_r < h & 
                 did_baseline$share_diff2_r > -h
)
iplot(list(ip = est_1, sd = est_2))
est_1$nobs/8 # 39 municipalities.
# Margins communal leadersv
est_1 <- feols(pop_lid_comunal_sector~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline, 
               subset = did_baseline$lid_comunal_sector > 0
)

est_2 <- feols(pop_comunal_sector_sd~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline,
               subset = did_baseline$comunal_sector_sd > 0
)
iplot(list(ip = est_1, sd = est_2))
# 77 municipalities. 
est_1$fixef_sizes
est_2$fixef_sizes
# Extensive margin
# Community leaders PDET.

est_1 <- feols(pop_killing_sd_dum~ i(time, Tr, ref = -1)
                 |
                 codmpio + year , 
               cluster = ~codmpio,
               data = did_baseline
                 )

est_2 <- feols(pop_lid_asis_dum~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline
)
f11 <- list(est_1, est_2)
iplot(f11)

# Incumebncy of center --> Possitive effect on first two years.
# **** finding. Robustness to RDD. 
est_1 <- feols(pop_killing_sd_dum~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline, 
               subset = did_baseline$ideol_incum_r != 2
)

est_2 <- feols(pop_lid_asis_dum~ i(time, Tr, ref = -1)
               |
                 codmpio + year , 
               cluster = ~codmpio,
               data = did_baseline,
               subset = did_baseline$ideol_incum_r != 2
)
f12 <- list(est_1, est_2)
iplot(f12)
est_1$fixef_sizes # 323 mpios
# Restrict to bandwidth RDD. Intensive
bw_RDD <- rdbwselect(RD_baseline$pop_lead1234_killing_sd, 
                     x = RD_baseline$share_diff2_r)
est_1 <- feols(pop_killing_sd_dum~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline, 
               subset = did_baseline$ideol_incum_r != 2&
                 did_baseline$share_diff2_r < bw_RDD$bws[[4]] &
                 did_baseline$share_diff2_r > -bw_RDD$bws[[4]]
)

est_2 <- feols(pop_lid_asis_dum~ i(time, Tr, ref = -1)
               |
                 codmpio + year , 
               cluster = ~codmpio,
               data = did_baseline,
               subset = did_baseline$ideol_incum_r != 2 &
                 did_baseline$share_diff2_r < bw_RDD$bws[[4]] &
                 did_baseline$share_diff2_r > -bw_RDD$bws[[4]] 
)
f13 <- list(est_1, est_2)
iplot(f13)
est_1$fixef_sizes # 209 municipalities across 8 years.

# Left-wing. High anticipation effects.
est_1 <- feols(pop_killing_sd~ i(time, Tr, ref = 0)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline, 
               subset = did_baseline$ideol_incum_r == 1&
                 did_baseline$share_diff2_r < bw_RDD$bws[[4]] &
                 did_baseline$share_diff2_r > -bw_RDD$bws[[4]]
)

est_2 <- feols(pop_lid_asis~ i(time, Tr, ref = 0)
               |
                 codmpio + year , 
               cluster = ~codmpio,
               data = did_baseline,
               subset = did_baseline$ideol_incum_r == 1 &
                 did_baseline$share_diff2_r < bw_RDD$bws[[4]] &
                 did_baseline$share_diff2_r > -bw_RDD$bws[[4]] 
)
f13 <- list(est_1, est_2)
iplot(f13)

# Incumbency right. No effects and lower killings.  
# ********* 
est_1 <- feols(pop_killing_sd_dum~ i(time, Tr, ref = -1)
               |
                 codmpio + year , 
               cluster = ~codmpio,
               data = did_baseline, 
               subset = did_baseline$ideol_incum_r == 2
)

est_2 <- feols(pop_lid_asis_dum~ i(time, Tr, ref = -1)
               |
                 codmpio + year , 
               cluster = ~codmpio,
               data = did_baseline,
               subset = did_baseline$ideol_incum_r == 2
)
f12 <- list(est_1, est_2)
iplot(f12)

# Higher non-lethal violence on the first term 
# when incumbent non-right. *****
est_3 <- feols(pop_lid_nl_vio~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline, 
               subset = did_baseline$ideol_incum_r != 2
)
iplot(est_3)

# Armed groups presence. No effects. 
est_1 <- feols(pop_killing_sd~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline, 
               subset = did_baseline$insurg_pres == 1
)

est_2 <- feols(pop_lid_asis~ i(time, Tr, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline,
               subset = did_baseline$insurg_pres == 1
)
iplot(list(ip = est_1, sd = est_2))
est_1$fixef_sizes
## Synthetic DID ------------------------------
x <- did_baseline %>% filter(!is.na(share_diff2_r)) %>% 
  mutate(year = ymd(paste0(year, "-01-01"))) %>% as.data.frame()
setup = panel.matrices(x, unit = "codmpio", time = "year", outcome = "pop_lid_asis", 
                       treatment = "d")
table(x$d, x$year)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
print(summary(tau.hat))
se = sqrt(vcov(tau.hat, method='bootstrap'))
plot(tau.hat, overlay = 1,  se.method='bootstrap')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
synthdid_plot(estimates, se.method='bootstrap')

## Monthly data.  -------------------------------------
x <- did_baseline_month%>% as.data.frame() %>% filter(!is.na(share_diff2_r))
setup = panel.matrices(x, unit = "codmpio", time = "date", outcome = "lid_assas_dum", 
                       treatment = "d")

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
print(summary(tau.hat))
se = sqrt(vcov(tau.hat, method='placebo'))
plot(tau.hat, overlay = 1,  se.method='placebo')

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
synthdid_plot(estimates, se.method='bootstrap')

# TWFE. Using all municipalitites.--------------------

# Potential issue: Control are municipalities with other parties elected.
# Might not be the best control group --> Use municipalties who didn't chage 
# party a control. 

## Leaders --------------------------------------------

#Base. Lower killings in the second year of term.
est_1 <- feols(pop_lid_assas_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)

est_2 <- feols(pop_killing_sd_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
f1 <- list(ip = est_1, sd = est_2)
iplot(f1)
### Community leaders. ------------------------

est_1 <- feols(pop_lid_comunal_sector~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)

est_2 <- feols(pop_comunal_sector_sd~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(list(ip = est_1, sd = est_2))

#### Community leaders subset of pdet municipalities. PDET
est_1 <- feols(pop_lid_comunal_sector~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset = did_baseline_2$pdet == 1
)

est_2 <- feols(pop_comunal_sector_sd~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$pdet == 1
)
iplot(list(ip = est_1, sd = est_2))
est_1$nobs/8 # 166 municipalities. 

#### Community leaders. Intensive margin.  
est_1 <- feols(pop_lid_comunal_sector~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset = did_baseline_2$pop_lid_comunal_sector > 0
)

est_2 <- feols(pop_comunal_sector_sd~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$pop_lid_comunal_sector > 0
)
iplot(list(ip = est_1, sd = est_2))
est_1$fixef_sizes # 176 municipalities 

# Community leaders armed group presence. 
# No effects insurgency presence
est_1 <- feols(pop_lid_comunal_sector~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset = did_baseline_2$insurg_pres == 1
)

est_2 <- feols(pop_comunal_sector_sd~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$insurg_pres == 1
)
iplot(list(ip = est_1, sd = est_2))
# Paramilitary presence. 
mod =
  etwfe(
    fml  = killing_sd ~ 0,
    tvar = time, 
    tref = -1,
    gvar = treat_r, 
    data = did_baseline_2, 
    ivar = codmpio,
    xvar = param_pres,
    vcov = ~codmpio)


est_1 <- feols(pop_lid_comunal_sector~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset = did_baseline_2$param_pres == 1
)

est_2 <- feols(pop_comunal_sector_sd~ i(time, treat_r, ref = -1) +
                 i(time, param_pres,treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(est_2)
iplot(list(ip = est_1, sd = est_2))
# No effects bacrim presence. 
est_1 <- feols(pop_lid_comunal_sector~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset = did_baseline_2$crimorg_pres == 1
)

est_2 <- feols(pop_comunal_sector_sd~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$crimorg_pres == 1
)
iplot(list(ip = est_1, sd = est_2))




### Indigenous sector.--------------------------

est_1 <- feols(pop_lid_indigenas_sector_dum~ i(time, treat_r, ref = 0)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)

est_2 <- feols(pop_indig_sector_sd_dum~ i(time, treat_r, ref = 0)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(list(ip = est_1, sd = est_2))

### Campesinos--------------------------------

est_1 <- feols(pop_lid_campesino_sector_dum~ i(time, treat_r, ref = 0)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)

est_2 <- feols(pop_campesino_sector_sd_dum~ i(time, treat_r, ref = 0)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(list(ip = est_1, sd = est_2))

## Incumbent heterogeneity --------------------------
### left-wing incumbent ------------------------------
# Left wing incumbent. No effect, if something there's a reduction, and increase in
# the last period of the term. 

est_1 <- feols(pop_lid_assas_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset = did_baseline_2$incum_l == 1
)

est_2 <- feols(pop_killing_sd_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$incum_l == 1
)
iplot(list(ip = est_1, sd = est_2))

### Center incumbent. ------------------------
# Differences across measuremnts. SD show 
# an increase in killings in the last electoral period.
est_1 <- feols(pop_lid_assas_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset = did_baseline_2$incum_c == 1
)

est_2 <- feols(pop_killing_sd_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$incum_c == 1
)
iplot(list(ip = est_1, sd = est_2))
# Community leaders. 
est_1 <- feols(pop_lid_comunal_sector_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset = did_baseline_2$incum_c == 1
)

est_2 <- feols(pop_comunal_sector_sd_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$incum_c == 1
)
iplot(list(ip = est_1, sd = est_2))

## Community leaders. intensive margin
est_1 <- feols(pop_lid_comunal_sector_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset = did_baseline_2$incum_c == 1 &
                 did_baseline_2$pop_lid_comunal_sector_dum
)

est_2 <- feols(pop_comunal_sector_sd_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$incum_c == 1
)
iplot(list(ip = est_1, sd = est_2))

### Right incumbent -----------------------------------
# Significant reduction in killings *** 
# Important finding. 
est_1 <- feols(pop_lid_assas_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset = did_baseline_2$incum_r == 1
)

est_2 <- feols(pop_killing_sd_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$incum_r == 1
)
f2 <- list(ip = est_1, sd = est_2)
iplot(f2)

## Political violence -----------------------

# Assasination. Increase in first period. 
est_1 <- feols(pop_asis_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(est_1)

### Collective violence. -----------------
#Slight increase after elections. Finding ***************
est_1 <- feols(pop_collective_violence~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
f2 <- est_1
iplot(f2)

### Non-lethal violence. ------------------------------ 
#No effect.
est_1 <- feols(pop_non_leth_vio~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(est_1)

### Threats -----------------------------------
#Important finding. ***********
# Higher individual threats after election, not higher collective threats.
est_1 <- feols(pop_ind_threat_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)

### Collective threat.  ------------------
#Important finding. ***********
# No higher collective threats. 
est_2 <- feols(pop_collective_threat_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
f3 <- list(est_1, est_2)
iplot(f3)

## Pol vio Subpopulations -----------------------

### leader sector ------------------------------
est_1 <- feols(pop_lider_sector_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(est_1)
### Campesino ---------------------------------
#
est_1 <- feols(pop_campesino_sector_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(est_1)
### Indigenous.------------------------- 
# Some anticipation effects.
est_1 <- feols(pop_indig_sector_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(est_1)

## MOE ---------------------------------
### Political violence ------------------------
# Some effects on second term, some anticipation effects.
est_1 <- feols(pop_moe_vio_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(est_1)
### Moe killing. ---------------------- 
# No effects.
est_1 <- feols(pop_moe_kill_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(est_1)

### Moe threat. ------------------------- 
# Very high anticipation effects.
# Look at incumbency differences
est_1 <- feols(pop_moe_threat_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
iplot(est_1)
### Moe threats Incumbency -------------------------
# High anticipation when right wing incumbent. 
est_1 <- feols(pop_moe_threat_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$incum_r == 1
)
iplot(est_1)

# Would left wing incumbent get rid of this anticipation effect?
# Yes but then there are no effects. 
est_1 <- feols(pop_moe_threat~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$incum_l == 1
)
iplot(est_1)
# Center incumbency. High anticipation effects. 
est_1 <- feols(pop_moe_threat~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$incum_c == 1
)
iplot(est_1)

# left wing violence. No effects.
est_1 <- feols(pop_moe_nright_kill_dum~ i(time, treat_r, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$incum_l == 1
)
iplot(est_1)


################################################
# Use parties that didn't chage party as control
# Sample 2, improve control group. ----------------------------------
# Define control municipalities as municipalities who didn't change 
# ideology. 
did_baseline_2 <- did_baseline_2 %>% mutate(
  treat_r_1 = case_when(
    treat_r == 1 & incum_r == 0 ~ 1, # Treat when party changes to right. 
    treat_r == 0 & (ideol_incum == ideologia) ~ 0, 
    # Control when party does not change and non-right.
    T ~ NA
  )
)
## Leaders --------------------------
# No effects.
est_1 <- feols(pop_lid_assas_dum~ i(time, treat_r_1, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)

est_2 <- feols(pop_killing_sd_dum~ i(time, treat_r_1, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
f1 <- list(ip = est_1, sd = est_2)
iplot(f1)

### incumbent heterogeneity. -----------------------
# No effects.
#### Left --------------------------
# Slight increase in second term. No robust across measuremnts. 
est_1 <- feols(pop_lid_assas_dum~ i(time, treat_r_1, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$incum_l == 1
)

est_2 <- feols(pop_killing_sd_dum~ i(time, treat_r_1, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset =  did_baseline_2$incum_l == 1
)
f1 <- list(ip = est_1, sd = est_2)
iplot(f1)

#### Center  ----------------------------------------

est_1 <- feols(pop_lid_assas_dum~ i(time, treat_r_1, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2,
               subset = did_baseline_2$incum_c == 1
)

est_2 <- feols(pop_killing_sd_dum~ i(time, treat_r_1, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2, 
               subset =  did_baseline_2$incum_c == 1
)
f1 <- list(ip = est_1, sd = est_2)
iplot(f1)


#Important finding. ***********
# Higher individual threats after election, not higher collective threats.
est_1 <- feols(pop_ind_threat_dum~ i(time, treat_r_1, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)

### Collective threat.  ------------------
#Important finding. ***********
# No higher collective threats. 
est_2 <- feols(pop_collective_threat_dum~ i(time, treat_r_1, ref = -1)
               |
                 codmpio + year, 
               cluster = ~codmpio,
               data = did_baseline_2
)
f3 <- list(est_1, est_2)
iplot(f3)
