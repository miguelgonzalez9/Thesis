#########################################################
# Extrapolation of results to close race municipalities.
########################################################

# Parmilitary 
param_count <- table(RD_baseline %>% filter(year == 2015 & !is.na(share_diff2_r)) %>% 
        select(param_pres, codmpio) %>% distinct() %>% 
        pull(param_pres))

param_prop <- table(RD_baseline %>% filter(year == 2015 & !is.na(share_diff2_r)) %>% 
                       select(param_pres, codmpio) %>% distinct() %>% 
                       pull(param_pres))/sum(param_count)

param_prop <- param_prop[[2]]
# Development of close races.
close_diff <- RD_baseline %>% filter(!is.na(share_diff2_r) & param_pres == 1) %>% 
  select(pop_lead1234_lid_comunal_sector, share_diff2_r, pop_lid_comunal_sector,
         pop_comunal_sector_sd, pop_moe_pol_kill) %>% 
  mutate(rw_mayor = as.integer(share_diff2_r > 0)) %>% select(-share_diff2_r) %>% 
  group_by(rw_mayor) %>% summarise(across(everything(), mean))

x <- close_diff[2,-1] -close_diff[1,-1] 
close_diff[3,] <- c(0,x)

# First estiamte. Only on paramilitary and close races. Use Caloncio's badwidth. 

mod <- rdrobust(y = param_base$pop_lead1234_moe_pol_kill, 
                x = param_base$share_diff2_r, p = 2, 
                all = T, covs = param_base[controls_list_r$gsci])
bdw <- mod$bws[2]
year <- 2019
alpha <-  1.6
pop_tot <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
  select(codmpio, population) %>% pull(population) %>% sum()

pop_param <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
  select(codmpio, population, share_diff2_r, param_pres) %>% mutate(Treat = as.integer(share_diff2_r >0)) %>% 
  mutate(T_pop = (population/100000)*Treat*alpha*param_pres) %>% pull(T_pop) %>% sum()

effect_1 <- 100000*pop_param/pop_tot
effect_1
# Comapre national rate
NR <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
                                    select(lead1234_moe_pol_kill, population) %>% summarise(across(everything(), sum))
NR <- 100000*NR$lead1234_moe_pol_kill/NR$population
effect_1/NR

# More conservative estimates.
mod <- rdrobust(y = param_base$pop_lead1234_moe_pol_kill, 
                x = param_base$share_diff2_r, p = 2, 
                all = T, covs = param_base[controls_list_r$gsci])
bdw <- mod$bws[2]
alpha <-  1.6
conv_rate = 10^4
pop_tot <- RD_baseline %>% filter(year == 2019) %>% pull(population) %>% sum()

pop_param <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r)) %>% 
  select(codmpio, population, share_diff2_r, param_pres) %>% mutate(Treat = as.integer(share_diff2_r >0)) %>% 
  mutate(T_pop = (population/100000)*Treat*alpha*param_pres/((alpha*conv_rate)^(share_diff2_r))) %>% pull(T_pop) %>% sum()

effect_2 <- 100000*pop_param/pop_tot
effect_2

NR <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
  select(lead1234_moe_pol_kill, population) %>% summarise(across(everything(), sum))
NR <- 100000*NR$lead1234_moe_pol_kill/NR$population
effect_2/NR


# Second. Communal leaders.

# Localized
mod <- rdrobust(y = param_base$pop_lead1234_lid_comunal_sector, 
                x = param_base$share_diff2_r, p = 2, 
                all = T, covs = param_base[controls_list_r$gsci])
bdw <- mod$bws[2]
year <- 2019
alpha <-  5.523
pop_tot <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
  select(codmpio, population) %>% pull(population) %>% sum()

pop_param <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
  select(codmpio, population, share_diff2_r, param_pres) %>% mutate(Treat = as.integer(share_diff2_r >0)) %>% 
  mutate(T_pop = (population/100000)*Treat*alpha*param_pres) %>% pull(T_pop) %>% sum()

effect_1 <- 100000*pop_param/pop_tot
effect_1
# Comapre national rate
NR <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
  select(lead1234_lid_comunal_sector, population) %>% summarise(across(everything(), sum))
NR <- 100000*NR$lead1234_lid_comunal_sector /NR$population
effect_1/NR

# Localized + decreasing TEs
mod <- rdrobust(y = param_base$pop_lead1234_lid_comunal_sector, 
                x = param_base$share_diff2_r, p = 2, 
                all = T, covs = param_base[controls_list_r$gsci])
bdw_comunal <- mod$bws[2]
year <- 2019
alpha <-  5.523
pop_tot <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
  select(codmpio, population) %>% pull(population) %>% sum()

pop_param <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
  select(codmpio, population, share_diff2_r, param_pres) %>% mutate(Treat = as.integer(share_diff2_r >0)) %>% 
  mutate(T_pop = (population/100000)*Treat*alpha*param_pres/((alpha*conv_rate)^(share_diff2_r))) %>% pull(T_pop) %>% sum()

effect_1 <- 100000*pop_param/pop_tot
effect_1
# Comapre national rate
NR <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
  select(lead1234_lid_comunal_sector, population) %>% summarise(across(everything(), sum))
NR <- 100000*NR$lead1234_lid_comunal_sector /NR$population
effect_1/NR

# More conservative estimates. 
conv_rate = 10^4
pop_tot <- RD_baseline %>% filter(year == 2019) %>% pull(population) %>% sum()

pop_param <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r)) %>% 
  select(codmpio, population, share_diff2_r, param_pres) %>% mutate(Treat = as.integer(share_diff2_r >0)) %>% 
  mutate(T_pop = (population/100000)*Treat*alpha*param_pres/((alpha*conv_rate)^(share_diff2_r))) %>% pull(T_pop) %>% sum()

effect_2 <- 100000*pop_param/pop_tot
effect_2

NR <- RD_baseline %>% filter(year == 2019 & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
  select(lead1234_moe_pol_kill, population) %>% summarise(across(everything(), sum))
NR <- 100000*NR$lead1234_moe_pol_kill/NR$population
effect_2/NR

# Sensitivity analysis. 
extrapol <- function(data_frame, conv_rate, level, yr ,alpha, outcome, bwd){

  if(level == "national"){
    pop_tot <- data_frame %>% filter(year == yr) %>% pull(population) %>% sum()
    pop_param <- data_frame %>% filter(year == yr & !is.na(share_diff2_r)) %>% 
      select(codmpio, population, share_diff2_r, param_pres) %>% mutate(Treat = as.integer(share_diff2_r >0)) %>% 
      mutate(T_pop = (population/100000)*Treat*alpha*param_pres/((alpha*conv_rate)^(share_diff2_r))) %>% pull(T_pop) %>% sum()
    effect_2 <- 100000*pop_param/pop_tot
    NR <- data_frame %>% filter(year == yr & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
      select(.data[[outcome]], population) %>% summarise(across(everything(), sum))
    NR <- 100000*NR[[outcome]]/NR$population
  }
  if (level == "localized"){
    pop_tot <- data_frame %>% filter(year == yr & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
      select(codmpio, population) %>% pull(population) %>% sum()
    pop_param <- RD_baseline %>% filter(year == yr & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
      select(codmpio, population, share_diff2_r, param_pres) %>% mutate(Treat = as.integer(share_diff2_r >0)) %>% 
      mutate(T_pop = (population/100000)*Treat*alpha*param_pres/((alpha*conv_rate)^(share_diff2_r))) %>% pull(T_pop) %>% sum()
    effect_2 <- 100000*pop_param/pop_tot
    NR <- RD_baseline %>% filter(year == yr & !is.na(share_diff2_r) & abs(share_diff2_r) < bdw) %>% 
      select(.data[[outcome]], population) %>% summarise(across(everything(), sum))
    NR <- 100000*NR[[outcome]] /NR$population
  }
  return(effect_2)
}
mod <- rdrobust(y = param_base$lead1234_moe_pol_kill, 
                x = param_base$share_diff2_r, p = 2, 
                all = T, covs = param_base[controls_list_r$gsci])
bdw_poli <- mod$bws[2]
extrapol(RD_baseline, conv_rate = 10^4, level = "localized", yr = 2019, 
         alpha =1.6, outcome = "lead1234_moe_pol_kill", bwd = bdw_poli)
extrapol(RD_baseline, conv_rate = 10^4, level = "localized", yr = 2019, 
         alpha =5.523, outcome = "lead1234_lid_comunal_sector", bwd = bdw_comunal)

extrapol(RD_baseline, conv_rate = 10^4, level = "national", yr = 2019, 
         alpha =1.6, outcome = "lead1234_moe_pol_kill", bwd = bdw_poli)
extrapol(RD_baseline, conv_rate = 10^4, level = "national", yr = 2019, 
         alpha =5.523, outcome = "lead1234_lid_comunal_sector", bwd = bdw_comunal)

graphs <- list()
x <- seq((10^4)/2, 2*(10^4), 100)
counter <- 1
for (l in c("national", "localized")) {
  for (yr in c(2015,2019)) {
    for (out in c("lead1234_lid_comunal_sector", "lead1234_moe_pol_kill")) {
      if (out == "lead1234_lid_comunal_sector"){
        alpha <-  5.523
        mod <- rdrobust(y = param_base$pop_lead1234_lid_comunal_sector, 
                        x = param_base$share_diff2_r, p = 2, 
                        all = T, covs = param_base[controls_list_r$gsci])
        bdw <- mod$bws[2]
        ti <- "Communal leaders"
      }
      else {
        alpha <- 1.6
        mod <- rdrobust(y = param_base$lead1234_moe_pol_kill, 
                        x = param_base$share_diff2_r, p = 2, 
                        all = T, covs = param_base[controls_list_r$gsci])
        bdw <- mod$bws[2]
        ti <- "Political leaders"
      }
      y <- c()
      i <- 1
      for (j in x) {
        y[i] <- extrapol(RD_baseline, conv_rate = j, level = l, yr = yr, 
                         alpha =alpha, outcome = out, bwd = bdw)
        i <- i +1
      }
      g <- ggplot(data = data.frame(x = x, y = y), aes(x = x, y = y)) + geom_line() + 
        labs(title = paste(yr, ti, "killing rate", l)) + xlab("Convergence rate") + 
        ylab("Effect on national killing rate")
      graphs[[counter]] <- g
      counter = counter + 1
    }
  }
}
n <- length(graphs)
nCol <- floor(sqrt(n))
sens_analysis_1 <- do.call("grid.arrange", c(graphs[1:4], ncol=nCol))
ggsave(filename = "figure7.pdf", plot = sens_analysis_1, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 9, height = 10, units = "in")
sens_analysis_2 <- do.call("grid.arrange", c(graphs[5:8], ncol=nCol))
ggsave(filename = "figure8.pdf", plot = sens_analysis_2, device = "pdf",
       path = "D:/Documents/GitHub/Thesis/Figures", 
       width = 9, height = 10, units = "in")
