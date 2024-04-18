# Loading packages --------------------------------------------------------
#dev.off(dev.list()["RStudioGD"])
chooseCRANmirror(ind=89)
rm(list=ls())

installation_needed  <- FALSE
loading_needed       <- TRUE

package_list <- 
  c("dplyr", "tidyr", "psych", "arsenal",  "qwraps2", "ggplot2", "plyr", "xtable", "stargazer", 
    "ggrepel", "metafor", "gridExtra", "ggpubr", "gghighlight",  "reshape", "stringr", "matlib", 
    "imputeTS", "cepiigeodist", "writexl", "slider", "visreg", "zoo", "vtable", "compareGroups", 
    "rstanarm", "dotwhisker", "ggthemes", "fixest", "conflicted", "haven", "gganimate", "tidytext", 
    "rvest", "sf", "scales", "raster", "spData",  "tmap", "leaflet","gsnth", "WDI", "panelView", 
    "plm", "bayesplot", "sandwich", "tseries", "car", "lmtest", "multiwayvcov", 
    "lfe", "tidyverse", "bacondecomp", "ggiplot", "didimputation", "readxl", "baggr", "rstan", "rlist", "bayesplot",
    "gridExtra", "grid", "ggpmisc", "stringi", "modelsummary", "estimatr", "fastDummies", 
    "fuzzyjoin", "lubridate", "rdrobust", "stringdist", "kableExtra", "rddensity", "xlsx",
    "tmap", "sfheaders", "viridis")

if(installation_needed){install.packages(package_list)}
if(loading_needed){lapply(package_list, require, character.only = TRUE)}
conflict_prefer_all("dplyr")
conflict_prefer("melt", "reshape")
conflict_prefer("normal", "baggr")
conflicts_prefer(ggplot2::annotate)

tidy=TRUE

path <- "D:/Documents/GitHub/Thesis/R_Code"
setwd(path)

# Working directory -------------------------------------------------------
options(qwraps2_markup = "markdown")
options(scipen=999)
start_time <- 2012
end_time <- 2023
source("functions.R")

############################ Cleaning ###################################

# Final Expenditure cleaning and saving
source("final_clean_spend.R")

# Elections data cleaning.  
source("elections_cleaning.R")

# Population data
source("population_clean.R")

# Social leaders cleaning
# source("Soc_lid_clean.R") Run manually
source("SIVel_cleaning.R")
path <- "D:/Documents/GitHub/Thesis/R_Code"
setwd(path)
source("SIEVAC_clean.R")

# Merging elections, spending and municipality characteristics. 
source("final_baseline_data.R")


########################### Results ###############################
# Run all together it's not required to run cleaning first
###################################################################
rm(list=ls())
source("functions.R")
# Load data
RD_baseline <- read.csv("D:/Documents/GitHub/Thesis/Data/Final_data/RD_data.csv")
var_names <- readRDS("D:/Documents/GitHub/Thesis/Data/Final_data/vnames.RData")
conflict_vars <- var_names[[1]]
spend_vars <- var_names[[2]]
control_vars <- var_names[[3]]

pop_conflict_vars <- paste0("pop_",conflict_vars)[c(1,2,5,7,10:16,18:20, 23, 24,27,29:33,35, 
                                                    50)][-c(14,22)]
conflict_vars_rd <- conflict_vars[c(1:2, 7, 11, 13, 14, 16,29,31,49)]
lead1_spending_vars <- c(spend_vars[str_detect(spend_vars, "lead1$")], "V1_payment_diff1", "V_payment_diff1")
lead2_spending_vars <- c(spend_vars[str_detect(spend_vars, "lead2$")], "V1_payment_diff2", "V_payment_diff2")

geo_controls <- control_vars[c(2:6, 9:12)]
soc_controls <- control_vars[c(13:20, 7, 8)]
running_vars <- c("share_diff2_r", "share_diff1_l")
incum_controls_r <- c("l_ideol_incum_r", "r_ideol_incum_r")
incum_controls_l <- c("l_ideol_incum_l", "r_ideol_incum_l")

controls_list_r <- list(nc = "",gc = geo_controls, sc = soc_controls, gsc =  c(geo_controls, soc_controls), 
                      gsci = c(geo_controls, soc_controls, incum_controls_r))
controls_list_l <- list(nc = "",gc = geo_controls, sc = soc_controls, gsc =  c(geo_controls, soc_controls), 
                        gsci = c(geo_controls, soc_controls, incum_controls_l))

# RD baseline results
source("RD_results.R")

# Covariates balance and placebo test.
source("placebo_RD.R")



