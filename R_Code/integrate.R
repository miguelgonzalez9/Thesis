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
    "fuzzyjoin", "lubridate", "rdd", "stringdist")

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



# Load fuctions
source("functions.R")

# Final Expenditure cleaning and saving
source("final_clean_spend.R")

# Elections data cleaning.  
source("elections_cleaning.R")

# Population data
source("population_clean.R")

# Social leaders cleaning
# source("Soc_lid_clean.R") Run manually
source("SIVel_cleaning.R")
source("SIEVAC_clean.R")

# Merging elections, spending and municipality characteristics. 
source("final_baseline_data.R")

