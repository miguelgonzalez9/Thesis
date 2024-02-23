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
    "fuzzyjoin", "lubridate")

if(installation_needed){install.packages(package_list)}
if(loading_needed){lapply(package_list, require, character.only = TRUE)}
conflict_prefer_all("dplyr")
conflict_prefer("melt", "reshape")
conflict_prefer("normal", "baggr")
conflicts_prefer(ggplot2::annotate)

tidy=TRUE

# Working directory -------------------------------------------------------
options(qwraps2_markup = "markdown")
options(scipen=999)

path <- "D:/Documents/GitHub/Thesis/R_Code"
setwd(path)

# Cleaning and merging datafiles --------------------------------------------------------------
source("data_cleaning_SISFUT_2017_2020.R")
source("data_cleaning_SISFUT_2015_2016.R")

# Final Expenditure cleaning and saving
source("final_clean_exp.R")


