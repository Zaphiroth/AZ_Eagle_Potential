# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ Eagle potential
# Purpose:      Potential division
# programmer:   Zhe Liu
# Date:         2020-11-10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


options(java.parameters = "-Xmx2048m",
        stringsAsFactors = FALSE)

##---- loading the required packages ----
suppressPackageStartupMessages({
  require(openxlsx)
  require(readxl)
  require(feather)
  require(plyr)
  require(stringi)
  require(feather)
  require(RODBC)
  require(MASS)
  require(car)
  require(data.table)
  require(plotly)
  require(tidyverse)
  require(lubridate)
  require(forecast)
  require(qdapRegex)
  require(doParallel)
})

##---- setup the directories ----
system("mkdir 01_Background 02_Inputs 03_Outputs 04_Codes 05_Internal_Review 06_Deliveries")
