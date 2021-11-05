#### Load only Libraries needed for  oddsportal_raspi.R ####

if (!require("dplyr")) install.packages("dplyr")
if (!require("httr")) install.packages("httr")
if (!require("rlist")) install.packages("rlist")
if (!require("stringr")) install.packages("stringr")

library(httr)
library(dplyr)
library(rlist)
library(stringr)

# Source script containing all API keys needed
source("API_keys.R")
# Source all R-files in the Functions folder
source("get_current_odds.R")

