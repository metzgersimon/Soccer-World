#### Loading libraries needed ####
if (!require("dplyr")) install.packages("dplyr")
if (!require("httr")) install.packages("httr")
if (!require("rlist")) install.packages("rlist")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyr")) install.packages("tidyr")

library(dplyr)
library(httr)
library(rlist)
library(stringr)
library(tidyr)


# Source script containing all API keys needed
source("API_keys.R")