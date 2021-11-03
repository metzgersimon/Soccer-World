#### Loading libraries needed ####
if (!require("dplyr")) install.packages("dplyr")
if (!require("devtools")) install.packages("devtools")
if (!require("httr")) install.packages("httr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("miceadds")) install.packages("miceadds")
if (!require("rlist")) install.packages("rlist")
if (!require("rvest")) install.packages("rvest")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("worldfootballR")) devtools::install_github("JaseZiv/worldfootballR")


library(dplyr)
library(httr)
library(lubridate)
library(miceadds)
library(rlist)
library(rvest)
library(stringr)
library(tidyr)
library(worldfootballR)


# Source script containing all API keys needed
source("API_keys.R")
# Source all R-files in the Functions folder
source.all("Functions/", grepstring="\\.R")


