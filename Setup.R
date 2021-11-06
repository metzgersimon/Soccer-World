#### Loading libraries needed ####
if (!require("binman")) install.packages("binman")
if (!require("dplyr")) install.packages("dplyr")
if (!require("devtools")) install.packages("devtools")
if (!require("httr")) install.packages("httr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("magrittr")) install.packages("magrittr")
if (!require("miceadds")) install.packages("miceadds")
if (!require("netstat")) install.packages("netstat")
if (!require("rlist")) install.packages("rlist")
if (!require("RSelenium")) install.packages("RSelenium")
if (!require("rvest")) install.packages("rvest")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("xfun")) install.packages("xfun")
if (!require("worldfootballR")) devtools::install_github("JaseZiv/worldfootballR")


library(binman)
library(dplyr)
library(httr)
library(lubridate)
library(miceadds)
library(netstat)
library(rlist)
library(RSelenium)
library(rvest)
library(stringr)
library(tidyr)
library(worldfootballR)


# Source script containing all API keys needed
source("API_keys.R")
# Source all R-files in the External Functions folder
source.all("Functions/External Functions/", grepstring="\\.R")
# Source all R-files in the Functions folder
source.all("Functions/", grepstring="\\.R")


