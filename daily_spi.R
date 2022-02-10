# This script tests if it is possible to daily pull the SPI data to the sql base

# source all relevant functions
source("Setup.R")

# run the global file
source("global.R")

# get data from 538 and push into data base
source("Get_data_538.R")

# safely disconnect from data base
dbDisconnect(con)