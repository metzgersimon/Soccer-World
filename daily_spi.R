# This script tests if it is possible to daily pull the SPI data to the sql base

source("Setup.R")

# connect database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='Soccer_Prediction_Data',
                 username='root',
                 password='my-secret-pw')

# get most recent table
all_leagues_spi_538_available_matches <- tbl(con, "all_leagues_spi_538") %>%
  data.frame()

# get data from 538 and push into data base
source("Get_data_538.R")

# safely disconnect from data base
dbDisconnect(con)