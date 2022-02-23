setwd("/srv/shiny-server/Soccer-Prediction-App")
source("Setup.R")
source("Get_data_API.R")

library(cronR)

# create database connection
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='soccerworld',
                 username='dev',
                 password='worldpw')

# get the matches actually scheduled for today
slots <- get_times_for_lineup_scraping(
            get_new_match_information_daily(season = 2021))

# save the list for today
dbWriteTable(con,"timeslots", slots,  overwrite = TRUE)
dbDisconnect(con)

# initialise new cronR tabs for today
slots <- slots %>% 
          ungroup() %>%
          select(lineup_time1_cronjob) %>%
          unique()

for (sl in slots[[1]]) {
  # add the right syntax for skript
  cron_add(
           command = "Rscript /srv/shiny-server/Soccer-Prediction-App/Scheduled/lineup_cron.R", 
           frequency = sl, 
           ask = FALSE,
           user = "ubuntu",
           id = as.character(paste(Sys.time(), sl, sep = "0000")))
}







