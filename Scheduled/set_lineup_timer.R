setwd("/home/ubuntu/project/Soccer-Prediction-App")
source("Setup.R")

# create database connection
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='Soccer_Prediction_Data',
                 username='root',
                 password='my-secret-pw')

# get the matches actually scheduled for today
leagues_today <- get_leagues_playing_today()
matches_today <- get_match_data_todays_games(leagues_today)
timeslots <- get_times_for_lineup_scraping(matches_today)

# save the list for today
dbRemoveTable(con, "timeslots")
dbWriteTable(con,"timeslots", timeslots,  overwrite = TRUE)
dbDisconnect(con)

# delete cron tabs from the day before
cron_clear(ask = FALSE, user = "ubuntu")

# initialise new cronR tabs for today
slots <- timeslots %>% 
          ungroup() %>%
          select(lineup_time1_cronjob) %>%
          unique()

for (sl in slots[[1]]) {
  # add the right syntax for skript
  cron_add(
           command = "Rscript /home/ubuntu/project/Soccer-Prediction-App/Scheduled/lineup_cron.R", 
           frequency = sl, 
           ask = FALSE,
           user = "ubuntu",
           id = as.character(paste(Sys.time(), sl, sep = "0000")))
}







