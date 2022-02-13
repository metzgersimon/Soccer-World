setwd("/home/ubuntu/project/Soccer-Prediction-App")
source("Setup.R")

# clear the cron tab
cron_clear(ask = FALSE, user = "ubuntu")

# get the matches actually scheduled for today
leagues_today <- get_leagues_playing_today()
matches_today <- get_match_data_todays_games(leagues_today)
timeslots <- get_times_for_lineup_scraping(matches_today)

save(timeslots, file = "./Scheduled/timeslots.RData")

# delete cron tabs from the day before
cron_clear(ask = FALSE, user = ubuntu)

# initialise new cronR tabs for today

slots <- timeslots %>% 
          ungroup() %>%
          select(lineup_time1_cronjob) %>%
          unique()

for (sl in slots[[1]]) {
  # add the right syntax for skript
  cron_add(
           command = "Rscript /home/ubuntu/project/Soccer-Prediction-App/Scheduled/set_lineup_timer.R", 
           frequency = sl, 
           ask = FALSE,
           user = "ubuntu",
           id = as.character(paste(Sys.time(), sl, sep = "0000")))
}
