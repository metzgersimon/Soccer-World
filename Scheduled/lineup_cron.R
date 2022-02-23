setwd("/srv/shiny-server/Soccer-Prediction-App")
source("Setup.R")
source("Get_data_API.R")

# connect to database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='Soccer_Prediction_Data',
                 username='root',
                 password='my-secret-pw')

# load the timetable
timeslots <- dbReadTable(con, "timeslots")


# select closest uscraped timeslots
current <- timeslots %>% 
              filter(fixture_POSIXct == min(timeslots$fixture_POSIXct))

# remove selected timeslots from table and save it 
timeslots <- anti_join(timeslots, current, by =  "fixture_id")
dbRemoveTable(con, "timeslots")
dbWriteTable(con, "timeslots", timeslots, overwrite = TRUE)

# receive data for selected ids
new_lineups <- data.frame()
for (id in current$fixture_id) {
  new_lineups <- rbind(new_lineups, get_fixture_lineups(id))
}

# write to database
dbWriteTable(con, "all_leagues_fixture_lineups", new_lineups,
             overwrite = FALSE, append = TRUE)


# reload most recent frame
all_leagues_fixture_lineups <-
  tbl(con, "all_leagues_fixture_lineups") %>% data.frame()

agg <- NULL
for (i in 1:length(new_lineups$fixture_id)) {
  
  ptm <- proc.time()
  temp <- prepare_lineup_data_subset(new_lineups$fixture_id[i], all_leagues_fixture_lineups)
  agg <- rbind(agg, temp)
  print(proc.time() - ptm)
  
}

# write new data to all_leagues_lineups_agg
dbWriteTable(con, "all_leagues_lineups_agg", agg, 
             overwrite = FALSE, append = TRUE)

Sys.sleep(10)

source("global.R")

# load the model here 
predicted_matches <- train_xgb_lin()

# connect to database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='Soccer_Prediction_Data',
                 username='root',
                 password='my-secret-pw')


dbWriteTable(con, "all_leagues_lineups_predictions", predicted_matches,
             overwrite = FALSE, append = TRUE)

dbDisconnect(con)
