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

dbWriteTable(con, "all_leagues_fixture_lineups", new_lineups,
             overwrite = FALSE, append = TRUE)

source("global.R")

# load the model here 

# then get the full lineup model data 

# then filter fixture ids only in new_lineups

# write predictions to all_leagues_historical_lineups_predictions
new_lineups

dbDisconnect(con)
