setwd("/home/ubuntu/project/Soccer-Prediction-App")
source("Setup.R")

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
              filter(fixture_POSIXct == as.character(
                                            min(timeslots$fixture_POSIXct)))

# remove selected timeslots from table and save it 
timeslots <- anti_join(timeslots, current)
save(timeslots, file = "./Scheduled/timeslots.RData")

# receive data for selected ids
new_lineups <- data.frame()
for (id in current$fixture_id) {
  new_lineups <- rbind(new_lineups, get_fixture_lineups(id))
}



dbWriteTable(con, "all_leagues_fifa_squads", all_leagues_fifa_squads,
             overwrite = TRUE)

dbDisconnect(con)
