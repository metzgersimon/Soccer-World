# first source the necessary files
setwd("/srv/shiny-server/Soccer-Prediction-App")
source("global.R")
source("Get_data_API.R")

# setup a connection to the database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='soccerworld',
                 username='dev',
                 password='worldpw')

################### 3. after matches: 23:00 ##################################

# 3.0 first call get_api_calls_left
# check how many calls we still have
api_calls_left <- get_api_calls_left()
print(get_api_calls_left())
# 3.1 fixture information call (for scores and points)
# check how many calls we would need
number_calls_needed <- compute_necessary_calls(number_leagues = 4,
                                               endpoint = "match_information")

matches_happened_today <- NULL

# check if we can make the call and get the data
if(api_calls_left >= number_calls_needed){
  matches_happened_today <- get_new_match_information_API(con)
  
} 

# 3.15 recall the fixture information from the data base
all_leagues_matches_today <- matches_happened_today

all_leagues_matches <- tbl(con, "all_leagues_matches") %>% data.frame()

if (!is.null(all_leagues_matches_today)) {
  
  # 3.2 team stats
  api_calls_left <- get_api_calls_left()
  
  # check how many calls we would need
  number_calls_needed <- compute_necessary_calls(number_matches_today = nrow(all_leagues_matches_today),
                                                 endpoint = "club_stats")
  
  # check if we can make the call and get the data
  if(api_calls_left >= number_calls_needed){
    get_new_club_stats_API(con)
   } 
  
  print(get_api_calls_left())
  
  # 3.2 fixture stats
  # check how many calls we still have
  api_calls_left <- get_api_calls_left()
  # check how many calls we would need
  number_calls_needed <- compute_necessary_calls(number_matches_today = nrow(all_leagues_matches_today),
                                                 endpoint = "fixture_stats")
  
  if(api_calls_left >= number_calls_needed){
    print("match stats start")
    fixture_stats_today <- get_new_match_stats_API(con)
    print("match stats start")
  }
  
  # 3.3 player stats
  # check how many calls we still have
  api_calls_left <- get_api_calls_left()
  
  # check how many calls we would need
  number_calls_needed <- compute_necessary_calls(number_matches_today = nrow(all_leagues_matches_today),
                                                 endpoint = "player_stats")
  
  if(api_calls_left >= number_calls_needed){
    player_stats_today <- get_new_player_stats_API(con)
  }
  
  # 3.4 match events
  # check how many calls we still have
  api_calls_left <- get_api_calls_left()
  
  # check how many calls we would need
  number_calls_needed <- compute_necessary_calls(number_matches_today = nrow(all_leagues_matches_today),
                                                 endpoint = "fixture_events")
  
  if(api_calls_left >= number_calls_needed){
    get_new_fixture_events_API(con)
  }

}

dbDisconnect(con)





