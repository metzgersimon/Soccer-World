# first source the necessary files
source("Setup.R")
source("Get_data_API.R")

# setup a connection to the database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='Soccer_Prediction_Data',
                 username='root',
                 password='my-secret-pw')

################### 3. after matches: 23:00 ##################################

# 3.0 first call get_api_calls_left
# check how many calls we still have
api_calls_left <- get_api_calls_left()

# 3.1 fixture information call (for scores and points)
# check how many calls we would need
number_calls_needed <- compute_necessary_calls(number_leagues = 4,
                                               endpoint = "match_information")
# check if we can make the call and get the data
if(api_calls_left >= number_calls_needed){
  matches_happened_today <- get_new_match_information_API(con)
} else {
  # if we do not have enough calls we want to write the missing data to the data base
  # to reload it at a later point
  matches_happened_today <- tbl(con, "all_leagues_matches") %>%
    data.frame() %>%
    filter(fixture_date == Sys.Date(),
           # and only those matches that do not have status "Match Cancelled"
           # or "Match Postponed")
           !(status_long %in% c("Match Cancelled", "Match Postponed"))) %>%
    # add a new variable to check later on what type of data was missing
    mutate(info_type_missing = "match_information")
  
  dbWriteTable(con, "all_leagues_matches_missed", matches_happened_today,
               overwrite = FALSE, append = TRUE)
}

# 3.15 recall the fixture information from the data base
all_leagues_matches_today <- matches_happened_today

# 3.2 team stats
api_calls_left <- get_api_calls_left()

# check how many calls we would need
number_calls_needed <- compute_necessary_calls(number_matches_today = nrow(all_leagues_matches_today),
                                               endpoint = "club_stats")

# check if we can make the call and get the data
if(api_calls_left >= number_calls_needed){
  get_new_club_stats_API(con)
} else {
  # if we do not have enough calls we want to write the missing data to the data base
  # to reload it at a later point
  matches_happened_today <- tbl(con, "all_leagues_matches") %>%
    data.frame() %>%
    filter(fixture_date == Sys.Date())
  
  dbWriteTable(con, "all_leagues_matches_missed")
}



# 3.2 fixture stats
# check how many calls we still have
api_calls_left <- get_api_calls_left()
# check how many calls we would need
number_calls_needed <- compute_necessary_calls(number_matches_today = nrow(all_leagues_matches_today),
                                               endpoint = "fixture_stats")

if(api_calls_left >= number_calls_needed){
  fixture_stats_today <- get_new_match_stats_API(con)
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










