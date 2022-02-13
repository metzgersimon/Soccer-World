###### daily morning call to get the todays matches ######
#### get_new_match_information_daily ####
get_new_match_information_daily <- function(league_ids = c(78, 79, 39, 61), season){
  all_leagues_matches_today <- NULL
  # iterate over all leagues
  for(i in 1:length(league_ids)){
    # get the matches of the current league in the current season
    curr_league_matches <- get_fixtures_in_league_by_season(league_ids[i],
                                                            season) %>%
      # filter for only those matches that happen today
      filter(fixture_date == Sys.Date(),
             # and only those matches that do not have status "Match Cancelled"
             # or "Match Postponed")
             !(status_long %in% c("Match Cancelled", "Match Postponed")))
    
    
    all_leagues_matches_today <- bind_rows(all_leagues_matches_today,
                                           curr_league_matches)
    
  }
  return(all_leagues_matches_today)
}


# function should determine the timestamps we need to get the lineups
# for the matches today
get_times_for_lineup_scraping <- function(all_leagues_matches_today){
  # we need the time and the match id
  matches_with_times <- all_leagues_matches_today %>%
    # create a posixct date
    mutate(fixture_POSIXct = as.POSIXct(paste0(fixture_date, " ", fixture_time))) %>%
    select(fixture_POSIXct, fixture_id) %>%
    # group by the fixture time
    group_by(fixture_POSIXct) %>%
    # add another variable which indicates when we need to get the lineups
    # 30 minutes before the match
    mutate(lineup_time_1 = fixture_POSIXct - minutes(30)) %>%
    # transform the lineup into a format we can work with for the cronjob,
    # e.g. "minute hour day month *" ("45 20 06 02 *")
    mutate(lineup_time1_cronjob = paste0(minute(lineup_time_1),
                                         " ",
                                         hour(lineup_time_1),
                                         " ",
                                         day(lineup_time_1),
                                         " ",
                                         month(lineup_time_1),
                                         " *"))
  
  return(matches_with_times)
}



############## 30 minutes before each match extracted above #########################
# run the cron job for the LINEUPS



################### 3. after matches: 23:00 ##################################

# 3.0 first call get_api_calls_left
# api_calls_left <- get_api_calls_left()

# 3.1 fixture information call (for scores and points)
# matches_happened_today <- get_new_match_information_API()

# 3.2 recall the fixture information from the data base
all_leagues_matches_today <- tbl(con, "all_leagues_matches") %>%
  filter(status_long == "Match Finished",
         fixture_date == Sys.Date())

# 3.2 fixture stats
# all_leagues_matches_today
# get_new_match_stats_API -> rewrite function such that it takes fixture ids
# (from all_leagues_matches_today$fixture_id as input)

# table in data base which contains all matches that we did not had the calls
# to get the data for
# check mit get_api_calls_left()

# 3.3 player stats
# all_leagues_matches_today
# get_new_match_stats_API -> rewrite function such that it takes fixture ids
# (from all_leagues_matches_today$fixture_id as input)

# check mit get_api_calls_left()






