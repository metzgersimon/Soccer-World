# resets the API call counter every day
resets_call_counter <- function(){
  ## global variable call_counter to count the API calls
  call_counter <<- 0
}


# function should check which leagues are actually having games today
get_leagues_playing_today <- function(league_ids = c(78, 79, 39, 61)){
  # get all matches that happen in the selected leagues at the current day
  matches_today <- all_leagues_matches %>%
    # filter for the selected league ids
    filter(league_id %in% league_ids,
           # check that the match date is the current date
           fixture_date == Sys.Date())

  return(matches_today)
}


# get all match information for the leagues
get_match_data_todays_games <- function(matches_today){
  # get the leagues and matchdays that are having games today
  matchdays_each_league <- matches_today %>%
    group_by(league_season, league_id) %>%
    select(league_round) %>%
    unique()
  
  # there is the possibility that a match is cancelled which we need to consider
  all_actual_matches <- NULL
  
  # iterate over all leagues and get the matches for the current matchday
  for(i in 1:nrow(matchdays_each_league)){
    # get the data (season, id and matchday) of the current league
    curr_data <- matchdays_each_league[i, ]
    # get the matches of the current league
    curr_league_matches <- get_fixtures_in_league_by_season(league_id = curr_data$league_id,
                                                            season = curr_data$league_season,
                                                            matchday = curr_data$league_round)
    
    # filter those matches for only those matches we want
    curr_league_matches_today <- curr_league_matches %>%
      # only the matches on the current day
      filter(fixture_date == Sys.Date(),
             # and only those matches that do not have status "Match Cancelled"
             # or "Match Postponed")
             !(status_long %in% c("Match Cancelled", "Match Postponed")))
    
    # and now add only those matches to the list of actual matches of interest
    # that really seem to happen
    all_actual_matches <- bind_rows(all_actual_matches,
                                    curr_league_matches_today)
  }
  
  
  return(all_actual_matches)
}

# function should determine the timestamps we need to get the lineups
# for the matches today
get_times_for_lineup_scraping <- function(all_actual_matches){
  # we need the time and the match id
  matches_with_times <- all_actual_matches %>%
    select(fixture_time, fixture_id) %>%
    # group by the fixture time
    group_by(fixture_time) %>%
    # add another variable which indicates when we need to get the lineups
    # (first time 30 minutes before the match)
    mutate(lineup_time_1 = hm(fixture_time) - minutes(30))
}




