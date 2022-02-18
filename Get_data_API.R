########## get newest match information ############
get_new_match_information_API <- function(con){
  all_leagues_matches <- tbl(con, "all_leagues_matches") %>% data.frame()
  # extract the matchday of the games that happen today
  curr_matchday <- all_leagues_matches %>%
    # filter in the current season and the past matches
    filter(league_season == max(league_season),#
           # get only matches that happen today
           fixture_date == Sys.Date()) %>% 
    group_by(league_id) %>%
    select(league_id, league_round) %>%
    distinct()
      
  
  # extract the current (and max) season
  max_season <- all_leagues_matches %>%
    summarize(max_season = max(league_season)) %>%
    pull()
  
  
  # create a variable to store the new match information
  all_leagues_new_matches <- NULL
  
  # iterate over all leagues in the newest_matchdays frame
  for(i in 1:nrow(max_matchdays)){
    # for every league get all the matches for the new matchday
    curr_league_matches <- get_fixtures_in_league_by_season(max_matchdays$league_id[i],
                                                            season = max_season,
                                                            matchday = max_matchdays$league_round[i]) 
    
    curr_leagues_matches_today <- curr_league_matches %>%
      # filter for only matches that happen today
      filter(!(status_long %in% c("Match Cancelled", "Match Postponed")),
             fixture_date == Sys.Date())

    
    # bind them together with the matches frame for all leagues
    all_leagues_new_matches <- bind_rows(all_leagues_new_matches,
                                         curr_leagues_matches_today)
  }
  
  # finally, if we have actual stats data, we want to write it to the data base
  if(nrow(all_leagues_new_matches) != 0){
    # delete the matches that are new from the data set in the data base
    all_leagues_matches_new <- all_leagues_matches %>%
      filter(!(fixture_id %in% all_leagues_new_matches$fixture_id))
    
    # and then insert the new data (with the score, points, etc)
    all_leagues_matches_new <- all_leagues_matches_new %>%
      bind_rows(all_leagues_new_matches) %>%
      # reorder the frame
      arrange(league_id, league_season, league_round, fixture_date, fixture_time)
    
    # write the new frame with the new match data into the data base
    # by overwriting the old frame
    dbWriteTable(con, "all_leagues_matches", all_leagues_matches_new,
                 overwrite = TRUE)
  }
  
  # we also want to return this frame to further use it in the process
  return(all_leagues_new_matches)
}



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



########## get newest fixture stats ############
get_new_match_stats_API <- function(con, date = NULL){
  # extract all past stats from the all_leagues_fixture_stats frame
  past_matches_stats <- tbl(con, "all_leagues_fixture_stats") %>%
    data.frame()
  
  # if there is no date given we just want to get all past stats
  if(is.null(date)){
    past_matches_stats <- past_matches_stats %>%
      filter(season == max(season),
             fixture_date <= Sys.Date()) %>%
      # extract all fixture ids
      select(fixture_id) %>% 
      pull() %>%
      unique()
    
    # now get all matches that are already finished but not yet
    # in the data set
    all_not_contained_matches <- tbl(con, "all_leagues_matches") %>%
      data.frame() %>%
      filter(league_season == max(league_season),
             fixture_date <= Sys.Date(),
             status_long == "Match Finished",
             !fixture_id %in% past_matches_stats)
  } else {
    # otherwise we just want to get the data for the given date
    past_matches_stats <- past_matches_stats %>%
      filter(season == max(season),
             fixture_date == ymd(date)) %>%
      # extract all fixture ids
      select(fixture_id) %>% 
      pull() %>%
      unique()
    
    # now get all matches that are already finished but not yet
    # in the data set
    all_not_contained_matches <- tbl(con, "all_leagues_matches") %>%
      data.frame() %>%
      filter(league_season == max(league_season),
             fixture_date == ymd(date),
             status_long == "Match Finished",
             !fixture_id %in% past_matches_stats)
  }
    
  # create a variable to store the newly extracted fixture stats
  all_leagues_fixture_stats_new <- NULL
  
  # iterate over all matches we extracted above
  for(i in 1:length(all_not_contained_matches$fixture_id)){
    # extract the fixture stats of the current match
    curr_fixture_stats <- get_fixture_stats(all_not_contained_matches$fixture_id[i])
  
    # bind them together with the fixture_ids frame for all matches
    all_leagues_fixture_stats_new <- bind_rows(all_leagues_fixture_stats_new,
                                               curr_fixture_stats)
  }
  
  # finally, if we have actual stats data, we want to write it to the data base
  if(nrow(all_leagues_fixture_stats_new) != 0){
    
    dbWriteTable(con, "all_leagues_fixture_stats", all_leagues_fixture_stats_new,
                 overwrite = FALSE, append = TRUE)
  }
}




########## get newest player stats ############
get_new_player_stats_API <- function(con, date = NULL){
  
  # extract all past stats from the all_leagues_player_stats frame
  past_matches_player_stats <- tbl(con, "all_leagues_player_stats") %>%
    data.frame()
  
  # if there is no date given we just want to get all past stats
  if(is.null(date)){
    past_matches_player_stats <- past_matches_player_stats %>%
      filter(league_season == max(league_season),
           fixture_date <= Sys.Date()) %>%
    data.frame() %>%
    # extract all fixture ids
    select(fixture_id) %>% 
    pull() %>%
    unique()
  
    # now get all matches that are already finished but not yet
    # in the data set
    all_not_contained_matches <- tbl(con, "all_leagues_matches") %>%
      filter(league_season == max(league_season),
             fixture_date <= Sys.Date(),
             status_long == "Match Finished",
             !fixture_id %in% past_matches_player_stats)
  
  } else {
    # otherwise we just want to get the data for the given date
    past_matches_player_stats <- past_matches_player_stats %>%
      filter(league_season == max(league_season),
             fixture_date == ymd(date)) %>%
      data.frame() %>%
      # extract all fixture ids
      select(fixture_id) %>% 
      pull() %>%
      unique()
    
    # now get all matches that are already finished but not yet
    # in the data set
    all_not_contained_matches <- tbl(con, "all_leagues_matches") %>%
      filter(league_season == max(league_season),
             fixture_date == ymd(date),
             status_long == "Match Finished",
             !fixture_id %in% past_matches_player_stats)
  }
  
  # create a variable to store the newly extracted fixture stats
  all_leagues_player_stats_new <- NULL
  
  # iterate over all matches we extracted above
  for(i in 1:length(all_not_contained_matches$fixture_id)){
    # extract the player stats of the current match
    curr_player_stats <- get_player_stats_fixture(all_not_contained_matches$fixture_id[i])
    
    # bind them together with the fixture_ids frame for all matches
    all_leagues_player_stats_new <- bind_rows(all_leagues_player_stats_new,
                                              curr_player_stats)
  }
  
  # finally, if we have actual stats data, we want to write it to the data base
  if(nrow(all_leagues_player_stats_new) != 0){
    
    dbWriteTable(con, "all_leagues_player_stats", all_leagues_player_stats_new,
                 overwrite = FALSE, append = TRUE)
  }
}




########## get newest team stats ############
get_new_club_stats_API <- function(con){
  all_leagues_club_stats <- tbl(con, "all_leagues_club_stats") %>% data.frame()
  all_leagues_matches <- tbl(con, "all_leagues_matches") %>% data.frame()
  # extract the max available match days for each league and team
  max_matchdays <- all_leagues_club_stats %>%
    filter(league_season == max(league_season)) %>%
    group_by(league_id, team_id) %>%
    summarize(max_matchday = max(matchday))
  
  # extract the current (and max) season
  max_season <- all_leagues_club_stats %>%
    summarize(max_season = max(league_season)) %>%
    pull()
  
  
  all_leagues_club_stats_new <- NULL
  
  for(i in 1:nrow(max_matchdays)){
    # get for the current club all the matches that are not currently
    # considered in the club stats frame
    current_club_missing_matchdays <- all_leagues_matches %>%
      filter(league_season == max(league_season),
             league_id == max_matchdays$league_id[i],
             max_matchdays$team_id[i] == club_id_home |
               max_matchdays$team_id[i] == club_id_away,
             league_round > max_matchdays$max_matchday[i],
             fixture_date <= Sys.Date(),
             status_long == "Match Finished")
    
    # get the club stats for the extracted matchdays that are missing
    # in the all_leagues_club_stats frame
    current_club_stats <- 
      get_team_stats_in_league_by_season(max_matchdays$league_id[i],
                                         max_matchdays$team_id[i],
                                         season = max_season,
                                         match_dates = current_club_missing_matchdays$fixture_date)
    
    # combine the data to the overall data set of new stats
    all_leagues_club_stats_new <- bind_rows(all_leagues_club_stats_new,
                                            current_club_stats)
  }
  
  # finally, if we have actual stats data, we want to write it to the data base
  if(nrow(all_leagues_club_stats_new) != 0){
    
    dbWriteTable(con, "all_leagues_club_stats", all_leagues_club_stats_new,
                 overwrite = FALSE, append = TRUE)
  }
}



########## get newest fixture events ############
get_new_fixture_events_API <- function(con){
  all_leagues_fixture_events <- tbl(con, "all_leagues_fixture_events") %>% data.frame()
  all_leagues_matches <- tbl(con, "all_leagues_matches") %>% data.frame()
  
  # extract all past stats from the all_leagues_fixture_events frame
  past_matches_events <- all_leagues_fixture_events %>%
    filter(league_season == max(league_season),
           fixture_date <= Sys.Date()) %>%
    # extract all fixture ids
    select(fixture_id) %>% 
    pull() %>%
    unique()
  
  # now get all matches that are already finished but not yet
  # in the data set
  all_not_contained_matches <- all_leagues_matches %>%
    filter(league_season == max(league_season),
           fixture_date <= Sys.Date(),
           status_long == "Match Finished",
           !fixture_id %in% past_matches_events)
  
  
  # create a variable to store the newly extracted fixture stats
  all_leagues_fixture_events_new <- NULL
  
  # iterate over all matches we extracted above
  for(i in 1:length(all_not_contained_matches$fixture_id)){
    # extract the event of the current match
    curr_match_events <- get_fixture_events(all_not_contained_matches$fixture_id[i])
    
    # bind them together with the fixture_ids frame for all matches
    all_leagues_fixture_events_new <- bind_rows(all_leagues_fixture_events_new,
                                                curr_match_events)
  }
  
  # finally, if we have actual events data, we want to write it to the data base
  if(nrow(all_leagues_fixture_events_new) != 0){
    
    dbWriteTable(con, "all_leagues_fixture_events", all_leagues_fixture_events_new,
                 overwrite = FALSE, append = TRUE)
  }
}


# function should compute all the calls we need to access the data
# of a certain endpoint based on the number of matches that happened today
# and based on the number of leagues we consider
compute_necessary_calls <- function(number_matches_today = NULL, 
                                    number_leagues = NULL, 
                                    endpoint = "fixture_stats"){
  necessary_calls <- 0
  
  # compute the number of calls we need to perform a call to a certain
  # endpoint. E.g., for the fixture stats we need as many calls as we have
  # matches because it is based on the fixture id.
  # for the club stats however, we need for every match 2 calls (because
  # there are 2 teams involved)
  if(endpoint == "fixture_stats"){
    necessary_calls <- number_matches_today
  } else if(endpoint == "player_stats"){
    necessary_calls <- number_matches_today
  } else if(endpoint == "fixture_events"){
    necessary_calls <- number_matches_today
  } else if(endpoint == "club_stats"){
    necessary_calls <- number_matches_today * 2
  } else if(endpoint == "match_information"){
    necessary_calls <- number_leagues
  } 
  
  return(necessary_calls)
}


# function should compute if the call we want to do is possible
api_call_is_possible <- function(){
  api_calls_left <- get_api_calls_left()
  number_calls_needed <- compute_necessary_calls(number_matches_today = nrow(all_leagues_matches_today),
                                                 endpoint = "fixture_stats")
  if(api_calls_left >= number_calls_needed){
    fixture_stats_today <- get_new_match_stats_API()
  }
}



# 
# ########## get newest team transfers ############
# get_new_team_transfers_API <- function(){
#   
# }