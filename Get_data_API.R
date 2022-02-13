########## get newest match information ############
get_new_match_information_API <- function(){
  # extract the max available match days for each league
  max_matchdays <- all_leagues_matches %>%
    # filter in the current season and the past matches
    filter(league_season == max(league_season),#
           ############### nur matches, die heute stattfidnen
           fixture_date == Sys.Date()) %>% #%>%
    group_by(league_id) %>%
    select(league_id, league_round) %>%
    distinct()
      
  
  # extract the current (and max) season
  max_season <- all_leagues_matches %>%
    summarize(max_season = max(league_season)) %>%
    pull()
  
  # compute the new matchday we want to get data for
  # newest_matchdays <- max_matchdays %>%
  #   mutate(new_matchday = max_matchday + 1)
  
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



########## get newest fixture stats ############
get_new_match_stats_API <- function(){
  # extract the max available match days for each league
  max_matchdays <- all_leagues_fixture_stats %>%
    filter(season == max(season)) %>%
    group_by(league_id) %>%
    summarize(max_matchday = max(matchday))
  
  # compute the new matchday we want to get data for
  newest_matchdays <- max_matchdays %>%
    mutate(max_matchday = max_matchday + 1)
  
  # extract all the fixture ids for all leagues on the newest matchday
  fixture_ids <- NULL
  
  # iterate over all leagues in the newest_matchdays frame
  for(i in 1:nrow(newest_matchdays)){
    # for every league get all the fixture ids for the new matchday
    curr_fixture_ids <- all_leagues_matches %>%
      filter(league_season == max(league_season),
             league_id == newest_matchdays$league_id[i],
             league_round == newest_matchdays$max_matchday[i])
    
    # bind them together with the fixture_ids frame for all leagues
    fixture_ids <- bind_rows(fixture_ids,
                             curr_fixture_ids)
  }
  
  # create a variable to store the newly extracted fixture stats
  all_leagues_fixture_stats_new <- NULL
  
  # iterate over all matches we extracted above
  for(i in 1:length(fixture_ids$fixture_id)){
    # extract the fixture stats of the current match
    curr_fixture_stats <- get_fixture_stats(fixture_ids$fixture_id[i])
  
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
get_new_player_stats_API <- function(){
  # extract the max available match days for each league
  max_matchdays <- all_leagues_player_stats %>%
    filter(league_season == max(league_season)) %>%
    group_by(league_id) %>%
    summarize(max_matchday = max(league_round))
  
  # compute the new matchday we want to get data for
  newest_matchdays <- max_matchdays %>%
    mutate(max_matchday = max_matchday + 1)
  
  # extract all the fixture ids for all leagues on the newest matchday
  fixture_ids <- NULL
  
  # iterate over all leagues in the newest_matchdays frame
  for(i in 1:nrow(newest_matchdays)){
    # for every league get all the fixture ids for the new matchday
    curr_fixture_ids <- all_leagues_matches %>%
      filter(league_season == max(league_season),
             league_id == newest_matchdays$league_id[i],
             league_round == newest_matchdays$max_matchday[i])
    
    # bind them together with the fixture_ids frame for all leagues
    fixture_ids <- bind_rows(fixture_ids,
                             curr_fixture_ids)
  }
  
  # create a variable to store the newly extracted player stats
  all_leagues_player_stats_new <- NULL
  
  # iterate over all matches we extracted above
  for(i in 1:length(fixture_ids$fixture_id)){
    # extract the player stats of the current match
    curr_player_stats <- get_player_stats_fixture(fixture_ids$fixture_id[i])
    
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
get_new_club_stats_API <- function(){
  # extract the max available match days for each league
  max_matchdays <- all_leagues_club_stats %>%
    filter(league_season == max(league_season)) %>%
    group_by(league_id) %>%
    summarize(max_matchday = max(matchday))
  
  # extract the current (and max) season
  max_season <- all_leagues_club_stats %>%
    summarize(max_season = max(league_season)) %>%
    pull()
  
  # compute the new matchday we want to get data for
  newest_matchdays <- max_matchdays %>%
    mutate(new_matchday = max_matchday + 1)
  
  
  # get all the teams that we want to look at
  league_team_ids <- all_leagues_club_stats %>%
    # only look in the current season
    filter(league_season == max(league_season)) %>%
    # select only the league and team id 
    select(league_id, team_id) %>%
    # only use distinct values
    distinct()
  
  
  # for those extracted league and team ids we now have to find the date of the
  # last match (i.e., the new match that is not already in the data base)
  all_team_match_dates <- NULL
  
  for(i in 1:nrow(league_team_ids)){
    # extract the matchday we want to get data for given our current league
    new_matchday_for_curr_league <- newest_matchdays %>%
      filter(league_id == league_team_ids$league_id[i]) %>%
      select(new_matchday) %>% 
      pull()
    
    # extract the match date of the last match for the current team
    curr_team_match_date <- all_leagues_matches %>%
      filter(league_id == league_team_ids$league_id[i],
             club_id_home == league_team_ids$team_id[i] |
               club_id_away == league_team_ids$team_id[i],
             league_season == max_season,
             league_round == new_matchday_for_curr_league) %>%
      # get only the match date
      select(league_id, fixture_date) %>%
      # add the team id as variable
      mutate(team_id = league_team_ids$team_id[i])
    
    
    # append the match date of the current team to the overall frame
    all_team_match_dates <- bind_rows(all_team_match_dates,
                                      curr_team_match_date)
  }
  
  
  # create an variable to store the team stats of all leagues
  all_leagues_club_stats_new <- NULL
  
  # iterate over all league-team pairs and get the team stats for these pairs
  for(i in 1:nrow(all_team_match_dates)){
    # extract and store the ids of the current league and team
    # and its last (most recent) match date
    curr_league_id <- all_team_match_dates$league_id[i]
    curr_team_id <- all_team_match_dates$team_id[i]
    curr_fixture_date <- all_team_match_dates$fixture_date[i]
    
    # extract the team stats of the current team
    curr_team_club_stats <- get_team_stats_in_league_by_season(curr_league_id,
                                                               curr_team_id,
                                                               max_season,
                                                               curr_fixture_date)
    
    # bind together the curr team data and the overall data
    all_leagues_club_stats_new <- bind_rows(all_leagues_club_stats_new,
                                            curr_team_club_stats)
                                                               
            
  }
  
  
  # finally, if we have actual stats data, we want to write it to the data base
  if(nrow(all_leagues_club_stats_new) != 0){
    
    dbWriteTable(con, "all_leagues_club_stats", all_leagues_club_stats_new,
                 overwrite = FALSE, append = TRUE)
  }
}




########## get newest fixture events ############
get_new_fixture_events_API <- function(){
  # extract the max available match days for each league
  macthes_to_get <- all_leagues_matches %>%
    filter(league_season == max(league_season),
           fixture_date < Sys.Date()) %>%
    semi_join(fixtur)
    group_by(league_id) %>%
    summarize(max_matchday = max(matchday))
  
  # extract the current (and max) season
  max_season <- all_leagues_club_stats %>%
    summarize(max_season = max(league_season)) %>%
    pull()
  
  # compute the new matchday we want to get data for
  newest_matchdays <- max_matchdays %>%
    mutate(new_matchday = max_matchday + 1)
  
  
  # get all the teams that we want to look at
  league_team_ids <- all_leagues_club_stats %>%
    # only look in the current season
    filter(league_season == max(league_season)) %>%
    # select only the league and team id 
    select(league_id, team_id) %>%
    # only use distinct values
    distinct()
  
  
  # for those extracted league and team ids we now have to find the date of the
  # last match (i.e., the new match that is not already in the data base)
  all_team_match_dates <- NULL
  
  for(i in 1:nrow(league_team_ids)){
    # extract the matchday we want to get data for given our current league
    new_matchday_for_curr_league <- newest_matchdays %>%
      filter(league_id == league_team_ids$league_id[i]) %>%
      select(new_matchday) %>% 
      pull()
    
    # extract the match date of the last match for the current team
    curr_team_match_date <- all_leagues_matches %>%
      filter(league_id == league_team_ids$league_id[i],
             club_id_home == league_team_ids$team_id[i] |
               club_id_away == league_team_ids$team_id[i],
             league_season == max_season,
             league_round == new_matchday_for_curr_league) %>%
      # get only the match date
      select(league_id, fixture_date) %>%
      # add the team id as variable
      mutate(team_id = league_team_ids$team_id[i])
    
    
    # append the match date of the current team to the overall frame
    all_team_match_dates <- bind_rows(all_team_match_dates,
                                      curr_team_match_date)
  }
  
  
  # create an variable to store the team stats of all leagues
  all_leagues_club_stats_new <- NULL
  
  # iterate over all league-team pairs and get the team stats for these pairs
  for(i in 1:nrow(all_team_match_dates)){
    # extract and store the ids of the current league and team
    # and its last (most recent) match date
    curr_league_id <- all_team_match_dates$league_id[i]
    curr_team_id <- all_team_match_dates$team_id[i]
    curr_fixture_date <- all_team_match_dates$fixture_date[i]
    
    # extract the team stats of the current team
    curr_team_club_stats <- get_team_stats_in_league_by_season(curr_league_id,
                                                               curr_team_id,
                                                               max_season,
                                                               curr_fixture_date)
    
    # bind together the curr team data and the overall data
    all_leagues_club_stats_new <- bind_rows(all_leagues_club_stats_new,
                                            curr_team_club_stats)
    
    
  }
  
  
  # finally, if we have actual stats data, we want to write it to the data base
  if(nrow(all_leagues_club_stats_new) != 0){
    
    dbWriteTable(con, "all_leagues_club_stats", all_leagues_club_stats_new,
                 overwrite = FALSE, append = TRUE)
  }
}




########## get newest team transfers ############
get_new_team_transfers_API <- function(){
  
}