# function should prepare the venue data frame to be able to be used in the model.
prepare_venue_data <- function(){
  venues <- venues_with_coordinates %>%
    # we want to start the model with 2016
    filter(season >= 2016) %>%
    # drop unnecessary variables
    select(-c(team_name, country, national, logo, venue_name, 
              venue_address, venue_city, venue_surface, venue_image))
  
  home_data <- venues %>%
    inner_join(., all_leagues_matches, 
               by = c("league_id", "season"  = "league_season",
                      "team_id" = "club_id_home"))
  
  away_data <- venues %>%
    inner_join(all_leagues_matches,
               by = c("league_id", "season"  = "league_season",
                      "team_id" = "club_id_away"))
  
  venues_data_all <- home_data %>%
    inner_join(away_data, by = "fixture_id") %>%
    # rename the important variables
    rename("longitude_home" = "longitude.x",
           "longitude_away" = "longitude.y",
           "latitude_home" = "latitude.x",
           "latitude_away" = "latitude.y") %>%
    # drop all duplicated columns
    select(-c(contains(".y"))) %>%
    # rename the duplicated columns and remove the ".x"
    rename_with(~str_remove(., "\\.x"), .cols = everything()) %>%
    select(league_id, season, fixture_date, fixture_time, league_round, 
           club_id_home, club_id_away, club_name_home, club_name_away, venue_capacity,
           longitude_home, latitude_home, longitude_away, latitude_away) %>%
    rowwise() %>%
    # compute the difference between the two venues
    mutate(travel_distance_away_team = prepare_venue_distance(longitude_home, latitude_home,
                                                              longitude_away, latitude_away))
  
  return(venues_data_all)
}

# function should compute the distance in kilometers of two venues
prepare_venue_distance <- function(longitude1, latitude1, longitude2, latitude2){
  venue_distance <- distm(c(longitude1, latitude1), 
                          c(longitude2, latitude2), 
                          fun = distHaversine)/1000
  
  venue_distance <- as.numeric(venue_distance)
  
  return(venue_distance)
}



# function converts a data frame with 2 rows (unsuitable for the model)
# into one row 
convert_two_lines_into_one <- function(frame_to_convert, columns_to_drop = NULL,
                                       join_columns = NULL){
  frame_to_convert <- frame_to_convert %>%
    group_by(match_group = rep(row_number(), length.out = n(), each = 2)) %>%
    ungroup() 
  
  home_part <- frame_to_convert %>%
    group_by(match_group) %>%
    filter(row_number(desc(match_group)) == 1) %>%
    ungroup() %>%
    select(-c(contains("away"),
              match_group))
  
  away_part <- frame_to_convert %>%
    group_by(match_group) %>%
    filter(row_number(desc(match_group)) == 2) %>%
    ungroup() %>%
    select(-c(contains("home"),
              match_group,
              columns_to_drop))
  
  one_line_frame <- home_part %>%
    inner_join(away_part, by = join_columns, suffix = c("_home", "_away"))
  
  return(one_line_frame)
}



# prepare the match stats for every club by aggregating the data
# and convert it into one row per match
# Immer nur für aktuelle Saison neu, für historische einmalig
prepare_team_match_stats_historical <- function(){
  match_stats_historical <- all_leagues_fixture_stats %>%
    # get only past seaons and remove matchdays that are not the regular season,
    # i.e., relegation matches
    filter(#season < max(season),
           season >= 2016,
           !is.na(matchday)) %>%
    # group by the league and team id
    group_by(league_id, team_id,) %>%
    # because we do not have the stats for e.g. matchday 1 at matchday 1
    # we lag the variables we consider for prediction with a n of 1
    mutate(across(c(shots_on_goal:passing_accuracy),
                  ~lag(.x, n = 1))) %>%
    # after that we calculate a cumulative running mean with a window of 2,
    # i.e., 2 matches
    group_by(league_id, season, team_id) %>%
    mutate(across(c(shots_on_goal:passing_accuracy),
                  ~rollmean(.x, k = 2, fill = NA))) %>%
                  # ~rollme(.x, n = 2, cumulative = TRUE))) %>%
    # then we use the helper function convert_two_lines_into_one to convert
    # the data frame from two rows for 1 match into one row per match
    convert_two_lines_into_one(., columns_to_drop = c("league_id", "season",
                                                      "matchday", "fixture_date",
                                                      "fixture_time"),
                               join_columns = "fixture_id")
    
  
    return(match_stats_historical)
    
}


######### 538 ###############
prepare_spi_data <- function(){
  spi_538_matches <- all_leagues_matches %>%
    filter(league_season >= 2016) %>%
    left_join(all_leagues_spi_538,
              by = c("league_name" = "league",
                     "league_season" = "season",
                     "fixture_date" =  "date",
                     "club_name_home" = "home_team",
                     "club_name_away" = "away_team")) %>%
    # drop variables we do not want to use as features
    select(-c(home_team_win_prob, away_team_win_prob, draw_prob, 
              home_team_projected_score, away_team_projected_score,
              home_team_goals, away_team_goals)) %>%
    mutate(across(c(home_team_expected_goals:away_team_adjusted_goals),
                  ~lag(.x, n = 1)))
  
  return(spi_538_matches)
}






################# FIFA STATS ##########################

prepare_fifa_team_stats <- function(){

  # get the all_leagues_matches data set with just a few columns to join
  all_leagues_matches_matcher <- all_leagues_matches %>%
    select(league_id, league_name, league_season, league_round,
           fixture_id, fixture_date, fixture_time)
  
  # join the league name from the all_leagues_matches_matcher to
  # be able to join the fifa stats later on
  all_leagues_fixture_stats2 <- all_leagues_fixture_stats %>% 
    distinct() %>%
    left_join(all_leagues_matches_matcher,
              by = c("league_id", 
                     "season" = "league_season",
                     "matchday" = "league_round",
                     "fixture_date",
                     "fixture_time",
                     "fixture_id"))
  
  # create the min and max dates of the team stats
  min_date_team_stats <- min(all_leagues_fifa_team_stats$date)
  max_date_team_stats <- max(all_leagues_fifa_team_stats$date)
  
  # extract all clubs
  unique_clubs <- unique(all_leagues_fifa_team_stats$club)
  
  all_leagues_fifa_daily_team_stats <- NULL
  
  # iterate over all clubs and create a data set where for every day
  # date is an imputed value for the stats (if it is missing)
  for(i in 1:length(unique_clubs)){
    # create a data frame with dates from the min date to the max date
    daily_seq <- seq(as.Date(min_date_team_stats), 
                     as.Date(max_date_team_stats), "days") %>%
      data.frame("date" = .)
    
    # filter for the current club in the fifa team stats frame
    # and reorder it
    curr_club_stats <- all_leagues_fifa_team_stats %>%
      filter(club == unique_clubs[i]) %>%
      arrange(fifa_vers, date)
    
    # join the club stats to the date data frame
    daily_seq_with_stats <- left_join(daily_seq,
                                      curr_club_stats,
                                      by = "date") 
    
    # now fill all the missing values "downwards"
    daily_stats <- daily_seq_with_stats %>%
      fill(everything())
    
    # bind the data together
    all_leagues_fifa_daily_team_stats <- bind_rows(all_leagues_fifa_daily_team_stats,
                                                   daily_stats)
    
  }
  
  # join the fixture stats we created above with the daily team stats
  # we created above
  fifa_stats_joined <- all_leagues_fixture_stats2 %>%
    mutate(date_minus1 = fixture_date - days(1)) %>%
    left_join(all_leagues_fifa_daily_team_stats,
              by = c("league_name" = "league",
                     "date_minus1" = "date",
                     "team_name" = "club"),
              keep = TRUE)
  
}
  


######################## ranking winning pcts ##########################
# function returns the ranking of a selected league in a selected season
# for every matchday
get_league_ranking_overall <- function(league_ids = c(78, 79, 39, 61), 
                                       seasons = c(2016:2021), 
                                       type = "home"){
  
  all_leagues_running_table <- NULL
  
  for(i in 1:length(league_ids)){
    all_seasons_running_table <- NULL
    for(j in 1:length(seasons)){
      
      # get the standing of the current season over time
      curr_season_table <- get_league_standing(league_ids[i], seasons[j]) %>%
        group_by(club_name) %>%
        # shift the data one matchday backwards
        mutate(across(c(rank, cum_points, cum_goals, cum_goals_against,
                        cum_goal_diff), ~shift(.x, n = 1))) %>%
        # drop variables we do not need
        select(-c(points, goals, goals_against, goal_diff)) %>%
        ungroup() %>%
        # add a variable for the season
        mutate(season = seasons[j])
      
      # bind the data together
      all_seasons_running_table <- bind_rows(all_seasons_running_table,
                                             curr_season_table)
      
    
    }
    
    # add a variable for the league id
    all_seasons_running_table <- all_seasons_running_table %>%
      mutate(league_id = league_ids[i])
    
    
    # bind the data together
    all_leagues_running_table <- bind_rows(all_leagues_running_table,
                                           all_seasons_running_table)
  }
  
  # reorder the data frame
  all_leagues_running_table <- all_leagues_running_table %>%
    select(league_id, season, league_round, everything())

  
  
  if(type == "home"){
    all_leagues_running_table <- all_leagues_running_table %>%
      rename(home_team_rank = rank,
             home_team_points = cum_points,
             home_team_goals = cum_goals,
             home_team_goals_against = cum_goals_against,
             home_team_goal_diff = cum_goal_diff)
  } else {
    all_leagues_running_table <- all_leagues_running_table %>%
      rename(away_team_rank = rank,
             away_team_points = cum_points,
             away_team_goals = cum_goals,
             away_team_goals_against = cum_goals_against,
             away_team_goal_diff = cum_goal_diff)
  }
  
  return(all_leagues_running_table)
  
}


# function should compute the probability that a team wins/draws/looses at
# home or away 
get_winning_pcts <- function(type = "home"){
  # filter all the matches in all leagues for the selected league and season
  # league_season_matches <- all_leagues_matches %>%
  #   filter(league_id == league,
  #          league_season == season)
  leagues_matches <- all_leagues_matches %>%
    filter(fixture_date < Sys.Date(),
           league_season >= 2016)
  
  # get the winning/draw/lose percentages for the team playing at home
  home_table <- leagues_matches %>%
    # group by the league and season
    group_by(league_id, league_season) %>%
    # rename the variables according to the home team
    select(league_round, club_name = club_name_home, club_id = club_id_home,
           points = home_points, goals = fulltime_score_home, 
           goals_against = fulltime_score_away) %>%
    # add a variable for the potential points a team could have scored at this
    # point
    mutate(potential_points = 3) %>%
    # group by league, the club and the season to compute the stats aggregated for each club
    # in each season
    group_by(league_id, club_id, league_season) %>%
    # sum up the potential points and wins/draws/loses of each team at home
    mutate(potential_points = cumsum(potential_points),
           home_played = potential_points / 3,
           home_wins = cumsum(points == 3),
           home_draws = cumsum(points == 1),
           home_losses = cumsum(points == 0),
           # compute the percentages that a team wins/draws/loses if it plays
           # at home
           home_win_pct = home_wins / (home_wins + home_draws + home_losses) * 100,
           home_draw_pct = home_draws / (home_wins + home_draws + home_losses) * 100,
           home_loss_pct = home_losses / (home_wins + home_draws + home_losses) * 100)
  
  
  # get the winning/draw/lose percentages for the team playing away
  away_table <- leagues_matches %>%
    # group by the league and season
    group_by(league_id, league_season) %>%
    # rename the variables according to the away team
    select(league_round, club_name = club_name_away, club_id = club_id_away,
           points = away_points, goals = fulltime_score_away, 
           goals_against = fulltime_score_home) %>%
    # add a variable for the potential points a team could have scored at this
    # point
    mutate(potential_points = 3) %>%
    # group by league, the club and the season to compute the stats aggregated for each club
    # in each season
    group_by(league_id, club_id, league_season) %>%
    # sum up the potential points and wins/draws/loses of each team away
    mutate(potential_points = cumsum(potential_points),
           away_played = potential_points / 3,
           away_wins = cumsum(points == 3),
           away_draws = cumsum(points == 1),
           away_losses = cumsum(points == 0),
           # compute the percentages that a team wins/draws/loses if it plays
           # away
           away_win_pct = away_wins / (away_wins + away_draws + away_losses) * 100,
           away_draw_pct = away_draws / (away_wins + away_draws + away_losses) * 100,
           away_loss_pct = away_losses / (away_wins + away_draws + away_losses) * 100)
  
  
  # combine the two tables to an overall table which indicates in pct
  # how many games a team wins/draws/loses in total independent of the
  # venue status
  full_win_pct <- bind_rows(home_table, 
                            away_table) %>%
    # group by the league and season
    group_by(league_id, league_season) %>%
    # order by the matchdays
    arrange(league_round) %>%
    # group by the matchdays
    group_by(league_id, league_season, league_round) %>%
    mutate(potential_points = 3) %>%
    ungroup() %>%
    # sum up the potential points that could have been achieved at each point
    mutate(potential_points = cumsum(potential_points)) %>%
    # group by the clubs to compute the stats aggregated for each club
    group_by(club_id, league_season) %>%
    # calculate the cumulative wins etc and then calculate the overall
    # percentage of winning etc
    mutate(wins = cumsum(points == 3),
           draws = cumsum(points == 1),
           losses = cumsum(points == 0),
           win_pct = (wins/ (wins + draws+ losses)) * 100,
           draw_pct = (draws/(wins + draws+ losses)) * 100,
           loss_pct = (losses/(wins + draws+ losses)) * 100) %>%
    # mutate(across(c(home_wins:loss_pct), ~shift(.x, n = 1))) %>%
    select(-c(points, goals, goals_against, potential_points)) %>%
    # mutate(home_played = cumsum(home_played))
    mutate(home_wins = ifelse(is.na(home_wins),
                              wins - away_wins,
                              home_wins),
           home_draws = ifelse(is.na(home_draws),
                               draws - away_draws,
                               home_draws),
           home_losses = ifelse(is.na(home_losses),
                                losses - away_losses,
                                home_losses),
           away_wins = ifelse(is.na(away_wins),
                              wins - home_wins,
                              away_wins),
           away_draws = ifelse(is.na(away_draws),
                               draws - home_draws,
                               away_draws),
           away_losses = ifelse(is.na(away_losses),
                                losses - home_losses,
                                away_losses),
           # home_team_home_win_pct = )#%>%
           home_played = home_wins + home_draws + home_losses,
           away_played = away_wins + away_draws + away_losses,
           home_win_pct = (home_wins / home_played) * 100,
           home_draw_pct = (home_draws / home_played) * 100,
           home_loss_pct = (home_losses / home_played) * 100,
           away_win_pct = (away_wins / away_played) * 100,
           away_draw_pct = (away_draws / away_played) * 100,
           away_loss_pct = (away_losses / away_played) * 100) %>%
    # lag everything
    mutate(across(c(home_played:loss_pct),
                  ~ lag(.x, n = 1)))
  
  
  if(type == "home"){
    full_win_pct <- full_win_pct %>%
      rename(home_team_wins = wins,
             home_team_draws = draws,
             home_team_losses = losses,
             home_team_win_pct = win_pct,
             home_team_draw_pct = draw_pct,
             home_team_loss_pct = loss_pct,
             home_team_home_wins = home_wins,
             home_team_home_draws = home_draws,
             home_team_home_losses = home_losses,
             home_team_home_win_pct = home_win_pct,
             home_team_home_draw_pct = home_draw_pct,
             home_team_home_loss_pct = home_loss_pct,
             
             home_team_away_wins = away_wins,
             home_team_away_draws = away_draws,
             home_team_away_losses = away_losses,
             home_team_away_win_pct = away_win_pct,
             home_team_away_draw_pct = away_draw_pct,
             home_team_away_loss_pct = away_loss_pct)
  } else {
    full_win_pct <- full_win_pct %>%
      rename(away_team_wins = wins,
             away_team_draws = draws,
             away_team_losses = losses,
             away_team_win_pct = win_pct,
             away_team_draw_pct = draw_pct,
             away_team_loss_pct = loss_pct,
             away_team_home_wins = home_wins,
             away_team_home_draws = home_draws,
             away_team_home_losses = home_losses,
             away_team_home_win_pct = home_win_pct,
             away_team_home_draw_pct = home_draw_pct,
             away_team_home_loss_pct = home_loss_pct,
             away_team_away_wins = away_wins,
             away_team_away_draws = away_draws,
             away_team_away_losses = away_losses,
             away_team_away_win_pct = away_win_pct,
             away_team_away_draw_pct = away_draw_pct,
             away_team_away_loss_pct = away_loss_pct)
  }
  
  return(full_win_pct)
}


############################ LINEUP DATA ######################################
# function should map the (historical) transfermarkt lineups data with
# the lineup information of a soon to begin match from the API
# to create one line of stats for the current lineup
prepare_lineup_data <- function(match_id){
  #match_id <- 719232

  # get the lineups from the data base
  current_game_lineup <- all_leagues_fixture_lineups %>%
    filter(fixture_id == match_id) %>%
    # prepare the name of the players to map the API and the TM data
    mutate(player_lastname = str_remove(player_name, pattern = ".*\\. |.*\\s"),
           # split the name by spaces
           player_lastname2 = str_split(player_lastname, " "))
  
  # extract the home id
  home_id <- unique(current_game_lineup$club_id_home)
  
  # extract the league name
  league_name <- unique(current_game_lineup$league_name)
  
  # now get from the string split only the last element
  current_game_lineup$player_lastname2 <- sapply(current_game_lineup$player_lastname2,
                                                 tail, 1)
  
  # create the lastname for mapping by removing special characters
  current_game_lineup <- current_game_lineup %>%
    mutate( 
      # map all special characters to plain ones
      player_lastname_map = stri_trans_general(player_lastname2, id = "Latin-ASCII")
    )
  
  # if it is a relegation game we just want to return NULL
  if(is.na(unique(current_game_lineup$league_round))){
    return(NULL)
  }
  
  # check if the current matchday is the first matchday of the season
  # If it is the first matchday, we want to impute the data somehow
  if(unique(current_game_lineup$league_round) == 1){
    curr_lineups_tm <- all_leagues_lineups_tm %>%
      filter(season == unique(current_game_lineup$league_season) - 1)
    
    # search in the previous season lineup data from transfermarkt for the
    # current teams (historical data)
    curr_lineups_tm2 <- curr_lineups_tm %>%
      filter(team %in% c(unique(current_game_lineup$club_name_home),
                         unique(current_game_lineup$club_name_away)),
             matchday >= unique(current_game_lineup$league_round)) %>%
      # split the name by spaces
      mutate(player_lastname2 = str_split(player_name, " "))
    # if it is not the first matchday, we get the current season
  } else {
    curr_lineups_tm <- all_leagues_lineups_tm %>%
      filter(season == unique(current_game_lineup$league_season))
    
    # search in the current season lineup data from transfermarkt for the
    # current teams (historical data)
    curr_lineups_tm2 <- curr_lineups_tm %>%
      filter(team %in% c(unique(current_game_lineup$club_name_home),
                         unique(current_game_lineup$club_name_away)),
             matchday < unique(current_game_lineup$league_round)) %>%
      # split the name by spaces
      mutate(player_lastname2 = str_split(player_name, " "))
  }
  
  
  
  # now get from the string split only the last element
  curr_lineups_tm2$player_lastname2 <- sapply(curr_lineups_tm2$player_lastname2,
                                             tail, 1)
  
  curr_lineups_tm2 <- curr_lineups_tm2 %>%
    mutate(# map all special characters to plain ones
           player_lastname_map = stri_trans_general(player_lastname2, id = "Latin-ASCII"),
           # map transfermarkt positions to match API positions
           player_pos_API_to_map = ifelse(str_detect(player_pos, pattern = "Goal"),
                                          "G",
                                          ifelse(str_detect(player_pos, pattern = "Back|Sweeper"),
                                                 "D",
                                                 ifelse(str_detect(player_pos, pattern = "Midfield"),
                                                        "M",
                                                        ifelse(str_detect(player_pos, pattern = "Winger|Striker|Forward"),
                                                               "F",
                                                               player_pos)))),
           # adjust the nationalities such that we can map them later
           player_nationality = ifelse(player_nationality == "United States",
                                       "USA",
                                       player_nationality))
  
  
  ######## if a player is missing then we want to impute the values for this player
  # based on the averages for this particular position and team
  
  # first check if there are any players that did not match from the API (newest data)
  # to the transfermarkt
  not_matched_players <- current_game_lineup %>%
    anti_join(curr_lineups_tm2, 
              by = c("team_name" = "team", 
                     "player_number",
                     "player_lastname_map")) 
  
  # we also want to join the all the other players
  matched_players <- current_game_lineup %>%
    # join by the team name, the player number and the (cleaned) lastname
    inner_join(curr_lineups_tm2, 
               by = c("team_name" = "team", 
                      "player_number",
                      "player_lastname_map")) %>%
    # create team, number and lastname pairs to get the latest information
    # for every pair from transfermarkt
    group_by(team_name, player_number, player_lastname_map) %>%
    filter(match_date == max(match_date)) %>%
    ungroup() %>%
    # do the final variable transformations and ordering
    rename(player_name_API = `player_name.x`,
           player_pos_API = `player_pos.x`,
           player_lastname_to_map = player_lastname_map,
           player_name_TM = `player_name.y`,
           player_pos_TM = `player_pos.y`) %>%
    # create a new variable for the match id
    mutate(fixture_id = match_id)
  
  
  
  # check if the current matchday is the first matchday of the season
  # If it is the first matchday, we want to impute the data somehow
  if(unique(current_game_lineup$league_round) == 1){
    # find the player match stats that are relevant for the current match
    player_stats <- all_leagues_player_stats %>%
      mutate(player_lastname = gsub("^.* ", "", player_name),
             # map all special characters to plain ones
             player_lastname_map = stri_trans_general(player_lastname, id = "Latin-ASCII")) %>%
      filter(league_season == unique(current_game_lineup$league_season) - 1,
             league_round >= unique(current_game_lineup$league_round),
             team_name %in% c(unique(current_game_lineup$club_name_home),
                              unique(current_game_lineup$club_name_away))) %>%
      # drop columns we do not need
      select(-c(contains("league"), contains("fixture"), contains("venue"),
                contains("status"), referee, contains("logo"),
                contains("score"), contains("club")))
  } else {
    # find the player match stats that are relevant for the current match
    player_stats <- all_leagues_player_stats %>%
      mutate(player_lastname = gsub("^.* ", "", player_name),
             # map all special characters to plain ones
             player_lastname_map = stri_trans_general(player_lastname, id = "Latin-ASCII")) %>%
      filter(league_season == unique(current_game_lineup$league_season),
             league_round < unique(current_game_lineup$league_round),
             team_name %in% c(unique(current_game_lineup$club_name_home),
                              unique(current_game_lineup$club_name_away))) %>%
      # drop columns we do not need
      select(-c(contains("league"), contains("fixture"), contains("venue"),
                contains("status"), referee, contains("logo"),
                contains("score"), contains("club")))
  }
  
  matched_lineups_complete <- NULL
  
  
  
  
  if(nrow(not_matched_players) == 0){
    # filter in the player stats for only those players that are in the
    # matched players
    matched_players_stats <- player_stats %>%
      # filter for the players in the current lineup
      filter(player_id %in% matched_players$player_id)
    
    # compute aggregated values for the stats grouped by team and position
    matched_lineups_stats <- matched_players %>%
      inner_join(matched_players_stats,
                 by = c("team_name", "team_id", "player_id")) %>%
      group_by(team_id, games_position) %>%
      summarize(across(c(player_age, player_market_value_in_million_euro,
                         player_rating_tm, games_minutes, games_rating, goals_total:cards_red),
                       ~ mean(.x, na.rm = TRUE))) %>%
      ungroup()
    
    # compute aggregated values for the transfers grouped by team and position
    matched_lineups_transfers <- matched_players %>%
      inner_join(matched_players_stats,
                 by = c("team_name", "team_id", "player_id")) %>%
      distinct(player_id, .keep_all = TRUE) %>%
      group_by(team_id, games_position) %>%
      summarize(across(c(player_is_new_transfer, player_is_new_winter_transfer,
                         player_is_returnee),
                       ~ sum(.x, na.rm = TRUE))) %>%
      ungroup()
    
    # bind the two frames together
    matched_lineups_complete <- matched_lineups_stats %>%
      inner_join(matched_lineups_transfers,
                 by = c("team_id", "games_position"))
    
    # if a player is missing then we want to impute the values for this player
    # based on the averages for this particular position and team
  } else {
    # filter in the player stats for only those players that are in the
    # matched players
    matched_players_stats <- player_stats %>%
      # filter for the players in the current lineup
      filter(player_id %in% matched_players$player_id)
    
    positions <- c("G", "D", "M", "F")
    
    # check if every position is available for both teams
    positions_home <- matched_players_stats %>%
      filter(team_id == home_id)
    
    positions_away <- matched_players_stats %>%
      filter(team_id != home_id) 
    
    
    if(nrow(positions_home) == 0 | nrow(positions_away) == 0){
      # in case the matched_player_stats only contain one team (because
      # of a successful relegation of a imperior league) we
      # give the team the average over the whole season of all teams
        # extract the missing team id
      missing_team_id <- current_game_lineup %>%
        filter(!(team_id %in% matched_players_stats$team_id)) %>%
        select(team_id) %>%
        distinct() %>%
        pull()
        
      # prepare the linups tm data
      curr_lineups_tm <- curr_lineups_tm %>%
        # split the name by spaces
        mutate(player_lastname2 = str_split(player_name, " "))  %>%
        filter(league == league_name)
      
      # now get from the string split only the last element
      curr_lineups_tm$player_lastname2 <-
        sapply(curr_lineups_tm$player_lastname2,
               tail, 1)
        
      curr_lineups_tm <- curr_lineups_tm %>%
        mutate(# map all special characters to plain ones
          player_lastname_map = stri_trans_general(player_lastname2, id = "Latin-ASCII"),
          # map transfermarkt positions to match API positions
          player_pos_API_to_map = ifelse(str_detect(player_pos, pattern = "Goal"),
                                         "G",
                                         ifelse(str_detect(player_pos, pattern = "Back|Sweeper"),
                                                "D",
                                                ifelse(str_detect(player_pos, pattern = "Midfield"),
                                                       "M",
                                                       ifelse(str_detect(player_pos, pattern = "Winger|Striker|Forward"),
                                                              "F",
                                                              player_pos)))),
          # adjust the nationalities such that we can map them later
          player_nationality = ifelse(player_nationality == "United States",
                                      "USA",
                                      player_nationality))
      
      # average the stats over the whole season in the league for each position
      imputed_lineups_stats_all_teams <- player_stats %>%
        inner_join(curr_lineups_tm,
                   by = c("team_name" = "team", 
                          "games_number" = "player_number",
                          "player_lastname_map")) %>%
        group_by(games_position) %>%
        summarize(across(c(player_age, player_market_value_in_million_euro,
                           player_rating_tm, games_minutes, games_rating, goals_total:cards_red),
                         ~ mean(.x, na.rm = TRUE))) %>%
        ungroup() %>%
        # insert the missing team id
        mutate(team_id = missing_team_id) %>%
        # reorder the values
        select(team_id, everything())
      
      
      # compute aggregated values for the transfers grouped by team and position
      imputed_lineups_transfers_all_teams <- curr_lineups_tm %>%
        group_by(player_pos_API_to_map) %>%
        summarize(across(c(player_is_new_transfer, player_is_new_winter_transfer,
                           player_is_returnee),
                         ~ median(.x, na.rm = TRUE))) %>%
        ungroup() %>%
        # insert the missing team id
        mutate(team_id = missing_team_id) %>%
        # reorder the values
        select(team_id, everything()) %>%
        # drop unwanted positions
        filter(player_pos_API_to_map %in% c("G", "D", "M", "F"))
      
      
      # bind the two frames together
      imputed_lineups_complete <- imputed_lineups_stats_all_teams %>%
        inner_join(imputed_lineups_transfers_all_teams,
                   by = c("team_id", 
                          "games_position" = "player_pos_API_to_map"))
      
      matched_lineups_stats <- matched_players %>%
        inner_join(matched_players_stats,
                   by = c("team_name", "team_id", "player_id")) %>%
        group_by(team_id, games_position) %>%
        summarize(across(c(player_age, player_market_value_in_million_euro,
                           player_rating_tm, games_minutes, games_rating, goals_total:cards_red),
                         ~ mean(.x, na.rm = TRUE))) %>%
        ungroup()
      
      # compute aggregated values for the transfers grouped by team and position
      matched_lineups_transfers <- matched_players %>%
        inner_join(matched_players_stats,
                   by = c("team_name", "team_id", "player_id")) %>%
        distinct(player_id, .keep_all = TRUE) %>%
        group_by(team_id, games_position) %>%
        summarize(across(c(player_is_new_transfer, player_is_new_winter_transfer,
                           player_is_returnee),
                         ~ sum(.x, na.rm = TRUE))) %>%
        ungroup()
      
      # bind the two frames together
      matched_lineups_complete <- matched_lineups_stats %>%
        inner_join(matched_lineups_transfers,
                   by = c("team_id", "games_position"))
      
      # check if there is a team missing
      if(nrow(matched_lineups_complete) < 8){
        matched_lineups_complete <- bind_rows(matched_lineups_complete,
                                              imputed_lineups_complete)
      }
    } else {
      missing_pos_home <- setdiff(positions, positions_home$games_position) %>%
        data.frame("pos" = .) %>%
        mutate(team_id = unique(positions_home$team_id))
      
      missing_pos_away <- setdiff(positions, positions_away$games_position) %>%
        data.frame("pos" = .) %>%
        mutate(team_id = unique(positions_away$team_id))
      
      if(nrow(missing_pos_home) > 0 | nrow(missing_pos_away) > 0){
        # prepare the linups tm data
        curr_lineups_tm <- curr_lineups_tm %>%
          # split the name by spaces
          mutate(player_lastname2 = str_split(player_name, " "))  %>%
          filter(league == league_name)
        
        # now get from the string split only the last element
        curr_lineups_tm$player_lastname2 <-
          sapply(curr_lineups_tm$player_lastname2,
                 tail, 1)
        
        curr_lineups_tm <- curr_lineups_tm %>%
          mutate(# map all special characters to plain ones
            player_lastname_map = stri_trans_general(player_lastname2, id = "Latin-ASCII"),
            # map transfermarkt positions to match API positions
            player_pos_API_to_map = ifelse(str_detect(player_pos, pattern = "Goal"),
                                           "G",
                                           ifelse(str_detect(player_pos, pattern = "Back|Sweeper"),
                                                  "D",
                                                  ifelse(str_detect(player_pos, pattern = "Midfield"),
                                                         "M",
                                                         ifelse(str_detect(player_pos, pattern = "Winger|Striker|Forward"),
                                                                "F",
                                                                player_pos)))),
            # adjust the nationalities such that we can map them later
            player_nationality = ifelse(player_nationality == "United States",
                                        "USA",
                                        player_nationality))
        
        # average the stats over the whole season in the league for each position
        imputed_lineups_stats_all_teams <- player_stats %>%
          inner_join(curr_lineups_tm,
                     by = c("team_name" = "team", 
                            "games_number" = "player_number",
                            "player_lastname_map")) %>%
          group_by(games_position) %>%
          summarize(across(c(player_age, player_market_value_in_million_euro,
                             player_rating_tm, games_minutes, games_rating, goals_total:cards_red),
                           ~ mean(.x, na.rm = TRUE))) %>%
          ungroup()
        
        
        # compute aggregated values for the transfers grouped by team and position
        imputed_lineups_transfers_all_teams <- curr_lineups_tm %>%
          group_by(player_pos_API_to_map) %>%
          summarize(across(c(player_is_new_transfer, player_is_new_winter_transfer,
                             player_is_returnee),
                           ~ median(.x, na.rm = TRUE))) %>%
          ungroup() %>%
          # drop unwanted positions
          filter(player_pos_API_to_map %in% c("G", "D", "M", "F")) 
        
        
        # bind the two frames together
        imputed_lineups_complete <- imputed_lineups_stats_all_teams %>%
          inner_join(imputed_lineups_transfers_all_teams,
                     by = c("games_position" = "player_pos_API_to_map")) %>%
          # filter only for the needed position
          filter(games_position %in% c(missing_pos_home$pos, missing_pos_away$pos))
        
        imputed_values_home <- imputed_lineups_complete %>%
          filter(games_position %in% missing_pos_home$pos) %>%
          # insert the missing team id
          mutate(team_id = unique(missing_pos_home$team_id)) %>%
          # reorder the values
          select(team_id, everything())
        
        imputed_values_away <- imputed_lineups_complete %>%
          filter(games_position %in% missing_pos_away$pos) %>%
          # insert the missing team id
          mutate(team_id = unique(missing_pos_away$team_id)) %>%
          # reorder the values
          select(team_id, everything())
        
        imputed_values_total <- bind_rows(imputed_values_home,
                                          imputed_values_away)
        
        
        matched_lineups_stats <- matched_players %>%
          inner_join(matched_players_stats,
                     by = c("team_name", "team_id", "player_id")) %>%
          group_by(team_id, games_position) %>%
          summarize(across(c(player_age, player_market_value_in_million_euro,
                             player_rating_tm, games_minutes, games_rating, goals_total:cards_red),
                           ~ mean(.x, na.rm = TRUE))) %>%
          ungroup()
        
        # compute aggregated values for the transfers grouped by team and position
        matched_lineups_transfers <- matched_players %>%
          inner_join(matched_players_stats,
                     by = c("team_name", "team_id", "player_id")) %>%
          distinct(player_id, .keep_all = TRUE) %>%
          group_by(team_id, games_position) %>%
          summarize(across(c(player_is_new_transfer, player_is_new_winter_transfer,
                             player_is_returnee),
                           ~ sum(.x, na.rm = TRUE))) %>%
          ungroup()
        
        # bind the two frames together
        matched_lineups_complete <- matched_lineups_stats %>%
          inner_join(matched_lineups_transfers,
                     by = c("team_id", "games_position"))
        
        # check if there is a team missing
        if(nrow(matched_lineups_complete) < 8){
          matched_lineups_complete <- bind_rows(matched_lineups_complete,
                                                imputed_values_total) %>%
            arrange(team_id, games_position)
        }
      } else {
      # if there are players that did not match we want to impute their values
      # based on the lineups of their team on the given position
      matched_lineups_stats <- matched_players %>%
        inner_join(matched_players_stats,
                   by = c("team_name", "team_id", "player_id")) %>%
        group_by(team_id, games_position) %>%
        summarize(across(c(player_age, player_market_value_in_million_euro,
                           player_rating_tm, games_minutes, games_rating, goals_total:cards_red),
                         ~ mean(.x, na.rm = TRUE))) %>%
        ungroup()
      
      # compute aggregated values for the transfers grouped by team and position
      matched_lineups_transfers <- matched_players %>%
        inner_join(matched_players_stats,
                   by = c("team_name", "team_id", "player_id")) %>%
        distinct(player_id, .keep_all = TRUE) %>%
        group_by(team_id, games_position) %>%
        summarize(across(c(player_is_new_transfer, player_is_new_winter_transfer,
                           player_is_returnee),
                         ~ sum(.x, na.rm = TRUE))) %>%
        ungroup()
      
      
      # bind the two frames together
      matched_lineups_complete <- matched_lineups_stats %>%
        inner_join(matched_lineups_transfers,
                   by = c("team_id", "games_position"))
      
      
      # imputed values for the missing players
      imputed_players <- NULL
      
      # iterate over all missing players
      for(i in 1:nrow(not_matched_players)){
        # get the imputed values for the current missing player
        curr_imputed_values <- matched_lineups_stats %>%
          filter(team_id == not_matched_players$team_id[i],
                 games_position == not_matched_players$player_pos[i]) %>%
          # drop the team id
          select(-team_id)
        
        # bind the player information to the imputed stats
        curr_imputed_player <- bind_cols(not_matched_players[i, ],
                                         curr_imputed_values)# %>%
        # select only columns we need
        #select(team_id:cards_red)
        
        # bind the data together
        imputed_players <- bind_rows(imputed_players,
                                     curr_imputed_player)
      }
      
      # compute aggregated values for the stats grouped by team and position
      matched_lineups_stats <- matched_players %>%
        inner_join(matched_players_stats,
                   by = c("team_name", "team_id", "player_id")) %>%
        bind_rows(imputed_players) %>%
        group_by(team_id, games_position) %>%
        summarize(across(c(player_age, player_market_value_in_million_euro,
                           player_rating_tm, games_minutes, games_rating, goals_total:cards_red),
                         ~ mean(.x, na.rm = TRUE))) %>%
        ungroup()
      
      # compute aggregated values for the transfers grouped by team and position
      matched_lineups_transfers <- matched_players %>%
        inner_join(matched_players_stats,
                   by = c("team_name", "team_id", "player_id")) %>%
        distinct(player_id, .keep_all = TRUE) %>%
        group_by(team_id, games_position) %>%
        summarize(across(c(player_is_new_transfer, player_is_new_winter_transfer,
                           player_is_returnee),
                         ~ sum(.x, na.rm = TRUE))) %>%
        ungroup()
      
      # bind the two frames together
      matched_lineups_complete <- matched_lineups_stats %>%
        inner_join(matched_lineups_transfers,
                   by = c("team_id", "games_position"))
    
      }
    }

  }

  

  # map the positions of the lineups
  matched_lineups_complete <- matched_lineups_complete %>%
    # rename the positions to get nice column names
    mutate(games_position = ifelse(games_position == "G",
                                   "Goal",
                                   ifelse(games_position == "D",
                                          "Def",
                                          ifelse(games_position == "M",
                                                 "Mid",
                                                 ifelse(games_position == "F",
                                                        "Att",
                                                        games_position)))))
  
  # map the positions of the lineups
  current_game_lineup <- current_game_lineup %>%
  # rename the positions to get nice column names
  mutate(player_pos = ifelse(player_pos == "G",
                                 "Goal",
                                 ifelse(player_pos == "D",
                                        "Def",
                                        ifelse(player_pos == "M",
                                               "Mid",
                                               ifelse(player_pos == "F",
                                                      "Att",
                                                      player_pos)))))
  
 
  
  # also compute the number of defenders, midfielders and attackers
  # as a replacement for the formation
  home_formation_info <- current_game_lineup %>%
    filter(team_id == home_id,
           is_starting_grid == TRUE) %>%
    group_by(player_pos) %>%
    count() %>%
    # drop the goalkeeper
    filter(player_pos != "Goal") %>%
    # convert into one line
    pivot_wider(names_from = player_pos,
                values_from = n,
                names_glue = "home_number_{player_pos}")
  
  # extract the home team values
  home_lineups_stats <- matched_lineups_complete %>%
    filter(team_id == home_id) %>%
    # convert it into a one liner
    pivot_wider(names_from = c(team_id, games_position),
                values_from = c(player_age:player_is_returnee),
                names_glue = "home_{games_position}_{.value}") %>%
    # bind the number of positions to it
    bind_cols(., home_formation_info)
  
  
  
  # also compute the number of defenders, midfielders and attackers
  # as a replacement for the formation
  away_formation_info <- current_game_lineup %>%
    filter(team_id != home_id,
           is_starting_grid == TRUE) %>%
    group_by(player_pos) %>%
    count() %>%
    # drop the goalkeeper
    filter(player_pos != "Goal") %>%
    # convert into one line
    pivot_wider(names_from = player_pos,
                values_from = n,
                names_glue = "away_number_{player_pos}")
  
  
  # extract the away team values
  away_lineups_stats <- matched_lineups_complete %>%
    filter(team_id != home_id) %>%
    # convert it into a one liner
    pivot_wider(names_from = c(team_id, games_position),
                values_from = c(player_age:player_is_returnee),
                names_glue = "away_{games_position}_{.value}") %>%
    # bind the number of positions to it
    bind_cols(., away_formation_info)
  
  
  
  # bind the whole data frame together
  lineup_stats_combined <- bind_cols(home_lineups_stats,
                                     away_lineups_stats)
  
  return(lineup_stats_combined)
}



# this function should call the function above on all historical matches
# to decrease the training time for the lineup model
historical_lineup_data <- function(){
  # get all the historical matches
  historical_matches <- all_leagues_matches %>%
    filter(league_id == 39,
           league_season >= 2016,
           fixture_date <= Sys.Date(),
           status_long == "Match Finished") %>%
    select(fixture_id) %>%
    pull() %>% 
    unique()
  
  # all past matches lineup data
  historical_lineup_data_pl <- NULL
  missing_lineups_pl <- NULL
  
  # for all of these matches iterate over the ids and use the prepare_lineups_function
  for(i in 1:length(historical_matches)){
    print(i)
    print(paste0("Current iteration: ", i))
    print(paste0("Current id: ", historical_matches[i]))
    
    # curr_lineup <- prepare_lineup_data(historical_matches[i])
    curr_lineup <- prepare_lineup_data_subset(historical_matches[i])
    
    if(is.null(curr_lineup)){
      next
    }
    if(ncol(curr_lineup) < 238){
      missing_lineups_pl <- bind_rows(missing_lineups_pl,
                                   curr_lineup)
    } else {
      # bind the data together
      historical_lineup_data_pl <- bind_rows(historical_lineup_data_pl,
                                          curr_lineup)
    }
    
    if(i %% 300 == 0){
      save(historical_lineup_data_pl, file = paste0("historical_lineup_data_pl_", i, ".RData"))
      save(missing_lineups_pl, file = paste0("missing_lineups_pl_", i, ".RData"))
    }
    
    
  }
  
  # write the historical data in the data base
  # dbWriteTable(con, "all_leagues_historical_lineups_data",
  #              historical_lineup_data)
  
  
}




prepare_lineup_data_subset <- function(match_id, con){
  # get the lineups from the data base
  current_game_lineup <- tbl(con, "all_leagues_fixture_lineups") %>%
    data.frame() %>%
    filter(fixture_id == match_id) %>%
    # prepare the name of the players to map the API and the TM data
    mutate(player_lastname = str_remove(player_name, pattern = ".*\\. |.*\\s"),
           # split the name by spaces
           player_lastname2 = str_split(player_lastname, " "))
  
  if(nrow(current_game_lineup) == 0){
    return(NULL)
  }
  
  home_id <- unique(current_game_lineup$club_id_home)
  away_id <- unique(current_game_lineup$club_id_away)
  
  # extract the league name
  league_name <- unique(current_game_lineup$league_name)
  
  # now get from the string split only the last element
  current_game_lineup$player_lastname2 <- sapply(current_game_lineup$player_lastname2,
                                                 tail, 1)
  
  # create the lastname for mapping by removing special characters
  current_game_lineup <- current_game_lineup %>%
    mutate( 
      # map all special characters to plain ones
      player_lastname_map = stri_trans_general(player_lastname2, id = "Latin-ASCII")
    )
  
  # if it is a relegation game we just want to return NULL
  if(is.na(unique(current_game_lineup$league_round))){
    return(NULL)
  }
  
  # check if it is the first matchday of the season because then we 
  # have to take the values of the last season as reference
  if(unique(current_game_lineup$league_round) == 1){
    # search for the players in the player stats
    player_stats <- all_leagues_player_stats %>%
      filter(league_season == (unique(current_game_lineup$league_season) - 1),
             league_round >= unique(current_game_lineup$league_round))#,
             # player_id %in% current_game_lineup$player_id,
             # team_id %in% current_game_lineup$team_id)
  } else {
    # search for the players in the player stats
    player_stats <- all_leagues_player_stats %>%
      filter(league_season == unique(current_game_lineup$league_season),
             league_round < unique(current_game_lineup$league_round))#,

      
  }
  
  # add the formation info (number defenders, midfielders, attackers)
  formation_data_home <- current_game_lineup %>% 
    filter(team_id == home_id,
           is_starting_grid == TRUE) %>%
    group_by(player_pos) %>%
    count() %>%
    # drop the goalkeeper
    filter(player_pos != "G") %>%
    # convert into one line
    pivot_wider(names_from = player_pos,
                values_from = n,
                names_glue = "home_number_{player_pos}")
  
  
  formation_data_away <- current_game_lineup %>% 
    filter(team_id == away_id,
           is_starting_grid == TRUE) %>%
    group_by(player_pos) %>%
    count() %>%
    # drop the goalkeeper
    filter(player_pos != "G") %>%
    # convert into one line
    pivot_wider(names_from = player_pos,
                values_from = n,
                names_glue = "away_number_{player_pos}")
  
  # compute the aggregated values for both teams for each position
  aggregated_values_pos <- player_stats %>% 
    filter(player_id %in% current_game_lineup$player_id,
           team_id %in% current_game_lineup$team_id) %>%
    group_by(team_id, games_position) %>%
    summarize(across(c(games_minutes, games_rating, goals_total:cards_red),
                     ~ mean(.x, na.rm = TRUE))) %>%
    ungroup()
  
  # drop all positions that are not in the normal "G", "D", "M", "F"
  # available positions
  positions <- c("G", "D", "M", "F")

  aggregated_values_pos <- aggregated_values_pos %>%
    filter(games_position %in% positions)
  
  # there should be 8 rows (1 per team per position (G, D, M, F))
  if(nrow(aggregated_values_pos) < 8){
    
    # check if there is only one team id, i.e., one team is missing
    # then we want to impute the values for this team
    if(length(unique(aggregated_values_pos$team_id)) == 1){
      missing_id <- ifelse(home_id %in% aggregated_values_pos$team_id,
                           away_id, home_id)
      
      # compute the aggregated values for over the whole seasons
      # of all teams as replacement for the missing teams
      aggregated_values_pos_missing <- player_stats %>% 
        group_by(games_position) %>%
        summarize(across(c(games_minutes, games_rating, goals_total:cards_red),
                         ~ mean(.x, na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(team_id = missing_id)
      
      # bind the two team values together
      aggregated_values_pos <- bind_rows(aggregated_values_pos,
                                         aggregated_values_pos_missing)
      
      
      # if there is no team in the aggregated_values_pos we need to impute
      # the values for both teams
    } else if(length(unique(aggregated_values_pos$team_id)) == 0){
      
      # compute the aggregated values for over the whole seasons
      # of all teams as replacement for the missing teams
      aggregated_values_pos_missing_home <- player_stats %>% 
        group_by(games_position) %>%
        summarize(across(c(games_minutes, games_rating, goals_total:cards_red),
                         ~ mean(.x, na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(team_id = home_id)
      
      aggregated_values_pos_missing_away <- player_stats %>% 
        group_by(games_position) %>%
        summarize(across(c(games_minutes, games_rating, goals_total:cards_red),
                         ~ mean(.x, na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(team_id = away_id)
      
      # bind the two team values together (in this case basically 
      # add exactly the same data to the frame)
      aggregated_values_pos <- bind_rows(aggregated_values_pos,
                                         aggregated_values_pos_missing_home,
                                         aggregated_values_pos_missing_away)
      
      # otherwise there have to be missing positions (but not the whole team)
    }
    
    if(nrow(aggregated_values_pos) < 8){
      
      # check which position is missing for which team
      positions_per_team <- aggregated_values_pos %>%
        group_by(team_id) %>%
        # filter for these teams that have missing positions
        filter(n_distinct(games_position) < 4)
      
      
      missing_pos_home <- positions_per_team %>%
        filter(team_id == home_id)
      
      if(nrow(missing_pos_home) != 0){
        missing_pos_home <- setdiff(positions, missing_pos_home$games_position) %>%
          data.frame("pos" = .) %>%
          mutate(team_id = home_id)
      }
        
      missing_pos_away <- positions_per_team %>%
        filter(team_id == away_id)
      
      if(nrow(missing_pos_away) != 0){
        missing_pos_away <- setdiff(positions, missing_pos_away$games_position) %>%
          data.frame("pos" = .) %>%
          mutate(team_id = away_id)
      }
      
      
      # compute the aggregated values for over the whole seasons
      # of all teams as replacement for the missing teams
      aggregated_values_pos_missing <- player_stats %>% 
        group_by(games_position) %>%
        summarize(across(c(games_minutes, games_rating, goals_total:cards_red),
                         ~ mean(.x, na.rm = TRUE))) %>%
        ungroup()
      
      
      # if there are missing positions we want to filter the respective position
      # out 
      aggregated_values_pos_missing_home <- NULL
      aggregated_values_pos_missing_away <- NULL
      
      # join the data to it
      if(nrow(missing_pos_home) != 0){
        aggregated_values_pos_missing_home <- aggregated_values_pos_missing %>%
          inner_join(missing_pos_home, by = c("games_position" = "pos")) %>%
          select(team_id, everything())
      }
      
      # join the data to it
      if(nrow(missing_pos_away) != 0){
        aggregated_values_pos_missing_away <- aggregated_values_pos_missing %>%
          inner_join(missing_pos_away, by = c("games_position" = "pos")) %>%
          select(team_id, everything())
      }
      
      # based on the content of the home and away missings we bind the
      # data together
      if(!is.null(aggregated_values_pos_missing_home) & 
         !is.null(aggregated_values_pos_missing_away)){
        
        aggregated_values_pos_missings <- bind_rows(aggregated_values_pos_missing_home,
                                                    aggregated_values_pos_missing_away)
      } else if(!is.null(aggregated_values_pos_missing_home) &
                is.null(aggregated_values_pos_missing_away)){
        aggregated_values_pos_missings <- aggregated_values_pos_missing_home
      } else {
        aggregated_values_pos_missings <- aggregated_values_pos_missing_away
      }
      
      
      # finally, bind the computed data with the data of the both teams
      aggregated_values_pos <- bind_rows(aggregated_values_pos,
                                         aggregated_values_pos_missings) %>%
        arrange(team_id, games_position)
      
      
    }
  }
  
  
  # filter the positions to only contain "G", "D", "M", "F"
  aggregated_values_pos <- aggregated_values_pos %>%
    filter(games_position %in% positions)
  
  
  # extract the away team values
  home_lineups_stats <- aggregated_values_pos %>%
    filter(team_id == home_id) %>%
    # convert it into a one liner
    pivot_wider(names_from = c(team_id, games_position),
                values_from = c(games_minutes:cards_red),
                names_glue = "home_{games_position}_{.value}") %>%
    # bind the number of positions to it
    bind_cols(., formation_data_home)
  
  
  # extract the away team values
  away_lineups_stats <- aggregated_values_pos %>%
    filter(team_id == away_id) %>%
    # convert it into a one liner
    pivot_wider(names_from = c(team_id, games_position),
                values_from = c(games_minutes:cards_red),
                names_glue = "away_{games_position}_{.value}") %>%
    # bind the number of positions to it
    bind_cols(., formation_data_away)
  
  
  
  # bind the whole data frame together
  lineup_stats_combined <- bind_cols(home_lineups_stats,
                                     away_lineups_stats) %>%
    # add a variable for the fixture id 
    mutate(fixture_id = match_id)
  
  
  return(lineup_stats_combined)
  
}



# function should return TRUE if the players nationality is in the current
# top 15 nationalities by the fifa rating
prepare_fifa15_lineup <- function(player_nationality, current_date){
  # check if the player_nationality is in the top15 nationality
  current_fifa_top_15 <- fifa_top_15_nations %>%
    # compute the difference in days between the given date and the 
    # last date the data in the fifa_top_15 frame was updated
    mutate(diff_to_date = as.difftime(ymd(current_date) - date, units = "days")) %>%
    # drop diffs with negative value because that means that the date is in the future
    filter(diff_to_date > 0) %>%
    # get the data with the smallest difference, i.e., the newest data
    filter(diff_to_date == min(diff_to_date)) %>%
    select(nation) %>%
    pull()
  
  # now check if the given nationality is in the current fifa top 15
  # set the variable is_fifa_top_15 to TRUE if we find the nationality in
  # the top 15 nationalities and FALSE otherwise
  is_fifa_top_15 <- ifelse(player_nationality %in% current_fifa_top_15,
                           TRUE, FALSE)
  
  
  return(is_fifa_top_15)
  
}


