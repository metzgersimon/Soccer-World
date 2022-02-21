# function should prepare the venue data frame to be able to be used in the model.
prepare_venue_data <- function(){
  venues <- all_leagues_venue_information %>%
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
prepare_team_match_stats_historical <- function(){
  match_stats_historical <- all_leagues_fixture_stats %>%
    # get only past seaons and remove matchdays that are not the regular season,
    # i.e., relegation matches
    filter(season >= 2016,
           !is.na(matchday)) %>%
    # group by the league and team id
    group_by(league_id, team_id) %>%
    # because we do not have the stats for e.g. matchday 1 at matchday 1
    # we lag the variables we consider for prediction with a n of 1
    mutate(across(c(shots_on_goal:passing_accuracy),
                  ~lag(.x, n = 1))) %>%
    # after that we calculate a cumulative running mean with a window of 2,
    # i.e., 2 matches
    group_by(league_id, season, team_id) %>%
    mutate(across(c(shots_on_goal:passing_accuracy),
                  ~rollmean(.x, k = 2, fill = NA))) %>%
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
    filter(league_season >= 2016) %>%
    select(league_id, league_name, league_season, league_round,
           fixture_id, fixture_date, fixture_time)
  
  # prepare the all leagues fixture stats
  all_leagues_fixture_stats_prep <- all_leagues_fixture_stats %>%
    filter(season >= 2016,
           !is.na(matchday)) %>%
    # group by the league and team id
    group_by(league_id, team_id) %>%
    # because we do not have the stats for e.g. matchday 1 at matchday 1
    # we lag the variables we consider for prediction with a n of 1
    mutate(across(c(shots_on_goal:passing_accuracy),
                  ~lag(.x, n = 1))) %>%
    # after that we calculate a cumulative running mean with a window of 2,
    # i.e., 2 matches
    group_by(league_id, season, team_id) %>%
    mutate(across(c(shots_on_goal:passing_accuracy),
                  ~rollmean(.x, k = 2, fill = NA)))
  
  
  # join the league name from the all_leagues_matches_matcher to
  # be able to join the fifa stats later on
  all_leagues_fixture_stats2 <- all_leagues_fixture_stats_prep %>% 
    distinct() %>%
    left_join(all_leagues_matches_matcher,
              by = c("league_id", 
                     "season" = "league_season",
                     "matchday" = "league_round",
                     "fixture_date",
                     "fixture_time",
                     "fixture_id")) %>%
    # drop relegation games
    filter(!is.na(matchday))
  
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
  
  return(fifa_stats_joined)
  
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

  
  # based on whether home or away is given we rename the features
  # accordingly
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




prepare_lineup_data_subset <- function(match_id, all_leagues_fixture_lineups){
  # get the lineups from the data base
  current_game_lineup <- all_leagues_fixture_lineups %>%
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
  # formation_data_home <- current_game_lineup %>% 
  #   filter(team_id == home_id,
  #          is_starting_grid == TRUE) %>%
  #   group_by(player_pos) %>%
  #   count() %>%
  #   # drop the goalkeeper
  #   filter(player_pos != "G") %>%
  #   # convert into one line
  #   pivot_wider(names_from = player_pos,
  #               values_from = n,
  #               names_glue = "home_number_{player_pos}")
  # 
  # 
  # formation_data_away <- current_game_lineup %>% 
  #   filter(team_id == away_id,
  #          is_starting_grid == TRUE) %>%
  #   group_by(player_pos) %>%
  #   count() %>%
  #   # drop the goalkeeper
  #   filter(player_pos != "G") %>%
  #   # convert into one line
  #   pivot_wider(names_from = player_pos,
  #               values_from = n,
  #               names_glue = "away_number_{player_pos}")
  
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
    filter(games_position %in% positions,
           games_position != "-")
  
  
  # extract the away team values
  home_lineups_stats <- aggregated_values_pos %>%
    filter(team_id == home_id) %>%
    # convert it into a one liner
    pivot_wider(names_from = c(team_id, games_position),
                values_from = c(games_minutes:cards_red),
                names_glue = "home_{games_position}_{.value}") #%>%
    # bind the number of positions to it
    # bind_cols(., formation_data_home)
  
  
  # extract the away team values
  away_lineups_stats <- aggregated_values_pos %>%
    filter(team_id == away_id) %>%
    # convert it into a one liner
    pivot_wider(names_from = c(team_id, games_position),
                values_from = c(games_minutes:cards_red),
                names_glue = "away_{games_position}_{.value}") #%>%
    # bind the number of positions to it
    # bind_cols(., formation_data_away)
  
  
  
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


