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
    filter(season < max(season),
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
  
  all_leagues_fixture_stats <- all_leagues_fixture_stats %>% distinct()
  
  min_date_team_stats <- min(all_leagues_fifa_team_stats$date)
  max_date_team_stats <- max(all_leagues_fifa_team_stats$date)
  
  daily_seq <- seq(as.Date(min_date_team_stats), as.Date(max_date_team_stats), "days") %>%
    data.frame("date" = .)
  
  daily_seq_with_stats <- left_join(daily_seq,
                                    all_leagues_fifa_team_stats,
                                    by = "date") %>%
    group_by(date, fifa_vers, league, club) %>%
    fill(everything())
  
  daily_seq_with_stats_filled <- na.locf(daily_seq_with_stats %>% group_by(league, club), fromLast = TRUE)
  
  
  test <- all_leagues_fixture_stats %>%
    left_join(all_leagues_fifa_team_stats, 
               by = c("team_name" = "club"),
               keep = TRUE) %>%
    mutate(dateDiff = fixture_date - date) %>%
    filter(dateDiff > 0) %>%
    group_by(team_name, fixture_date) %>%
    filter(dateDiff == min(dateDiff)) %>%
    ungroup()
    distinct() %>%
    ungroup()
  

  test2 <- test %>%
    left_join(all_leagues_fifa_squads, 
               by = c("team_name" = "team",
                      "date")) %>%
    mutate(dateDiff2 = fixture_date - date) %>%
    filter(dateDiff2 > 0) %>%
    group_by(team_name, fixture_date) %>%
    filter(dateDiff2 == min(dateDiff2)) %>%
    distinct() %>%
    ungroup()
  
  test2_not_matched <- test %>%
    anti_join(all_leagues_fifa_squads, 
              by = c("team_name" = "team",
                     "date"))
  
  
  # data.table solution because the dplyr one needs to much memory
  setDT(test)
  setDT(all_leagues_fifa_squads)
  
  setkey(test, team, date)[, dateMatch:=date]
  setkey(all_leagues_fifa_squads, team, date)[, dateMatch:=date]
  test2 <- all_leagues_fifa_squads[test, roll = 'nearest']
  
  # setkey(test, team_name, date)
  # setkey(all_leagues_fifa_squads, team, date)
  # test2 <- test[all_leagues_fifa_squads, roll = 'nearest']
  
  

  
  
  
  setDT(test)
  setDT(all_leagues_fifa_squads)
  #convert dates to 'real' dates
  test[, date := as.IDate(date) ]
  all_leagues_fifa_squads[, date := as.IDate(date) ]
  #update df1 by reference with a rolling join
  test[, randomVar := all_leagues_fifa_squads[test,  
                                               on = .(club, date), roll = Inf ] ]
  
    
  # abfangen bis zu welchem Datumz urück dates gematched werden
  # (mit club angleichen)

  
  club_names_sofifa <- unique(important_leagues_fifa_team_stats$club)
  club_names_api <- unique(all_leagues_fixture_stats$team_name)
  
  club_names_api_not_in_sofifa <- club_names_api[!(club_names_api %in% club_names_sofifa)]
  club_names_sofifa_not_in_api <- club_names_sofifa[!(club_names_sofifa %in% club_names_api)]
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


get_winning_pcts <- function(type = "home"){
  # filter all the matches in all leagues for the selected league and season
  # league_season_matches <- all_leagues_matches %>%
  #   filter(league_id == league,
  #          league_season == season)
  leagues_matches <- all_leagues_matches %>%
    filter(fixture_date < Sys.Date(),
           league_season >= 2015)
  
  # get the winning/draw/lose percentages for the team playing at home
  home_table <- leagues_matches %>%
    # group by the league and season
    group_by(league_id, league_season) %>%
    # rename the variables according to the home team
    select(league_round, club_name = club_name_home,
           points = home_points, goals = fulltime_score_home, 
           goals_against = fulltime_score_away) %>%
    # add a variable for the potential points a team could have scored at this
    # point
    mutate(potential_points = 3) %>%
    # group by league, the club and the season to compute the stats aggregated for each club
    # in each season
    group_by(league_id, club_name, league_season) %>%
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
    select(league_round, club_name = club_name_away,
           points = away_points, goals = fulltime_score_away, 
           goals_against = fulltime_score_home) %>%
    # add a variable for the potential points a team could have scored at this
    # point
    mutate(potential_points = 3) %>%
    # group by league, the club and the season to compute the stats aggregated for each club
    # in each season
    group_by(league_id, club_name, league_season) %>%
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
    group_by(club_name, league_season) %>%
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
           away_loss_pct = (away_losses / away_played) * 100)
  
  
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
prepare_lineup_data <- function(match_id, max_season = 2021){
  # match_id <- 719494
  match_information <- all_leagues_matches %>%
    filter(fixture_id == match_id) %>%
    mutate(fixture_date = ymd(fixture_date)) %>%
    data.frame()
  
  # get the lineups from the API
  # live call to API to get the lineups for the soon to begin lineup
  lineups_API2 <- get_fixture_lineups(match_id) %>%
    # lineups_API2 <- lineups_API %>%
    # prepare the name of the players to map the API and the TM data
    mutate(player_lastname = str_remove(player_name, pattern = ".*\\. |.*\\s"),
           # map all special characters to plain ones
           player_lastname2 = stri_trans_general(player_lastname, id = "Latin-ASCII"))
  
  
  # check if the current matchday is the first matchday of the season
  # If it is the first matchday, we want to impute the data somehow
  if(match_information$league_round == 1){
    # in solchen fällen den durchschnittswert der letzten mannschaft (absteiger)
    lineups_tm <- buli_fixture_lineups_2015_2021_tm %>%
      filter(season == match_information$league_season - 1)
    # if it is not the first matchday, we get the current season
  } else {
    lineups_tm <- buli_fixture_lineups_2015_2021_tm %>%
      filter(season == match_information$league_season)
  }
  
  # search in the current season lineup data from transfermarkt for the
  # current teams (historical data)
  lineups_tm <- lineups_tm %>%
    filter(team %in% c(match_information$club_name_home,
                       match_information$club_name_away),
           matchday < match_information$league_round) %>%
    mutate(player_lastname = str_to_title(sub(".*?\\s", "", player_name)),
           # map all special characters to plain ones
           player_lastname2 = stri_trans_general(player_lastname, id = "Latin-ASCII"),
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
  not_matched_players <- lineups_API2 %>%
    anti_join(lineups_tm, by = c("team_name" = "team", "player_number",
                                 "player_lastname2")) 
  
  # we also want to join the all the other players
  matched_players <- lineups_API2 %>%
    # join by the team name, the player number and the (cleaned) lastname
    inner_join(lineups_tm, by = c("team_name" = "team", "player_number",
                                  "player_lastname2")) %>%
    # create team, number and lastname pairs to get the latest information
    # for every pair from transfermarkt
    group_by(team_name, player_number, player_lastname2) %>%
    filter(match_date == max(match_date)) %>%
    ungroup() %>%
    # do the final variable transformations and ordering
    rename(player_name_API = `player_name.x`,
           player_pos_API = `player_pos.x`,
           player_lastname_to_map = player_lastname2,
           player_name_TM = `player_name.y`,
           player_pos_TM = `player_pos.y`) %>%
    # create a new variable for the match id
    mutate(fixture_id = match_id)
  
  
  # find the player match stats that are relevant for the current match
  player_stats <- buli_player_fixture_stats %>%
    mutate(player_lastname = str_to_title(sub(".*?\\s", "", player_name)),
           # map all special characters to plain ones
           player_lastname_map = stri_trans_general(player_lastname, id = "Latin-ASCII")) %>%
    filter(league_season == match_information$league_season,
           league_round < match_information$league_round,
           team_name %in% c(match_information$club_name_home,
                            match_information$club_name_away))
  
  
  
  ############ bedeutet nur, dass er nicht in dem team ist mit der nummer
  
  # if not_matched_players is not empty, i.e., there is at least one player
  # that is now, i.e., not in the historical data of transfermarkt
  if(nrow(not_matched_players) != 0){
    missing_player_stats <- not_matched_players %>%
      inner_join(lineups_tm, by = c("team_name" = "team",
                                    "player_pos" = "player_pos_API"))
    ####### API STATS VERWENDEN, 
    
    
    
    # search for the player in the player_fixture_stats of the league
    ###### WIR BRAUCHEN AGGREGIERTE TM DATEN NICHT API
    missing_player_stats_TM <- lineups_tm %>%
      filter(player_lastname2 == not_matched_players$player_lastname2)
    
    
    missing_player_stats <- buli_player_fixture_stats %>%
      filter(league_season == match_information$league_season,
             player_id == not_matched_players$player_id) %>%
      filter(fixture_date == max(fixture_date))
    
    # if the missing_player_stats is empty that means that the player was not previously
    # playing in one of the leagues we consider. Therefore we impute its values 
    # with information about its current position and team
    if(nrow(missing_player_stats) == 0){
      # get the average player stats of the current team for every position
      missing_player_stats_imputed <- player_stats %>%
        group_by(team_id, games_position) %>%
        summarize(across(c(games_minutes, games_rating, goals_total:cards_red),
                         ~ median(.x, na.rm = TRUE))) %>% 
        ungroup()
      
      # join the data of players that did not match with the statistics we
      # imputed for those players from the team averages
      missing_player_stats <- not_matched_players %>%
        inner_join(missing_player_stats_imputed, 
                   by = c("team_id", 
                          "player_pos" = "games_position"))
      
    }
  }
  
  
  # 
  
  # now get the player stats for all players that are in the current lineup
  
  
  ####################### TO DO ###########################
  # lineup_tm daten ebenfalls imputen für spieler, die nicht vorhanden sind
  # und diese an die player_stats joinen
  # Stats für die lineups berechnen (missing_players) vor aggregierung einfügen
  player_stats_lineup <- player_stats %>%
    filter(player_id %in% matched_players$player_id) %>%
    ##### missing player einfügen
    group_by(team_id, games_position) %>%
    summarize(across(c(games_minutes, games_rating, goals_total:cards_red),
                     ~median(.x, na.rm = TRUE)))
  
  
  
  player_data_with_stats <- players_list2 %>%
    left_join(., player_agg_data_up_to_match,
              by = c("team_id",  "player_id")) %>%
    select(-c(#player_lastname, 
      player_lastname.x, player_lastname.y))
  
}


# function should return TRUE if the players nationality is in the current
# top 15 nationalities by the fifa rating
prepare_fifa15_lineup <- function(player_nationality, current_date){
  # check if the player_nationality is in the top15 nationality
  current_fifa_top_15 <- fifa_top_15_nations %>%
    # compute the difference in days between the given date and the 
    # last date the data in the fifa_top_15 frame was updated
    mutate(diff_to_date = as.difftime(ymd(current_date) - date, units = "days")) %>%
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


