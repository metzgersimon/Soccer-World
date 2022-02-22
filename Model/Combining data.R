

get_model_data <- function(){
  spi_data <- prepare_spi_data()
  # add a goal_diff variable to the data
  spi_data <- spi_data %>%
    mutate(goal_diff = fulltime_score_home - fulltime_score_away)
  
  fixture_stats <- prepare_match_stats()
  # 
  # # fifa_team_stats <- prepare_fifa_team_stats() %>%
  #   convert_two_lines_into_one(., columns_to_drop = c("team_logo"),
  #                              join_columns = c("fixture_id", "fixture_date",
  #                                               "league_id", "league_name",
  #                                               "league_season", "fixture_time")) %>%
  # #   # drop unnecessary columns
   
  
  # fifa_team_stats <- prepare_fifa_team_stats() %>%
  #   select(-c(league_id_away, league_season_away, league_round_away, fixture_date_away,
  #             fixture_time_away, date_minus1_home, date_home, fifa_vers_home,
  #             league_home, club_home, date_minus1_away, date_away, fifa_vers_away,
  #             league_away, club_away)) %>%
  #   select(league_id = league_id_home, league_season = league_season_home,
  #          league_name = league_name_home,
  #          league_round = league_round_home, fixture_date = fixture_date_home,
  #          fixture_time = fixture_time_home, everything()) %>%
  #   unique()
  
  all_leagues_seasons_running_tables_home <- get_league_ranking_overall(type = "home")
  all_leagues_seasons_running_tables_away <- get_league_ranking_overall(type = "away")
  
  all_leagues_seasons_win_pct_home <- get_winning_pcts(type = "home")
  all_leagues_seasons_win_pct_away <- get_winning_pcts(type = "away")
  

  
  all_leagues_club_stats_model <- prepare_club_stats()
  
  stats_combined <- fixture_stats %>%
    left_join(all_leagues_club_stats_model, by = c("league_id", "league_name",
                                                   "league_season", "league_round" = "matchday",
                                                   "team_id"),
              suffix = c("_home", "_away"))
  
  
  model_data_home <- spi_data %>%
    select(-c(club_id_away, team_name_away)) %>%
    left_join(all_leagues_seasons_running_tables_home, 
               by = c("league_season",
                      "league_round" = "league_round",
                      "league_id",
                      "club_id_home" = "team_id")) %>%
    left_join(all_leagues_seasons_win_pct_home,
               by = c("league_season",
                      "league_id",
                      "league_round",
                      "club_id_home" = "club_id")) %>%
    filter(!is.na(league_round))
  
  
  model_data_away <- spi_data %>%
    select(-c(club_id_home, team_name_home)) %>%
    left_join(all_leagues_seasons_running_tables_away, 
               by = c("league_season",
                      "league_round" = "league_round",
                      "league_id",
                      "club_id_away" = "team_id")) %>%
    left_join(all_leagues_seasons_win_pct_away,
               by = c("league_season",
                      "league_id",
                      "league_round",
                      "club_id_away" = "club_id")) %>%
    filter(!is.na(league_round)) %>%
    # drop unwanted columns
    select(fixture_id.x, fixture_date.x, league_id, league_name.x, league_season,
           league_round, club_id_away, team_name_away, spi_home:adjusted_goals_home,
           spi_away:goal_diff, away_team_points,
           away_team_goals_against, away_team_goal_diff, away_team_rank,
           home_played:away_team_loss_pct)  %>%
    select(-contains("home"))
  
  
  model_data_all <- inner_join(model_data_home, model_data_away,
                               by = c("fixture_id.x", "fixture_date.x",
                                      "league_id", "league_name.x",
                                      "league_season", "league_round"))
  
  
  
  # join the spi and match data with the stats (fifa stats, club stats, match stats)
  model_data_all2 <- model_data_all %>%
    left_join(stats_combined, by = c("fixture_id")) %>%
    unique() %>%
    mutate(across())
  
  return(model_data_all2)
  
}



get_model_data_lineups <- function(){
  # get the data of the plain model
  plain_model_data <- get_model_data()
  # get the aggregated lineup data from the data base
  aggregated_lineup_stats <- tbl(con, "all_leagues_lineups_agg") %>%
    data.frame()
  
  # join the two data sets together
  lineup_model_data <- plain_model_data %>%
    inner_join(aggregated_lineup_stats, by = "fixture_id")
  
  # drop unnecessary columns
  
  return(lineup_model_data)
}
