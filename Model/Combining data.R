

get_model_data <- function(){
  spi_data <- prepare_spi_data()
  # add a goal_diff variable to the data
  spi_data <- spi_data %>%
    mutate(goal_diff = fulltime_score_home - fulltime_score_away)
  
  fifa_team_stats <- prepare_fifa_team_stats() %>%
    convert_two_lines_into_one(., columns_to_drop = c("team_logo",
                                                      "league_name", "league",
                                                      "fifa_vers", "date", "date_minus1"),
                               join_columns = c("league_id", "season",
                                                "matchday", "fixture_date",
                                                "fixture_time", "fixture_id")) %>%
    # drop unnecessary columns
    select(-c(team_logo, league, date, date_minus1, fifa_vers, club_home, 
              club_away)) %>%
    unique()
  league_ids <- c(78, 79, 39, 61)
  seasons <- c(2016:2021)
  
  all_leagues_seasons_running_tables_home <- get_league_ranking_overall(type = "home")
  all_leagues_seasons_running_tables_away <- get_league_ranking_overall(type = "away")
  
  all_leagues_seasons_win_pct_home <- get_winning_pcts(type = "home")
  all_leagues_seasons_win_pct_away <- get_winning_pcts(type = "away")
  

  
  all_leagues_club_stats_lagged <- all_leagues_club_stats %>%
    group_by(league_season, team_id) %>%
    # because we do not have the stats for e.g. matchday 1 at matchday 1
    # we lag the variables we consider for prediction with a n of 1
    mutate(across(c(fixtures_played_home:cards_red_106_120_percentage),
                  ~lag(.x, n = 1)))
  
  # join the club stats to the historical match stats
  stats_joined <- fifa_team_stats %>%
    inner_join(all_leagues_club_stats_lagged,
               by = c("league_id",
                      "season" = "league_season",
                      "matchday",
                      "team_id_home" = "team_id")) %>%
    inner_join(all_leagues_club_stats_lagged,
               by = c("league_id",
                      "season" = "league_season",
                      "matchday",
                      "team_id_away" = "team_id"),
               suffix = c("_home", "_away"))
  
  
  
  model_data_home <- spi_data %>%
    select(-c(club_id_away, club_name_away, club_logo_away)) %>%
    inner_join(all_leagues_seasons_running_tables_home, 
               by = c("league_season" = "season",
                      "league_round" = "league_round",
                      "league_id",
                      "club_name_home" = "club_name")) %>%
    inner_join(all_leagues_seasons_win_pct_home,
               by = c("league_season",
                      "league_id",
                      "league_round",
                      "club_id_home" = "club_id")) %>%
    filter(!is.na(league_round))
  
  
  model_data_away <- spi_data %>%
    select(-c(club_id_home, club_name_home, club_logo_home)) %>%
    inner_join(all_leagues_seasons_running_tables_away, 
               by = c("league_season" = "season",
                      "league_round" = "league_round",
                      "league_id",
                      "club_name_away" = "club_name")) %>%
    inner_join(all_leagues_seasons_win_pct_away,
               by = c("league_season",
                      "league_id",
                      "league_round",
                      "club_id_away" = "club_id")) %>%
    filter(!is.na(league_round)) %>%
    # drop unwanted columns
    select(fixture_id, club_id_away, club_name_away, away_team_points,
           away_team_goals_against, away_team_goal_diff, away_team_rank,
           home_played:away_team_loss_pct)
  
  
  model_data_all <- inner_join(model_data_home, model_data_away,
                               by = c("fixture_id"))
  
  
  
  # join the spi and match data with the stats (fifa stats, club stats, match stats)
  model_data_all2 <- model_data_all %>%
    inner_join(stats_joined, by = c("fixture_id"))
                                   #"league_season" = "season",
                                   #"league_round" = "matchday",
                                   #"club_id_home" = "team_id_home",
                                   #"club_id_away" = "team_id_away")) #%>%
  
  # join the current data with the match stats
  # model_data_all <- model_data_all %>%
  #   left_join(historical_match_stats, by = c("fixture_id",
  #                                            "club_id_home" = "team_id_home",
  #                                            "club_id_away" = "team_id_away")) %>%
    # drop unwanted columns for the model
    # select(-c(league_country, league_logo, league_flag, venue_name, venue_city,
    #           referee, status_long, status_short, status_elapsed, club_logo_home,
    #           team_name_home, team_name_away,
    #           team_logo_home, team_logo_away),
    #        -contains(".y"))
  
  
  
  
  # append the venue data
  # model_data_all2 <- model_data_all %>%
  #   left_join(venues, by = c("league_id.x" = "league_id",
  #                            "league_season" = "season",
  #                            "league_round" = "league_round",
  #                            "club_id_home"))#,
                             # "club_id_away",
                             # "fixture_date.x" =  "fixture_date",
                             # "fixture_time.x" = "fixture_time"))
  
  
  
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
