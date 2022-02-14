

get_model_data <- function(){
  spi_data <- prepare_spi_data()
  historical_match_stats <- prepare_team_match_stats_historical()
  venues <- prepare_venue_data()
  
  league_ids <- c(78, 79, 39, 61)
  seasons <- c(2016:2021)
  
  all_leagues_seasons_running_tables_home <- get_league_ranking_overall(type = "home")
  all_leagues_seasons_running_tables_away <- get_league_ranking_overall(type = "away")
  
  
  model_data_home <- spi_data %>%
    select(-c(club_id_away, club_name_away, club_logo_away)) %>%
    inner_join(all_leagues_seasons_running_tables_home, 
               by = c("league_season" = "season",
                      "league_round" = "league_round",
                      "league_id",
                      "club_name_home" = "club_name")) %>%
    filter(fixture_date <= "2022-01-31",
           !is.na(league_round))
  
  
  model_data_away <- spi_data %>%
    select(-c(club_id_home, club_name_home, club_logo_home)) %>%
    inner_join(all_leagues_seasons_running_tables_away, 
               by = c("league_season" = "season",
                      "league_round" = "league_round",
                      "league_id",
                      "club_name_away" = "club_name")) %>%
    filter(fixture_date <= "2022-01-31",
           !is.na(league_round)) %>%
    # drop unwanted columns
    select(fixture_id, club_id_away, club_name_away, away_team_points,
           away_team_goals_against, away_team_goal_diff, away_team_rank)
  
  
  model_data_all <- inner_join(model_data_home, model_data_away,
                               by = c("fixture_id"))
  
  
  # join the current data with the match stats
  model_data_all <- model_data_all %>%
    left_join(historical_match_stats, by = c("fixture_id",
                                             "club_id_home" = "team_id_home",
                                             "club_id_away" = "team_id_away")) %>%
    # drop unwanted columns for the model
    select(-c(league_country, league_logo, league_flag, venue_name, venue_city,
              referee, status_long, status_short, status_elapsed, club_logo_home,
              season, team_name_home, team_name_away,
              team_logo_home, team_logo_away),
           -contains(".y"))
  
  # append the venue data
  model_data_all2 <- model_data_all %>%
    left_join(venues, by = c("league_id.x" = "league_id",
                             "league_season" = "season",
                             "league_round" = "league_round",
                             "club_id_home"))#,
                             "club_id_away",
                             "fixture_date.x" =  "fixture_date",
                             "fixture_time.x" = "fixture_time"))
  
  
  
  
  
}
