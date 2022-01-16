
append_score_to_match_stats <- function(match_data, m)



get_points_per_match <- function(league, season){
  # filter the data for the given season and remove the relegation matches
  buli_matches <- tbl(con, "buli_matches_2010_2021") %>%
    data.frame() %>%
    filter(!is.na(league_round),
           league_season == season,
           fixture_date < Sys.Date()) %>%
    # add variables for the points the teams got for a played match
    mutate(home_points = ifelse(fulltime_score_home > fulltime_score_away,
                                3,
                                ifelse(fulltime_score_home == fulltime_score_away,
                                       1,
                                       0)),
           away_points = ifelse(fulltime_score_away > fulltime_score_away,
                                3,
                                ifelse(fulltime_score_home == fulltime_score_away,
                                       1,
                                       0))) 
  
  return(buli_matches)
}

# function returns the ranking of a selected league in a selected season
# for every matchday
get_league_ranking_overall <- function(league, season, matches, type = "home"){
  
  buli_matches <- matches
  # create the table for the home ranking
  home_table <- buli_matches %>%
    # group_by(league_round) %>%
    # mutate(potential_points = 3) %>%
    # ungroup() %>%
    # mutate(potential_points = cumsum(potential_points)) %>%
    # group_by(club_name_home) %>%
    # mutate(wins_home = cumsum(home_points == 3),
    #        draws_home = cumsum(home_points == 1),
    #        losses_home = cumsum(home_points == 0),
    #        win_pct_home = (wins_home/(league_round / 2)) * 100,
    #        draw_pct_home = (draws_home/(league_round / 2)) * 100,
    #        loss_pct_home = (losses_home/(league_round / 2)) * 100) %>%
    # filter(club_name_home == "Borussia Dortmund")
  select(league_round, club_name = club_name_home,
         points = home_points, goals = fulltime_score_home, 
         goals_against = fulltime_score_away)#,
         # wins_home, draws_home, losses_home, win_pct_home, draw_pct_home,
         # loss_pct_home)
  
  # create the table for the away ranking
  away_table <- buli_matches %>%
  select(league_round, club_name = club_name_away,
         points = away_points, goals = fulltime_score_away, 
         goals_against = fulltime_score_home)

    
  # create the table for the overall ranking
  full_table <- bind_rows(home_table,
                          away_table) %>%
    # add a new variable for the goal difference
    mutate(goal_diff = goals - goals_against) %>%
    # arrange the data frame by league_round
    arrange(league_round) %>%
    group_by(club_name) %>%
    # summarize all the values
    mutate(cum_points = cumsum(points),
           cum_goals = cumsum(goals),
           cum_goals_against = cumsum(goals_against),
           cum_goal_diff = cumsum(goal_diff)) %>%
    ungroup() %>%
    group_by(league_round, club_name) %>%
    # reorder the data frame first by points, then goal difference
    # and then goals
    arrange(desc(cum_points), desc(cum_goal_diff), desc(cum_goals)) %>%
    ungroup() %>%
    group_by(league_round) %>%
    # create a new variable representing the current ranking
    mutate(rank = rank(-cum_points, ties.method = "first")) %>%
    arrange(league_round) %>%
    group_by(club_name) %>%
    mutate(across(c(rank, cum_points, cum_goals, cum_goals_against,
                    cum_goal_diff), ~shift(.x, n = 1))) %>%
    ungroup()
  
  if(type == "home"){
    full_table <- full_table %>%
      rename(home_team_rank = rank,
             home_team_points = cum_points,
             home_team_goals = cum_goals,
             home_team_goals_against = cum_goals_against,
             home_team_goal_diff = cum_goal_diff)
  } else {
    full_table <- full_table %>%
      rename(away_team_rank = rank,
             away_team_points = cum_points,
             away_team_goals = cum_goals,
             away_team_goals_against = cum_goals_against,
             away_team_goal_diff = cum_goal_diff)
  }
  
  return(full_table)
    
}


get_winning_pcts <- function(league, season, matches, type = "home"){
  buli_matches <- matches
  
  # create the table for the home ranking
  home_table <- buli_matches %>%
    select(league_round, club_name = club_name_home,
           points = home_points, goals = fulltime_score_home, 
           goals_against = fulltime_score_away)
  
  
  # create the table for the away ranking
  away_table <- buli_matches %>%
    select(league_round, club_name = club_name_away,
           points = away_points, goals = fulltime_score_away, 
           goals_against = fulltime_score_home)
  
  full_win_pct <- bind_rows(home_table, away_table) %>%
    arrange(league_round) %>%
    # filter(club_name == "Borussia Dortmund") %>%
    group_by(league_round) %>%
    mutate(potential_points = 3) %>%
    ungroup() %>%
    mutate(potential_points = cumsum(potential_points)) %>%
    group_by(club_name) %>%
    mutate(wins = cumsum(points == 3),
           draws = cumsum(points == 1),
           losses = cumsum(points == 0),
           win_pct = (wins/league_round) * 100,
           draw_pct = (draws/league_round) * 100,
           loss_pct = (losses/league_round) * 100) %>%
    mutate(across(c(wins:loss_pct), ~shift(.x, n = 1))) %>%
    select(-c(points, goals, goals_against, potential_points))
  
  if(type == "home"){
    full_win_pct <- full_win_pct %>%
      rename(home_team_wins = wins,
             home_team_draws = draws,
             home_team_losses = losses,
             home_team_win_pct = win_pct,
             home_team_draw_pct = draw_pct,
             home_team_loss_pct = loss_pct)
  } else {
    full_win_pct <- full_win_pct %>%
      rename(away_team_wins = wins,
             away_team_draws = draws,
             away_team_losses = losses,
             away_team_win_pct = win_pct,
             away_team_draw_pct = draw_pct,
             away_team_loss_pct = loss_pct)
  }
  
  return(full_win_pct)
}



convert_two_lines_into_one <- function(frame_to_convert, join_columns = NULL){
  frame_to_convert <- frame_to_convert %>%
    group_by(match_group = rep(row_number(), length.out = n(), each = 2)) %>%
    ungroup() 
  
  home_part <- frame_to_convert %>%
    group_by(match_group) %>%
    filter(row_number(desc(match_group)) == 1) %>%
    ungroup() %>%
    select(-c(contains("away")))
  
  away_part <- frame_to_convert %>%
    group_by(match_group) %>%
    filter(row_number(desc(match_group)) == 2) %>%
    ungroup() %>%
    select(-c(contains("home")))
  
  one_line_frame <- home_part %>%
    inner_join(away_part, by = join_columns, suffix = c("_home", "_away")) %>%
    select(-match_group)
  
  return(one_line_frame)
}



get_matches_with_features <- function(league, season_start){
  buli_matches <- get_points_per_match("bundesliga", season_start)
  
  win_pct_features_home <- get_winning_pcts("bundesliga", season_start, buli_matches,
                                            type = "home")
  
  win_pct_features_away <- get_winning_pcts("bundesliga", season_start, buli_matches,
                                            type = "away")
  
  rank_features_home <- get_league_ranking_overall("bundesliga", season_start, 
                                                   buli_matches, type = "home")
  
  rank_features_away <- get_league_ranking_overall("bundesliga", season_start, 
                                                   buli_matches, type = "away")
  
  # buli_match_stats_2015_2021$team_name <- unname(buli_match_stats_2015_2021$team_name)
  # 
  # buli_match_stats_2015_2021 <- buli_match_stats_2015_2021 %>%
  #   mutate(ball_possession = str_remove(ball_possession, "%"),
  #          passing_accuracy = str_remove(passing_accuracy, "%")) %>%
  #   mutate(across(c(shots_on_goal:passing_accuracy), as.numeric)) %>%
  #   mutate(across(c(shots_on_goal:passing_accuracy), ~replace_na(.x, 0))) %>%
  #   mutate(fixture_date = ymd(fixture_date))
    
    
  # buli_match_stats <- buli_match_stats_2015_2021 %>%
  #   filter(season == season_start) %>%
  #   group_by(team_name) %>%
  #   mutate(across(c(shots_on_goal:passing_accuracy), ~shift(.x, n = 1))) %>%
  #   mutate(shots_test = runMean(shots_total, n = 2))
  #   convert_two_lines_into_one(., join_columns = c("match_group", "fixture_id",
  #                                                  "matchday", "fixture_date",
  #                                                  "season", "league_id",
  #                                                  "fixture_time")) %>%
  #   group_by(matchday)
  #   
  #   
  #   
  #   
  # buli_rest_days <- tbl(con, "buli_2000_2021_regen_days") %>%
  #   filter(season_start_year == season) %>%
  #   mutate(home_team_rest_days = ifelse(team_name == home_team,
  #                                       regeneration_days,
  #                                       NA),
  #          away_team_rest_days = ifelse(team_name == away_team,
  #                                       regeneration_days,
  #                                       NA)) %>%
  #   # select(-team_name) %>%
  #   data.frame() %>%
  #   arrange(matchday, date, time, home_team) %>%
  #   group_by(match_group = rep(row_number(), length.out = n(), each = 2)) %>%
  #   ungroup() 
  # 
  # home_rest_days <- buli_rest_days %>%
  #   group_by(match_group) %>%
  #   filter(row_number(desc(match_group)) == 1) %>%
  #   ungroup() %>%
  #   rename(home_team_coach = coach)
  # 
  # away_rest_days <- buli_rest_days %>%
  #   group_by(match_group) %>%
  #   filter(row_number(desc(match_group)) == 2) %>%
  #   ungroup()
  # 
  # rest_days_frame <- home_rest_days %>%
  #   inner_join(away_rest_days, by = c("match_group", "matchday", "date", "time")) %>%
  #   rename(club_name_home = `team_name.x`,
  #          club_name_away = `team_name.y`)
  # 
  # alls_frame$club_name_home <- sapply(alls_frame$club_name_home,
  #                                     club_name_mapping)
  # 
  # alls_frame$club_name_away <- sapply(alls_frame$club_name_away,
  #                                     club_name_mapping)
  
  
  
  # 
  # buli_regen_days <- tbl(con, "buli_2000_2021_regen_days") %>%
  #   filter(season_start_year == season) %>%
  #   mutate(home_team_rest_days = ifelse(team_name == home_team,
  #                                       regeneration_days,
  #                                       NA),
  #          away_team_rest_days = ifelse(team_name == away_team,
  #                                       regeneration_days,
  #                                       NA)) %>%
  #   select(-c(season_start_year, league, result, team_name)) %>%
  #   data.frame()
  # 
  # buli_regen_days <- buli_regen_days %>%
  #   inner_join(buli_regen_days %>%
  #                select(-c(coach, time)), 
  #              by = c("matchday", "date", "home_team", "away_team"))
  
  buli_matches_home_team <- buli_matches %>%
    select(-c(club_id_away, club_name_away, club_logo_away)) %>%
    inner_join(win_pct_features_home, by = c("league_round" = "league_round",
                                    "club_name_home" = "club_name")) %>%
    inner_join(rank_features_home, by = c("league_round" = "league_round",
                                          "club_name_home" = "club_name")) #%>%
    # inner_join(buli_regen_days, by = c("league_round" = "matchday",
    #                                    "club_name_home" = "team_name"))
  
  buli_matches_away_team <- buli_matches %>%
    select(-c(club_id_home, club_name_home, club_logo_home)) %>%
    inner_join(win_pct_features_away, by = c("league_round" = "league_round",
                                             "club_name_away" = "club_name")) %>%
    inner_join(rank_features_away, by = c("league_round" = "league_round",
                                          "club_name_away" = "club_name"))
  
  buli_matches_with_features <- cbind(buli_matches_home_team,
                                      buli_matches_away_team)
  
  buli_matches_with_features <- 
    buli_matches_with_features[, !duplicated(colnames(buli_matches_with_features))] %>%
    mutate(fixture_date = ymd(fixture_date))
  
  # buli_matches_with_features <- buli_matches_with_features %>%
  #   left_join(buli_odds, by = c("league_season" = "season",
  #                               "fixture_date" = "date",
  #                               "club_name_home" = "home",
  #                               "club_name_away" = "away"))
  
  return(buli_matches_with_features)
}

buli_with_stats_2015 <- get_matches_with_features("bundesliga", 2015)
buli_with_stats_2016 <- get_matches_with_features("bundesliga", 2016)
buli_with_stats_2017 <- get_matches_with_features("bundesliga", 2017)
buli_with_stats_2018 <- get_matches_with_features("bundesliga", 2018)
buli_with_stats_2019 <- get_matches_with_features("bundesliga", 2019)
buli_with_stats_2020 <- get_matches_with_features("bundesliga", 2020)
buli_with_stats_2021 <- get_matches_with_features("bundesliga", 2021)

buli_with_stats_2015_to_2021 <- bind_rows(buli_with_stats_2015,
                                          buli_with_stats_2016,
                                          buli_with_stats_2017,
                                          buli_with_stats_2018,
                                          buli_with_stats_2019,
                                          buli_with_stats_2020,
                                          buli_with_stats_2021)
  


  