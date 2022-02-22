# function returns the current standing for a selected league and season
get_league_standing <- function(league_ID, season, matchday = NULL){
  # filter all the matches in all leagues for the selected league and season
  league_season_matches <- all_leagues_matches %>%
    filter(league_id == league_ID,
           league_season == season,
           fixture_date < Sys.Date())
  
  # if a matchday is given we want to extract all matchdays smaller than
  # this matchday to compute the cumulative stats
  if(!is.null(matchday)){
    league_season_matches <- league_season_matches %>%
      filter(league_round <= matchday)
  }
  
  # create the table for the home ranking
  home_table <- league_season_matches %>%
    select(league_round, club_name = club_name_home,
           points = home_points, goals = fulltime_score_home, 
           goals_against = fulltime_score_away,
           club_id = club_id_home)
  
  # create the table for the away ranking
  away_table <- league_season_matches %>%
    select(league_round, club_name = club_name_away,
           points = away_points, goals = fulltime_score_away, 
           goals_against = fulltime_score_home,
           club_id = club_id_away)
  
  
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
    ungroup() %>%
    arrange(league_round) 
  
  
  # if a matchday is given we only want to get the table for this given matchday
  if(!is.null(matchday)){
    full_table <- full_table %>%
      # filter only for the selected matchday
      filter(league_round == matchday)
  }
    
  
  return(full_table)
}
