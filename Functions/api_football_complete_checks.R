############## api_football_fixtures_general_complete_check #################
# inputs: data_frame_to_observe, content_type
# outputs: returns the given data frame as it should look like 
api_football_fixtures_general_complete_check <- function(data_frame_to_observe,
                                                         content_type = "general"){
  
  # check which content type it is, i.e., what kind of data we want to
  # produce and based on that create an emtpy data frame with the correct
  # column names in the right order
  if(content_type == "general"){
    data_frame_template <- data.frame(matrix(ncol = 9,
                                             nrow = 0))
    
    
    colnames(data_frame_template) <- c("id", "referee", "date",
                                       "venue.id", "venue.name", "venue.city", 
                                       "status.long", "status.short", "status.elapsed")
  } else if(content_type == "league"){
    data_frame_template <- data.frame(matrix(ncol = 7,
                                             nrow = 0))
    
    
    colnames(data_frame_template) <- c("league_id", "league_name", "league_country",
                                       "league_logo", "league_flag", "league_season",
                                       "league_round")
  } else if(content_type == "team"){
    data_frame_template <- data.frame(matrix(ncol = 8,
                                             nrow = 0))
    
    
    colnames(data_frame_template) <- c("home_id", "home_name", "home_logo",
                                       "home_winner", "away_id", "away_name",
                                       "away_logo", "away_winner")
  } else if(content_type == "score"){
    data_frame_template <- data.frame(matrix(ncol = 4,
                                             nrow = 0))
    
    
    colnames(data_frame_template) <- c("halftime_home", "halftime_away", 
                                       "fulltime_home", "fulltime_away")
    
  } else if(content_type == "standing"){
    data_frame_template <- data.frame(matrix(ncol = 29,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("rank", "team_id", "team_name", "team_logo",
                                       "points", "goalsDiff", "group", "form", 
                                       "status", "description", "all_played",
                                       "all_win", "all_draw", "all_lose",
                                       "all_goals_for", "all_goals_against",
                                       "home_played", "home_win", "home_draw",
                                       "home_lose", "home_goals_for", 
                                       "home_goals_against", "away_played",
                                       "away_win", "away_draw", "away_lose",
                                       "away_goals_for", "away_goals_against",
                                       "update")
    
  } else if(content_type == "player_stats_fixture"){
    data_frame_template <- data.frame(matrix(ncol = 33,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("games_minutes", "games_number",
                                       "games_position", "games_rating",
                                       "games_captain", "games_substitute",
                                       "offsides", "shots_total", "shots_on",
                                       "goals_total", "goals_conceded",
                                       "goals_assists", "goals_saves", 
                                       "passes_total", "passes_key", 
                                       "passes_accuracy", "tackles_total",
                                       "tackles_blocks", "tackles_interception",
                                       "duels_total", "duels_won",
                                       "dribbles_attempts", "dribbles_success",
                                       "dribbles_past", "fouls_drawn", 
                                       "fouls_committed", "cards_yellow", 
                                       "cards_red", "penalty_won", 
                                       "penalty_commited", "penalty_scored",
                                       "penalty_missed", "penalty_saved")
    
  } else if(content_type == "player_stats_season"){
    data_frame_template <- data.frame(matrix(ncol = 57,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("league_id", "league_name" , "league_country",
                                       "league_logo", "league_flag", "league_season",
                                       "team_id", "team_name", "team_logo", 
                                       "player_id",
                                       "player_name", "player_firstname", "player_lastname",
                                       "player_age", "player_birth_date",     
                                       "player_birth_place", "player_birth_country",
                                       "player_nationality", "player_injured",
                                       "player_photo", "player_weight",     
                                       "player_height", 
                                       "games_appearences",     
                                       "games_lineups", "games_minutes", "games_position",
                                       "games_rating", 
                                       "substitutes_in",
                                       "substitutes_out", "substitutes_bench",
                                       "shots_total", "shots_on", 
                                       "goals_total", 
                                       "goals_conceded", "goals_assists", "goals_saves",
                                       "passes_total", "passes_key", "passes_accuracy",
                                       "tackles_total", "tackles_blocks", 
                                       "tackles_interceptions", "duels_total", 
                                       "duels_won", "dribbles_attempts", 
                                       "dribbles_success", "dribbles_past",
                                       "fouls_drawn", "fouls_committed",
                                       "cards_yellow",
                                       "cards_yellowred",  "cards_red",
                                       "penalty_won",  "penalty_commited",     
                                       "penalty_scored", "penalty_missed",
                                       "penalty_saved")

    
  } else if(content_type == "fixture_events"){
    data_frame_template <- data.frame(matrix(ncol = 12,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("time_elapsed", "time_extra",
                                       "team_id", "team_name", "team_logo",
                                       "player_id", "player_name",
                                       "assist_id", "assist_name",
                                       "event_type", "event_detail", "comments")
    
  } else if(content_type == "team_transfer"){
    data_frame_template <- data.frame(matrix(ncol = 11,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("player_id", "player_name", "date", "type",
                                       "transfer_sum_mil_euro",
                                       "from_team_id", "from_team_name",
                                       "from_team_logo", "to_team_id", 
                                       "to_team_name", "to_team_logo")
    
  } else if(content_type == "team_stats_league_info"){
    data_frame_template <- data.frame(matrix(ncol = 6,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("league_id", "league_name", "league_country",
                                       "league_logo", "league_flag", "league_season")
    
  } else if(content_type == "team_stats_team_info"){
    data_frame_template <- data.frame(matrix(ncol = 3,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("team_id", "team_name", "team_logo")
    
  } else if(content_type == "team_stats_fixture_info"){
    data_frame_template <- data.frame(matrix(ncol = 12,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("fixtures_played_home", "fixtures_played_away",  
                                       "fixtures_played_total", "fixtures_wins_home",   
                                       "fixtures_wins_away", "fixtures_wins_total",
                                       "fixtures_draws_home", "fixtures_draws_away",  
                                       "fixtures_draws_total", "fixtures_loses_home",
                                       "fixtures_loses_away", "fixtures_loses_total" )
    
  } else if(content_type == "team_stats_goal_info"){
    data_frame_template <- data.frame(matrix(ncol = 44,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("goals_for_total_home",
                                       "goals_for_total_away",                  
                                       "goals_for_total_total",
                                       "goals_for_average_home",                
                                       "goals_for_average_away",
                                       "goals_for_average_total",               
                                       "goals_for_minute_0-15_total",
                                       "goals_for_minute_0-15_percentage",      
                                       "goals_for_minute_16-30_total", 
                                       "goals_for_minute_16-30_percentage",    
                                       "goals_for_minute_31-45_total",
                                       "goals_for_minute_31-45_percentage",     
                                       "goals_for_minute_46-60_total",
                                       "goals_for_minute_46-60_percentage",     
                                       "goals_for_minute_61-75_total",
                                       "goals_for_minute_61-75_percentage",     
                                       "goals_for_minute_76-90_total",
                                       "goals_for_minute_76-90_percentage",     
                                       "goals_for_minute_91-105_total",
                                       "goals_for_minute_91-105_percentage",
                                       "goals_for_minute_106-120_total",
                                       "goals_for_minute_106-120_percentage",
                                       "goals_against_total_home",
                                       "goals_against_total_away",              
                                       "goals_against_total_total",
                                       "goals_against_average_home",            
                                       "goals_against_average_away",
                                       "goals_against_average_total",           
                                       "goals_against_minute_0-15_total",
                                       "goals_against_minute_0-15_percentage",  
                                       "goals_against_minute_16-30_total",
                                       "goals_against_minute_16-30_percentage", 
                                       "goals_against_minute_31-45_total",
                                       "goals_against_minute_31-45_percentage", 
                                       "goals_against_minute_46-60_total",
                                       "goals_against_minute_46-60_percentage", 
                                       "goals_against_minute_61-75_total",
                                       "goals_against_minute_61-75_percentage", 
                                       "goals_against_minute_76-90_total",
                                       "goals_against_minute_76-90_percentage", 
                                       "goals_against_minute_91-105_total",
                                       "goals_against_minute_91-105_percentage",
                                       "goals_against_minute_106-120_total",
                                       "goals_against_minute_106-120_percentage")
    
  } else if(content_type == "team_stats_penalty_info"){
    data_frame_template <- data.frame(matrix(ncol = 5,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("penalty_scored_total", 
                                       "penalty_scored_percentage",
                                       "penalty_missed_total",     
                                       "penalty_missed_percentage",
                                       "penalty_total")
    
  } else if(content_type == "team_stats_biggest_info"){
    data_frame_template <- data.frame(matrix(ncol = 11,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("biggest_streak_wins", "biggest_streak_draws",
                                       "biggest_streak_loses", "biggest_wins_home",
                                       "biggest_wins_away", "biggest_loses_home",        
                                       "biggest_loses_away", "biggest_goals_for_home",
                                       "biggest_goals_for_away",    
                                       "biggest_goals_against_home",
                                       "biggest_goals_against_away")
    
  } else if(content_type == "team_stats_clean_sheet_info"){
    data_frame_template <- data.frame(matrix(ncol = 3,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("clean_sheet_home", "clean_sheet_away",
                                       "clean_sheet_total")
    
  } else if(content_type == "team_stats_failed_to_score_info"){
    data_frame_template <- data.frame(matrix(ncol = 3,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("failed_to_score_home", 
                                       "failed_to_score_away",
                                       "failed_to_score_total")
    
  } else if(content_type == "team_stats_cards_info"){
    data_frame_template <- data.frame(matrix(ncol = 32,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("cards_yellow_0-15_total",
                                       "cards_yellow_0-15_percentage",
                                       "cards_yellow_16-30_total",
                                       "cards_yellow_16-30_percentage",
                                       "cards_yellow_31-45_total",      
                                       "cards_yellow_31-45_percentage",
                                       "cards_yellow_46-60_total",
                                       "cards_yellow_46-60_percentage", 
                                       "cards_yellow_61-75_total",
                                       "cards_yellow_61-75_percentage",
                                       "cards_yellow_76-90_total",      
                                       "cards_yellow_76-90_percentage",
                                       "cards_yellow_91-105_total",
                                       "cards_yellow_91-105_percentage",
                                       "cards_yellow_106-120_total",
                                       "cards_yellow_106-120_percentage",
                                       "cards_red_0-15_total",
                                       "cards_red_0-15_percentage",
                                       "cards_red_16-30_total",
                                       "cards_red_16-30_percentage",
                                       "cards_red_31-45_total",
                                       "cards_red_31-45_percentage",
                                       "cards_red_46-60_total",
                                       "cards_red_46-60_percentage",
                                       "cards_red_61-75_total",
                                       "cards_red_61-75_percentage",
                                       "cards_red_76-90_total",
                                       "cards_red_76-90_percentage",
                                       "cards_red_91-105_total",
                                       "cards_red_91-105_percentage",
                                       "cards_red_106-120_total",
                                       "cards_red_106-120_percentage")
    
  } 
  
  # get the colnames of the data frame we want to check and 
  # of the one that is how it should be
  colnames_right <- colnames(data_frame_template)
  colnames_wrong <- colnames(data_frame_to_observe)
  
  # get all columns that are missing
  missing_columns <- setdiff(colnames_right, colnames_wrong)
  
  # if all columns are as they should be just return the data frame
  if(length(missing_columns) == 0){
    return(data_frame_to_observe)
    # otherwise we need to work on the data frame
  } else {
    # create a vector to store the positions of the missing columns
    col_positions <- rep(NA, length(missing_columns))
    
    # create a new variable to work on
    data_frame_complete <- data_frame_to_observe
    
    # iterate over all missing columns
    for(i in 1:length(col_positions)){
      # get the position of the current column
      col_positions[i] <- which(colnames_right == missing_columns[i])
      # get the name of the current column
      col_name <- colnames_right[col_positions[i]]
      
      # insert missing column with dynamic name 
      # and insert it at the correct position
      data_frame_complete <- data_frame_complete %>%
        add_column(!! paste0(col_name) := NA, .after = (col_positions[i] - 1))
    }
    
    # return the data frame which is now complete
    return(data_frame_complete)
  }

}