api_football_fixtures_general_complete_check <- function(data_frame_to_observe,
                                                         content_type = "general"){
  
  if(content_type == "general"){
    data_frame_template <- data.frame(matrix(ncol = 13,
                                             nrow = 0))
    
    
    colnames(data_frame_template) <- c("id", "referee", "timezone", "date",
                                       "timestamp", "periods.first", "periods.second",
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
    
    
    colnames(data_frame_template) <- c("home.id", "home.name", "home.logo",
                                       "home.winner", "away.id", "away.name",
                                       "away.logo", "away.winner")
  } else if(content_type == "score"){
    data_frame_template <- data.frame(matrix(ncol = 4,
                                             nrow = 0))
    
    
    colnames(data_frame_template) <- c("halftime.home", "halftime.away", 
                                       "fulltime.home", "fulltime.away")
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
  } else if(content_type == "player_stats"){
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
    
  } else if(content_type == "fixture_events"){
    data_frame_template <- data.frame(matrix(ncol = 12,
                                             nrow = 0))
    
    colnames(data_frame_template) <- c("time_elapsed", "time_extra",
                                       "team_id", "team_name", "team_logo",
                                       "player_id", "player_name",
                                       "assist_id", "assist_name",
                                       "type", "detail", "comments")
    
  }
  
  
  colnames_right <- colnames(data_frame_template)
  colnames_wrong <- colnames(data_frame_to_observe)
  
  missing_columns <- setdiff(colnames_right, colnames_wrong)
  
  if(length(missing_columns) == 0){
    return(data_frame_to_observe)
  } else {
    col_positions <- rep(NA, length(missing_columns))
    
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