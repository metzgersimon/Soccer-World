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