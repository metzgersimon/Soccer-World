############## concat_odds_api #################
# inputs: path, 
# outputs: should return frame with odds data 

concat_odds_api <- function(path) {

  # create the dataframe for the data
  l <- list("id", "sport_key", "sport_title", "commence_time", "home_team", "away_team", "bookmakers" )
  final <- setNames(data.frame(matrix(ncol = 12, nrow = 0)), l)
  
  # list all files from target folder
  data_files <- list.files(path)
  
  for( i in 1:length(data_files)) {    
    
    # iterate over the files 
    load(paste0(path,data_files[i]))
    
    # create the working franes for current file
    bookm <- list.stack(current_odds[[1]], fill = TRUE)$bookmakers
    subframe <- list.stack(current_odds[[1]], fill = TRUE)
    subframe$maker <- 0
    subframe$home <- 0
    subframe$away <- 0
    subframe$draw <- 0
    subframe$update <- 0
    n <-1
    
    # iterate over the different bookmakers for each matchup
    for (k in bookm) {
      
      # append bookmaker, timestamp, and the respective quotes
      subframe$maker[n] <- bookm[[n]]$title
      subframe$update[n] <- bookm[[n]]$last_update
      subframe$home[n] <- bookm[[n]]$markets[[1]]$outcomes[[1]]$price
      subframe$away[n] <- bookm[[n]]$markets[[1]]$outcomes[[2]]$price
      subframe$draw[n] <- bookm[[n]]$markets[[1]]$outcomes[[3]]$price
      
      n <- n + 1
      
    }
    
    # append the current iteration subframe to the final frame
    final <- rbind(final, subframe)
  }
  
  return(final)
}









