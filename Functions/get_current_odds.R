############## get_current_odds #################
# inputs: keys
# outputs: should return a list for current odds data

get_current_odds <- function(keys){
  current_odds_data <- vector(mode = "list", length = length(keys))
  
  for(i in 1:length(current_odds_data)){
    odds_api_url <- paste0("https://api.the-odds-api.com/v4/sports/",
                           keys[i], "/odds/?apiKey=",
                           odds_api_key, "&regions=eu")
    
    odds_api_response <- GET(odds_api_url)
    odds_api_content <- content(odds_api_response)
    
    current_odds_data[[i]] <- odds_api_content
  }
  
  return(current_odds_data)
}

