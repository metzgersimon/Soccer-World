API_map_league_to_id <- function(league_name){
  league_id <- NULL
  if(league_name == "Bundesliga"){
    league_id <- 78
  } else if(league_name == "Bundesliga 2"){
    league_id <- 79
  } else if(league_name == "Premier League"){
    league_id <- 39
  } else if(league_name == "Ligue 1"){
    league_id <- 61
  }
  
  return(league_id)
}