############## get_all_available_leagues_by_country #################
# inputs: country
# outputs: should return a data frame for a selected country that contains
# information about all available leagues in that country
# example: 
get_all_available_leagues_by_country <- function(country){
  # handle weird ways of writing the country by transforming
  # it into title, i.e., "germany", "gERMANY" and so on all become
  # "Germany"
  country <- str_to_title(country)
  
  # set the endpoint of the API
  endpoint <- "https://v3.football.api-sports.io/leagues"
  
  # create a get request to that API with the API key
  # and the selected parameters (country) and we just want to get
  # leagues, not cups, therefore, we set type to "league"
  response <- GET(endpoint, 
                  add_headers('x-apisports-key' = football_api_key),
                  query = list(country = country,
                               type = "league"))
  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(response) >= 200 & status_code(response) < 300){
    # extract the content from the response
    content <- content(response)$response
    
    # create an empty variable to store the data
    available_leagues_and_seasons <- NULL
    
    # iterate through the content
    for(i in 1:length(content)){
      # extract all league related information and store it in a variable
      league_infos <- content[[i]]$league
      # extract all season related information and store it in a variable
      season_infos <- content[[i]]$seasons
      
      # extract the information of the leagues
      league_id <- league_infos$id
      league_name <- league_infos$name
      league_type <- league_infos$type
      league_logo <- league_infos$logo
      
      # extract the content of the league
      country <- content[[i]]$country$name
      
      # create a length for the season information
      season_length <- length(season_infos)
      
      # create a frame to store the information of all seasons
      current_league_seasons <- data.frame(season_year = integer(season_length),
                                           season_start_date = character(season_length),
                                           season_end_date = character(season_length),
                                           is_current = logical(season_length),
                                           has_event_data = logical(season_length),
                                           has_lineup_data = logical(season_length),
                                           has_fixture_stat_data = logical(season_length),
                                           has_player_stat_data = logical(season_length),
                                           has_standings_data = logical(season_length),
                                           has_player_data = logical(season_length),
                                           has_topscorer_data = logical(season_length),
                                           has_topassist_data = logical(season_length),
                                           has_topcards_data = logical(season_length),
                                           has_injury_data = logical(season_length),
                                           has_prediction_data = logical(season_length),
                                           has_odd_data = logical(season_length)
                                           )
      
      # extract season specific information
      for(j in 1:length(season_infos)){
        season_year <- season_infos[[j]]$year
        season_start_date <- season_infos[[j]]$start
        season_end_date <- season_infos[[j]]$end
        # booleans to check whether specific information are
        # available for a given season
        is_current <- season_infos[[j]]$current
        has_event_data <- season_infos[[j]]$coverage$fixtures$events
        has_lineup_data <-season_infos[[j]]$coverage$fixtures$lineups
        has_fixture_stat_data <-season_infos[[j]]$coverage$fixtures$statistics_fixtures
        has_player_stat_data <- season_infos[[j]]$coverage$fixtures$statistics_players
        has_standings_data <- season_infos[[j]]$coverage$standings
        has_player_data <- season_infos[[j]]$coverage$players
        has_topscorer_data <- season_infos[[j]]$coverage$top_scorers
        has_topassist_data <- season_infos[[j]]$coverage$top_assists
        has_topcards_data <- season_infos[[j]]$coverage$top_cards
        has_injury_data <- season_infos[[j]]$coverage$injuries
        has_prediction_data <- season_infos[[j]]$coverage$predictions
        has_odd_data <- season_infos[[j]]$coverage$odds
        
        # store the extracted information into a data frame
        # at the j-th row
        current_league_seasons[j, ] <- cbind(season_year,
                                             season_start_date,
                                             season_end_date,
                                             is_current, has_event_data,
                                             has_lineup_data, 
                                             has_fixture_stat_data, 
                                             has_player_stat_data,
                                             has_standings_data,
                                             has_player_data,
                                             has_topscorer_data,
                                             has_topassist_data,
                                             has_topcards_data,
                                             has_injury_data,
                                             has_prediction_data,
                                             has_odd_data)
      }
     
      
      # create a data frame from the current data
      current_league_data <- 
        data.frame(cbind(league_id, league_name,
                         league_type, league_logo,
                         country, current_league_seasons))
      
      # store the data of the current league into the data frame
      # which stores the information about all leagues
      available_leagues_and_seasons <- bind_rows(available_leagues_and_seasons,
                                                 current_league_data)
    }
  
    # if the request was not successful print an error  
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(response)))
  }
  
  # return the data frame that contains all league information in a 
  # given country
  return(available_leagues_and_seasons)
}



############## get_team_information_in_league #################
# inputs: league_id, season
# outputs: should return a data frame containing information about all
# teams that are in the given league for the selected season
# example: founding year of the club, stadium information of the club, etc.
get_team_information_in_league <- function(league_id, season){
  # to be able to avoid problems, we convert the season parameter into a numeric
  # in case it is given as a string
  season <- as.numeric(season)
  
  # set the endpoint of the API
  endpoint <- "https://v3.football.api-sports.io/teams"
  
  # create a get request to that API with the API key
  # and the selected parameters (league via league_id and season)
  response <- GET(endpoint, 
                  add_headers('x-apisports-key' = football_api_key),
                  query = list(league = league_id,
                               season = season))
  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(response) >= 200 & status_code(response) < 300){
    # extract the content from the response
    content <- content(response)$response
    
    # compute the number of teams to pre-allocate the data frame
    number_of_teams <- length(content)
    
    # pre-allocate the data frame
    team_info_frame <- data.frame(team_id = integer(number_of_teams),
                                  team_name = character(number_of_teams),
                                  country = character(number_of_teams),
                                  founded = integer(number_of_teams),
                                  national = logical(number_of_teams),
                                  logo = character(number_of_teams),
                                  venue_id = integer(number_of_teams),
                                  venue_name = character(number_of_teams),
                                  venue_address = character(number_of_teams),
                                  venue_city = character(number_of_teams),
                                  venue_capacity = integer(number_of_teams),
                                  venue_surface = character(number_of_teams),
                                  venue_image = character(number_of_teams))
    
    # iterate through all teams
    for(i in 1:number_of_teams){
      # get the team infos for the current team
      curr_team_infos <- unname(unlist(content[[i]]))
      # and insert it into the i-th row of the data frame
      team_info_frame[i,] <- curr_team_infos
    }
    
    # after the loop integrate variables for the league (given by league_id)
    # and the season
    team_info_frame <- team_info_frame %>%
      mutate(league_id = league_id,
             season = season)
    
    # if the request was not successful print an error 
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(response)))
  }
  
  # return the data frame containing information about all teams
  # in the league for the given season
  return(team_info_frame)
}



############## get_team_squads_current_saison #################
# inputs: league_id
# outputs: should return a data frame containing information about all
# team squads in the current season for the given league_id
# example: squad of Dortmund when the bundesliga id is given
get_team_squads_current_saison <- function(league_id){
  
  # call the get_team_information_in_league function with
  # the given league_id and the current season to get all team ids
  all_teams_in_league <- get_team_information_in_league(league_id, 2021) %>%
    select(team_id) %>%
    unlist() %>%
    unname()
  
  # set the endpoint of the API
  endpoint <- "https://v3.football.api-sports.io/players/squads"
  
  # create an empty variable to store the data
  team_squads <- NULL
  
  # iterate through all the teams that play in the league
  for(i in 1:length(all_teams_in_league)){
    
    # create a get request to that API with the API key
    # and the selected parameter (team given by its id got from the 
    # get_team_information_in_league function)
    response <- GET(endpoint, 
                    add_headers('x-apisports-key' = football_api_key),
                    query = list(team = all_teams_in_league[i]))
    
    response <- GET(endpoint, 
                    add_headers('x-apisports-key' = football_api_key),
                    query = list(team = id))
    
    # check if the request was successful and only then go on with the 
    # transformation of the data
    if(status_code(response) >= 200 & status_code(response) < 300){
      
      # extract the content from the response
      content <- content(response)$response[[1]]
      
      # get all the player information from the players list
      curr_team_squad <- list.stack(content$players)
      
      # add columns that stands for the team id
      # and the team name
      curr_team_squad <- curr_team_squad %>%
        mutate(team_id = all_teams_in_league[i],
               team_name = content$team$name)
      
      # combine the team_squads data frame and the curr_team_squad frame
      # by rows
      team_squads <- bind_rows(team_squads,
                               curr_team_squad)
      
      # if the request was not successful print an error 
    } else {
      print(paste0("Error: The request was not successful. \nStatus code: ",
                   status_code(response)))
    }
  }
  
  # return the data frame containing information about all squads
  # of the teams
  return(team_squads)
}
# 
# get_team_stats <- function(league_id, number_of_teams){
#   endpoint <- "https://v3.football.api-sports.io/teams"
#   
#   for(i in 1:number_of_teams){
#     response <- GET(endpoint, add_headers('x-apisports-key' = football_api_key),
#                     query = list(team = 157)))
#   }
#   
# }
# 
# endpoint <- 
# 
# season_response <- GET(endpoint,  
#                        add_headers('x-apisports-key' = football_api_key),
#                        query = list(team = 157))
# 
# 
# # extract the content
# season_content <- content(season_response)

# create a data frame by parsing the list of lists
# season_frame <- list.stack(season_content)