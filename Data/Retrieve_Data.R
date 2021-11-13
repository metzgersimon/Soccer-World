# Source script setting up all libraries
source("Setup.R")

##################### Loading Data from APIs ########################

############# Odds API ##############

odds_api_url <- "https://api.the-odds-api.com/v4/sports/?"
response <- GET(odds_api_url, query = list(apiKey = odds_api_key))
odds_api_content <- content(response)

# Extracting keys for the Bundesliga and the 2. Bundesliga
bundesliga_keys <- list.stack(odds_api_content) %>%
  filter(str_detect(title, "Bundesliga")) %>%
  select(key) %>%
  unlist() %>%
  unname()

# Retrieving current odds for both the Bundesliga and the 2. Bundesliga
# and store them as a list of lists
current_odds <- get_current_odds(bundesliga_keys)



############ Elenasport API ##############
# For this API one first have to get a token which is valid for one hour
# (this is done with a POST).
# This token then must be used instead of the API key when retrieving data
post_request_elena <-
  POST("https://oauth2.elenasport.io/oauth2/token",
       add_headers(
         .headers = c('Content-Type' = 'application/x-www-form-urlencoded',
                      'Authorization' = elenasport_api_key)
       ),
       body = 'grant_type=client_credentials')

access_token_elena <- content(post_request_elena)$access_token

# Set the host URL
root_endpoint <- "https://football.elenasport.io/v2/"

### Get league information ###
# German league ids
# We just used the IDs to save requests (only 80/day possible)
german_league_ids <- c(272, 273, 274)

endpoint <- "leagues"

# Create the URL for the leagues (we want to start with the Bundesliga)
url_leagues <-
  paste0(root_endpoint, endpoint, "/", german_league_ids[1])

# Create a data frame to store information on German leagues
german_leagues <-
  data.frame(
    "league_id" = rep(NA, length(german_league_ids)),
    "country_id" = rep(NA, length(german_league_ids)),
    "country_name" = rep(NA, length(german_league_ids)),
    "league_name" = rep(NA, length(german_league_ids)),
    "is_national_league" = rep(NA, length(german_league_ids)),
    "is_club_league" = rep(NA, length(german_league_ids))
  )

# Get the league information for all german leagues
league_response <- GET(url_leagues,
                       add_headers('Authorization' =
                                     paste0("Bearer ", access_token_elena)))

league_content <- content(league_response)$data

league_frame <- list.stack(league_content)

### Get season information ###
# Create the URL for the seasons of the Bundesliga
endpoint <- "seasons"
url_seasons_bundesliga <- paste0(url_leagues, "/", endpoint)

season_response <- GET(url_seasons_bundesliga,
                       add_headers('Authorization' =
                                     paste0("Bearer ", access_token_elena)))

season_content <- content(season_response)$data

season_frame <- list.stack(season_content)

### Join League and season data ###
german_frame <-
  left_join(
    league_frame,
    season_frame,
    by = c("id" = "idLeague"),
    suffix = c(".league", ".season")
  ) %>%
  rename(idSeason = id.season,
         idLeague = id)



############ Scrape Fussballdaten.de ##############
root_url <- "https://www.fussballdaten.de/"

# test the written functions
league <- "bundesliga"
season <- 1964

number_of_teams <-
  get_number_of_teams_in_season(root_url, league, season)
season_results <- get_results_for_season(root_url, league,
                                         season, number_of_teams)

# Create a data frame with all results of all seasons
# 2022 does not work correctly at the moment because get_results_for_season
# can not deal with matches in the future
seasons <- c(1964:2021)

all_seasons_frame <- NULL

for(i in seasons[1]:seasons[length(seasons)]){
  number_of_teams <- get_number_of_teams_in_season(root_url, league, season)
  season_results <- get_results_for_season(root_url, league,
                                           season, number_of_teams)
  
  all_seasons_frame <- rbind(all_seasons_frame, test)
}



############## Scrape ODDSPORTAL ############
list_save <- list()
seasons <- c(2004:2022)
ports <-
  for (season in seasons) {
    # get the random port that is free
    rand_port <-
      as.integer(runif(1, 0, 1) * length(netstat::unassigned_ports()))
    port <- netstat::unassigned_ports()[rand_port]
    print(port)
    
    test <-
      get_past_odd_results(
        sport = "soccer",
        country = "germany",
        league = "bundesliga",
        season = season,
        port = port
      )
    
    list_save <- append(list_save,
                        test)
  }



