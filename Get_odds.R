##### Get the odds 
setwd("/srv/shiny-server/Soccer-Prediction-App")
source("Setup.R")

#### Loading Data from APIs ####

### Odds API ####

#odds_api_url <- "https://api.the-odds-api.com/v4/sports/?"
#response <- GET(odds_api_url, query = list(apiKey = odds_api_key))
#odds_api_content <- content(response)

# premiere League not available for live service
relevant_keys <- c("soccer_france_ligue_one",
                   "soccer_germany_bundesliga",
                   "soccer_germany_bundesliga2")

current_odds <- get_current_odds(relevant_keys)

l <- list("id", "sport_key", "sport_title", "commence_time", "home_team", "away_team", "bookmakers" )
final <- setNames(data.frame(matrix(ncol = 12, nrow = 0)), l)

for( i in 1:length(current_odds)) {   
  
  # create the working franes for current file
  bookm <- list.stack(current_odds[[i]], fill = TRUE)$bookmakers
  subframe <- list.stack(current_odds[[i]], fill = TRUE)
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

# restructure the data frame
odds_today <- final %>% select(-c(id, sport_key, bookmakers)) %>%
  filter(maker == "Pinnacle") %>%
  mutate(update = ymd_hms(gsub("Z", "", gsub("T", " ", update)))) %>%
  separate(commence_time, c("fixture_date", "fixture_time"),
           sep = "T") %>% 
  rename(., "league" = "sport_title" )

odds_today <- odds_today %>%
  mutate(fixture_time = gsub("Z", "", odds_today$fixture_time)) 

odds_today$league <- ifelse(odds_today$league == "Ligue 1 - France", "Ligue 1", 
                            ifelse(odds_today$league == "Bundesliga - Germany", "Bundesliga",
                                   "Bundesliga 2"))

odds_today$home_team <- sapply(odds_today$home_team, club_name_mapping)
odds_today$away_team <- sapply(odds_today$away_team, club_name_mapping)


# setup a connection to the database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='soccerworld',
                 username='dev',
                 password='worldpw')

dbWriteTable(con, "all_leagues_odds_live", odds_today, 
             overwrite = FALSE, append = TRUE)

