# Source script setting up all libraries
source("Light_Setup.R")

#### Loading Data from APIs ####

### Odds API ####

# This script is meant to run continuously on the Raspberry Pi gathering a
# historic data stream for later analysis

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

save(current_odds, 
     file = paste("rdata/", 
                  format(Sys.time(), "%Y_%m_%d_%H_%M"), 
                  ".RData", sep = ""))


