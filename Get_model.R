########## fit and predict weekly model ############
setwd("/srv/shiny-server/Soccer-Prediction-App")
source("global.R")
source("Model/xgb_weekly.R")
source("Model/Prepare Data.R")
source("Model/Combining data.R")

# setup a connection to the database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='soccerworld',
                 username='dev',
                 password='worldpw')


# get the already available data 
all_leagues_historical_predictions <- tbl(con, "all_leagues_historical_predictions") %>% data.frame()

future_predictions <- train_xgb()

new <- future_predictions %>%
  filter(!(fixture_id %in% all_leagues_historical_predictions$fixture_id))

if(nrow(new) != 0) {
  
  dbWriteTable(con, "all_leagues_historical_predictions", new, 
               overwrite = FALSE, append = TRUE)
  
}





