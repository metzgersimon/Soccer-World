### script should check for updates on the sofifa web page for 
# the fifa squads or team stats daily ###

########## get newest fifa team stats ############
setwd("/srv/shiny-server/Soccer-Prediction-App")
source("global.R")

# setup a connection to the database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='soccerworld',
                 username='dev',
                 password='worldpw')

# check for the newest date we have in our data base
max_date <- all_leagues_fifa_team_stats %>% 
  filter(date == max(date)) %>% 
  select(date) %>% 
  pull() %>% 
  unique()

# check for the newest fifa we have in our data base
max_fifa <- all_leagues_fifa_team_stats %>%
  filter(fifa_vers == max(fifa_vers)) %>% 
  select(fifa_vers) %>% 
  pull() %>% 
  unique()

# create a vector with all leagues of interest
leagues <- c("Bundesliga", "Bundesliga 2", "Premier League", "Ligue 1")

# get the new team stats for all leagues in the above vector
all_leagues_fifa_team_stats_new <- get_team_stats_fifa_all_leagues(leagues, 
                                                                   fifa_version = max_fifa,
                                                                   date = max_date)

if(!is.null(all_leagues_fifa_team_stats_new)){
  dbWriteTable(con, "all_leagues_fifa_team_stats", all_leagues_fifa_team_stats_new,
               overwrite = FALSE,
               append = TRUE)
  
}


########## get newest fifa squads ############
# check for the newest date we have in our data base
max_date <- all_leagues_fifa_squads %>% 
  filter(date == max(date)) %>% 
  select(date) %>% 
  pull() %>% 
  unique()

# check for the newest fifa we have in our data base
max_fifa <- all_leagues_fifa_squads %>%
  filter(fifa_version == max(fifa_version)) %>% 
  select(fifa_version) %>% 
  pull() %>% 
  unique()

# create a vector with all leagues given as league ids that are of interest
league_ids <- c(19, 20, 13, 16)

# get the new fifa squads stats for all leagues in the above vector
all_leagues_fifa_squads_new <- get_squads_fifa_all_leagues(league_ids,
                                                           fifa_version = max_fifa,
                                                           date = max_date) 
                                                                   

if(!is.null(all_leagues_fifa_squads_new)){
  dbWriteTable(con, "all_leagues_fifa_squads", all_leagues_fifa_squads_new,
               overwrite = FALSE,
               append = TRUE)
  
}

dbDisconnect(con)

