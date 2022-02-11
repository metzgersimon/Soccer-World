### script should check for updates on the Transfermarkt.com web page for 
# the market values, the fifa top nations rating, daily ###

########## get market values from TM ############
# get the max available date for this data set in our data base
max_date <- all_leagues_market_values_over_time %>%
  summarize(date = max(date)) %>%
  select(date) %>%
  pull()

# create the vectors of the league names and ids
leagues <- c("bundesliga", "2-bundesliga", "premier-league", "ligue-1")
league_ids <- c("L1", "L2", "GB1", "FR1")

# do the actual scraping 
all_leagues_market_values_new <- get_market_values_over_time_all_leagues(leagues,
                                                                         league_ids,
                                                                         max_date)

# if the all_leagues_market_values_new element is not null, i.e.,
# there was no new data available, we write the new data (by appending it)
# into the data base
if(!is.null(all_leagues_market_values_new)){
  
  dbWriteTable(con, "all_leagues_market_values_over_time", 
               all_leagues_market_values_new,
               overwrite = FALSE, append = TRUE)
}




########## get squads ############
# get the newest available season
max_season <- max(all_leagues_tm_squads$season)

# create the vectors of the league names and ids
leagues <- c("bundesliga", "2-bundesliga", "premier-league", "ligue-1")
league_ids <- c("L1", "L2", "GB1", "FR1")

# do the actual scraping 
all_leagues_squads_new <- get_squads_by_season_all_leagues(leagues,
                                                           league_ids,
                                                           max_season)


# filter for only those matches that are not already in the data base
all_leagues_squads_new_test <- all_leagues_tm_squads %>%
  # filter only for the current season
  filter(season == max_season,
         # drop the serie a and la liga
         league %in% leagues) %>%
  # use an anti join to get all of those matches that are not in the data base
  anti_join(all_leagues_squads_new, 
            by = c("league", "season", "club")) %>%
  # filter for only those that are in the past
  filter(date < Sys.Date())

# if the all_leagues_market_values_new element is not null, i.e.,
# there was new data available, we write the new data (by appending it)
# into the data base
if(!is.null(all_leagues_market_values_new)){
  
  dbWriteTable(con, "all_leagues_market_values_over_time", 
               all_leagues_market_values_new,
               overwrite = FALSE, append = TRUE)
}




########## get fifa world ranking top nations ############
# get the newest date in the data base
max_date <- fifa_top_15_nations %>%
  filter(date == max(date)) %>%
  select(date) %>%
  pull() %>%
  unique()

# extract the new fifa top 15 ranking table
fifa_top_15_nations_new <- get_fifa_world_ranking_top_nations(max_date,
                                                              top_nations = 15)


# if the fifa_top_15_nations_new element is not null, i.e.,
# there was new data available, we write the new data (by appending it)
# into the data base
if(!is.null(fifa_top_15_nations_new)){
  
  dbWriteTable(con, "fifa_top_15_nations", 
               fifa_top_15_nations_new,
               overwrite = FALSE, append = TRUE)
}




