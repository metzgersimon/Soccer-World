### script should check for updates on the Transfermarkt.com web page for 
# the market values, the fifa top nations rating, daily ###

########## get market values from TM ############

setwd("/srv/shiny-server/Soccer-Prediction-App")
source("global.R")
# setup a connection to the database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='soccerworld',
                 username='dev',
                 password='worldpw')


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
max_season <- max(all_leagues_squads_tm$season)

# create the vectors of the league names and ids
leagues <- c("bundesliga", "2-bundesliga", "premier-league", "ligue-1")
league_ids <- c("L1", "L2", "GB1", "FR1")

# do the actual scraping 
all_leagues_squads_new <- get_squads_by_season_all_leagues(leagues,
                                                           league_ids,
                                                           max_season)


# filter for only those matches that are not already in the data base
all_leagues_squads_new <- all_leagues_squads_tm %>%
  # filter only for the current season
  filter(season == max_season,
         # drop the serie a and la liga
         league %in% leagues) %>%
  # use an anti join to get all of those matches that are not in the data base
  anti_join(all_leagues_squads_new, 
            by = c("league", "season", "club", "player_name" ))

# if the all_leagues_squads_new element is not null, i.e.,
# there was new data available, we write the new data (by appending it)
# into the data base
if(!is.null(all_leagues_squads_new)){
  
  dbWriteTable(con, "all_leagues_squads_tm", 
               all_leagues_squads_new,
               overwrite = FALSE, append = TRUE)
}




# ########## get fifa world ranking top nations ############
# # get the newest date in the data base
# max_date <- fifa_top_15_nations %>%
#   filter(date == max(date)) %>%
#   select(date) %>%
#   pull() %>%
#   unique()
# 
# # extract the new fifa top 15 ranking table
# fifa_top_15_nations_new <- get_fifa_world_ranking_top_nations(max_date,
#                                                               top_nations = 15)
# 
# 
# # if the fifa_top_15_nations_new element is not null, i.e.,
# # there was new data available, we write the new data (by appending it)
# # into the data base
# if(!is.null(fifa_top_15_nations_new)){
#   
#   dbWriteTable(con, "fifa_top_15_nations", 
#                fifa_top_15_nations_new,
#                overwrite = FALSE, append = TRUE)
# }



########## get the historical lineups ############
max_season <- 2021

# create the vectors for the function parameters
leagues <- c("bundesliga", "2-bundesliga", "premier-league", "ligue-1")
league_ids <- c("L1", "L2", "GB1", "FR1")

# get the max matchday that is available for each league
max_matchday <- all_leagues_lineups_tm %>%
  filter(season == max_season) %>%
  group_by(league) %>%
  summarize(max_matchday = max(matchday))

# count the number of teams that should play on each matchday for each league
number_teams <- all_leagues_lineups_tm %>%
  filter(season == max_season) %>%
  group_by(league) %>%
  summarize(number_matches = n_distinct(team))


# create a variable to store leagues we need to scrape
missing_leagues <- NULL

# iterate over all matchdays in each league
for(i in 1:nrow(max_matchday)){
  # get the current league and matchday
  curr_league <- max_matchday$league[i]
  curr_matchday <- max_matchday$max_matchday[i]
  
  # compute the number of teams for the given matchday
  teams_played <- all_leagues_lineups_tm %>%
    filter(season == max_season,
           league == curr_league,
           matchday == curr_matchday) %>%
    summarize(number_teams = n_distinct(team)) %>%
    select(number_teams) %>%
    pull()
  
  # check if the the number of teams that played on the given matchday
  # matches with the number of matches
  if(number_teams$number_matches[i] > teams_played){
    # if there are teams not in the lineups table we store the league
    # and the matchday in the missing_leagues frame
    curr_info <- data.frame(c(curr_league, curr_league ),
                            c(curr_matchday, (curr_matchday + 1)))
    
    colnames(curr_info) <- c("league", "matchday")
    missing_leagues <- bind_rows(missing_leagues, curr_info)
  }
}


# create a variable to store the new lineups
all_leagues_lineups_tm_new <- NULL

# now iterate over those leagues and matchdays where there are not extracted
# games
for(i in 1:nrow(missing_leagues)){
  # get the current league and matchday
  curr_league <- missing_leagues$league[i]
  curr_matchday <- missing_leagues$matchday[i]
  
  # map the league name to the league ids
  league_id <- ifelse(curr_league == "Bundesliga",
                      "L1", ifelse(curr_league == "Bundesliga 2",
                                   "L2", ifelse(curr_league == "Premier League",
                                                "GB1", ifelse(curr_league == "Ligue 1",
                                                              "FR1", NA))))
  
  # extract the lineups for the current league and matchday
  curr_lineups <- get_lineups_by_season_tm(curr_league, league_id,
                                           season = max_season, matchday = curr_matchday)
  
  all_leagues_lineups_tm_new <- bind_rows(all_leagues_lineups_tm_new,
                                          curr_lineups)
}

# if the all_leagues_lineups_tm_new element is not null, i.e.,
# there was new data available, we write the new data (by appending it)
# into the data base
if(!is.null(all_leagues_lineups_tm_new)){
  
  dbWriteTable(con, "all_leagues_lineups_tm", 
               all_leagues_lineups_tm_new,
               overwrite = FALSE, append = TRUE)
}

dbDisconnect(con)
  



