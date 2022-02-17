############# shiny global #############
# global file should provide the shiny app with the needed
# packages and data 
# for this, we source the setup file to get all the needed functions
# and rdata objects for the shiny app


# source("Setup.R")
# set a color scheme
colors <- viridis_pal(option = "D")(30)
# create the seasons
seasons <- c(paste0(sort(c(2015:2021),
                         decreasing = TRUE),
                    "/", sort(c(2016:2022),
                              decreasing = TRUE)))

# setup a connection to the database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='Soccer_Prediction_Data',
                 username='root',
                 password='my-secret-pw')


dbListTables(con)

####### with datenbank
### match tab data
all_leagues_matches <- tbl(con, "all_leagues_matches") %>% data.frame()
all_leagues_spi_538_available_matches <- tbl(con, "all_leagues_spi_538") %>%
  data.frame()


### load team tab data
all_leagues_tm_squads <- tbl(con, "all_leagues_tm_squads") %>% data.frame()
all_leagues_market_values_over_time<- tbl(con, "all_leagues_market_values_over_time") %>%
  data.frame()
all_infos_club <- inner_join(all_leagues_tm_squads, unique(all_leagues_matches[,c(2,3,19)]), by=c("club"="club_name_home", "league"="league_name"))

### load player tab data
# all_fixture_stats <- tbl(con, "all_fixture_stats") %>% data.frame()
all_leagues_player_stats<- tbl(con, "all_leagues_player_stats") %>% data.frame()

# all_leagues_fixture_stats <- all_fixture_stats %>% left_join(all_leagues_matches, by = c("fixture_id"="fixture_id","league_id"="league_id"))
all_leagues_lineups<- tbl(con, "all_leagues_lineups") %>% data.frame()
all_leagues_fixture_stats<- tbl(con, "all_leagues_fixture_stats") %>% data.frame()

### load home tab data
all_leagues_venue_information<- tbl(con, "all_leagues_venue_information") %>% data.frame()

# dbDisconnect(con)
