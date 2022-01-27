# global file should provide the shiny app with the needed
# packages and data 
# for this, we source the setup file to get all the needed functions
# and rdata objects for the shiny app
source("Setup.R")
# set a color scheme
colors <- viridis_pal(option = "D")(30)
# create the seasons
seasons <- c(paste0(sort(c(2015:2021), 
                         decreasing = TRUE),
                    "/", sort(c(2016:2022), 
                              decreasing = TRUE))
)

# connect to the SQL server
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='testdb',
                 username='root',
                 password='my-secret-pw')


# dbGetQuery(con, "SELECT * FROM Persons")
buli_matches_2010_2021 <- tbl(con, "buli_matches_2010_2021") %>%
  data.frame()
buli_fixture_events_2010_to_2021 <- tbl(con, "buli_fixture_events_2010_to_2021") %>%
  data.frame()
#market_values_over_time <- tbl(con, "market_values_over_time") %>%
#data.frame()
# buli_fixture_stats_2015_2021 <- tbl(con, "buli_fixture_stats_2015_2021") %>%
#data.frame()

major_five_league_transfers <- tbl(con, "major_five_league_transfers") %>%
  data.frame()
squads_by_season_2010_2021 <- tbl(con, "squads_by_season_2010_2021") %>%
  data.frame()


########## as we have datenbank, we don't need to mapping the names ##############
# map all club names in all available data frames 
# with the mapping function club_name_mapping we created
# to be able to join over the names
# all_seasons_running_table$club <- sapply(all_seasons_running_table$club,
#                                        club_name_mapping)

# all_season_infos$team_name <- sapply(all_season_infos$team_name,
#                                          club_name_mapping)

#squads_season_2020$club_name <- sapply(squads_season_2020$club_name,
#club_name_mapping)

#fixtures_bundesliga_2010_2021$club_name_home <-
#  sapply(fixtures_bundesliga_2010_2021$club_name_home,
#         club_name_mapping)

#fixtures_bundesliga_2010_2021$club_name_away <-
#  sapply(fixtures_bundesliga_2010_2021$club_name_away,
#         club_name_mapping)

#all_seasons_from_2010_squads$club_name <-
#  sapply(all_seasons_from_2010_squads$club_name,
#         club_name_mapping)


# combine all data frames together
# season_players_joined <- all_seasons_running_table %>%
#   inner_join(all_seasons_from_2010_squads,
#              by = c("club" = "club_name",
#                     "season_start_year" = "season")) %>%
#   inner_join(all_season_infos,
#              by = c("club" = "team_name"))
##############################################################################

# fixtures_with_stats 2021
fixtures_with_stats_2021 <- all_fixture_stats  %>%
  left_join(fixtures_bundesliga_2010_2021, by = "fixture_id")

fixtures_with_stats_2021$team_name <- sapply(fixtures_with_stats_2021$team_name,
                                             club_name_mapping)

fixtures_with_stats_2021$club_name_home <- sapply(fixtures_with_stats_2021$club_name_home,
                                             club_name_mapping)

fixtures_with_stats_2021$club_name_away <- sapply(fixtures_with_stats_2021$club_name_away,
                                             club_name_mapping)

market_values_over_time$club <- sapply(market_values_over_time$club,
                                       club_name_mapping)

#bayern_transfers$from_team_name <- sapply(bayern_transfers$from_team_name,
#                                          club_name_mapping)

#bayern_transfers$to_team_name <- sapply(bayern_transfers$to_team_name,
#                                          club_name_mapping)

#all_transfers$from_team_name <- sapply(all_transfers$from_team_name,
#                                          club_name_mapping)

#all_transfers$to_team_name <- sapply(all_transfers$to_team_name,
#                                        club_name_mapping)

player_stats_2021_buli$team_name <- sapply(player_stats_2021_buli$team_name,
                                           club_name_mapping)

#buli_fixture_events_2019_2021_18_12_21$team_name <- sapply(buli_fixture_events_2019_2021_18_12_21$team_name,
#                                                           club_name_mapping)

buli_fixture_events_2010_to_2021$team_name <- sapply(buli_fixture_events_2010_to_2021$team_name,
                                                           club_name_mapping)