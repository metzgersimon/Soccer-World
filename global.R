############# shiny global #############
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
                              decreasing = TRUE)))

# setup a connection to the database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='Soccer_Prediction_Data',
                 username='root',
                 password='my-secret-pw')


####### with datenbank
### match tab data
all_leagues_matches <- tbl(con, "all_leagues_matches") %>% data.frame()

all_league_fixture_stats <- all_leagues_matches %>% left_join(all_fixture_stats, by = c("league_season"="season", "club_name_home"="team_name"))
### team tab data
all_leagues_tm_squads <- tbl(con, "all_leagues_tm_squads") %>% data.frame()

### player tab data
all_fixture_stats <- tbl(con, "all_fixture_stats") %>% data.frame()
all_leagues_player_stats<- tbl(con, "all_leagues_player_stats") %>% data.frame()

### home tab data
all_leagues_venue_information<- tbl(con, "all_leagues_venue_information") %>% data.frame()


### match infos
#buli_matches_2010_2021 <-
#  tbl(con, "buli_matches_2010_2021") %>% data.frame()
#buli_fixture_events_2010_to_2021  <-
#  tbl(con, "buli_fixture_events_2010_to_2021") %>% data.frame()

# convert buli_match_stats_2015_2021
#buli_match_stats_2015_2021 <-
#  buli_match_stats_2015_2021 %>% pivot_wider(c("fixture_date","fixture_time"),names_from = "shots_on_goal", value="shots_on_goal")
  

### player infos
#major_five_league_transfers <-
#  tbl(con, "major_five_league_transfers") %>% data.frame()
#buli_fixture_lineups_2015_2021 <-
#  tbl(con, "buli_fixture_lineups_2015_2021") %>% data.frame()
#major_six_leagues_team_infos_2010_2021 <-
#  tbl(con, "major_six_leagues_team_infos_2010_2021") %>% data.frame()
# infos player
#player_team_join <-
#major_six_leagues_team_infos_2010_2021%>% left_join(buli_squads_tm, by = c("team_name" = "club", "season"))
# transfer infos player
#transfer_join <-
#  major_five_league_transfers %>% left_join(major_six_leagues_team_infos_2010_2021, by =
 #                                                        "team_id")
### team infos
#fifa_team_stats_buli_2015_2021<- tbl(con, "fifa_team_stats_buli_2015_2021") %>% data.frame()



###### old
# match infos
all_leagues_spi_538_available_matches <- tbl(con, "all_leagues_spi_538") %>%
  data.frame()

# player infos
major_five_league_transfers <- tbl(con, "major_five_league_transfers") %>% data.frame()
fifa_squads_buli2_2015_2022<- tbl(con, "fifa_squads_buli2_2015_2022") %>% data.frame()
fifa_squads_buli_2015_2022<- tbl(con, "fifa_squads_buli_2015_2022") %>% data.frame()
buli_fixture_lineups_2015_2021<- tbl(con, "buli_fixture_lineups_2015_2021") %>% data.frame()
buli_squads_tm <- tbl(con, "buli_squads_tm") %>% data.frame()

player_team_join <- major_six_leagues_team_infos_2010_2021 %>% left_join(buli_squads_tm, by=c("team_name"="club"))


######################## old ################
# map all club names in all available data frames 
# with the mapping function club_name_mapping we created
# to be able to join over the names
#all_seasons_running_table$club <- sapply(all_seasons_running_table$club,
#                                         club_name_mapping)

#all_season_infos$team_name <- sapply(all_season_infos$team_name,
#                                         club_name_mapping)

# combine all data frames together
#season_players_joined <- all_seasons_running_table %>%
#  inner_join(all_seasons_from_2010_squads,
#             by = c("club" = "club_name",
#                    "season_start_year" = "season")) 
#%>%
#  inner_join(all_season_infos,
#             by = c("club" = "team_name"))







