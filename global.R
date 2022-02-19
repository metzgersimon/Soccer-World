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
                 dbname='soccerworld',
                 username='dev',
                 password='worldpw')


####### with data base
### match tab data
all_leagues_matches <- tbl(con, "all_leagues_matches") %>% data.frame()
all_leagues_spi_538_available_matches <- tbl(con, "all_leagues_spi_538") %>%
  data.frame()


### load team tab data
all_leagues_squads_tm <- tbl(con, "all_leagues_squads_tm") %>% data.frame()
all_leagues_market_values_over_time <- tbl(con, "all_leagues_market_values_over_time") %>%
  data.frame()
all_infos_club <- inner_join(all_leagues_tm_squads, unique(all_leagues_matches[,c(2,3,19)]), 
                             by=c("club"="club_name_home", "league"="league_name"))

all_leagues_club_stats <- tbl(con, "all_leagues_club_stats") %>%
  data.frame()
all_leagues_team_transfers <- tbl(con, "all_leagues_team_transfers") %>%
  data.frame()

### load player tab data
# all_fixture_stats <- tbl(con, "all_fixture_stats") %>% data.frame()
all_leagues_player_stats <- tbl(con, "all_leagues_player_stats") %>% data.frame()

all_leagues_lineups_tm <- tbl(con, "all_leagues_lineups_tm") %>% data.frame()
all_leagues_fixture_stats <- tbl(con, "all_leagues_fixture_stats") %>% data.frame()

all_leagues_fifa_team_stats <- tbl(con, "all_leagues_fifa_team_stats") %>% data.frame()
all_leagues_fifa_squads <- tbl(con, "all_leagues_fifa_squads") %>% data.frame()

# merge all the player tab data for the overview and statistics tab
filter_player_stats <- 
  all_leagues_player_stats %>% select(
    league_id,
    league_name,
    league_country,
    league_season,
    fixture_date,
    league_round,
    team_name,
    team_logo,
    player_name,
    player_photo,
    games_minutes:cards_red
  )

# get the player stats summary (calculate sum for some variable like offsides) per season
x <-
  filter_player_stats %>% group_by(league_season, player_name, league_name, team_name, team_logo) %>%
  summarise_if(is.numeric, sum) %>% ungroup() %>%select(league_season:team_name,games_captain:offsides, tackles_total:tackles_interception,dribbles_attempts:cards_red)

# get the player stats (calculate mean for some variable like games_minutes) per season

y <-
  filter_player_stats %>% group_by(league_season, player_name, league_name, team_name, team_logo) %>%
  summarise_if(is.numeric, mean) %>%ungroup() %>% select(league_season:team_name,games_minutes, contains("passes"), contains("duels"))

# merge two player stats
player_stats <- inner_join(x,y, by=c("league_season", "player_name", "league_name", "team_name")) %>% select(-contains("x"))

# filter the season larger than 2015 so that we don't have many missing values
# as the player stats starts from 2015
filter_all_leagues_squads_tm <- all_leagues_squads_tm %>% filter(season>=2015)

# merge the stats with the basic infos of players
player_tab_data <-
  player_stats %>% inner_join(filter_all_leagues_squads_tm, by = c("team_name"="club", "player_name"="player_name", "league_season"="season"))

### load home tab data
all_leagues_venue_information <- tbl(con, "all_leagues_venue_information") %>% data.frame()
