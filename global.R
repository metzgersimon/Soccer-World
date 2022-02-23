############# shiny global #############
# global file should provide the shiny app with the needed
# packages and data 
# for this, we source the setup file to get all the needed functions
# and rdata objects for the shiny app

source("Setup.R")
# set a color scheme
colors <- viridis_pal(option = "D")(30)
# create the seasons
seasons <- c(paste0(sort(c(2016:2021),
                         decreasing = TRUE),
                    "/", sort(c(2017:2022),
                              decreasing = TRUE)))

# setup a connection to the database
con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='soccerworld',
                 username='dev',
                 password='worldpw')


####### load the data from database
### match tab data
# load leagues matches infos
all_leagues_matches <-
  tbl(con, "all_leagues_matches") %>% data.frame()

# load spi data
all_leagues_spi_538 <- tbl(con, "all_leagues_spi_538") %>%
  data.frame()

# load odds data 
buli_all_seasons_odds <- tbl(con, "all_leagues_odds") %>% data.frame()

# load fixture infos
all_leagues_fixture_events <-
  tbl(con, "all_leagues_fixture_events") %>% data.frame()

# load fixture lineup
all_leagues_fixture_lineups <-
  tbl(con, "all_leagues_fixture_lineups") %>% data.frame()

### load team tab data
# load squads infos
all_leagues_squads_tm <-
  tbl(con, "all_leagues_squads_tm") %>% data.frame()

# load market value over time data
all_leagues_market_values_over_time <-
  tbl(con, "all_leagues_market_values_over_time") %>%
  data.frame()

# filter the infos from 2016
filter_all_leagues_squads_tm <-
  all_leagues_squads_tm %>% filter(season >= 2016)

# merge the squads infos with the matches info for the team tab data
all_infos_club <-
  inner_join(
    filter_all_leagues_squads_tm,
    unique(all_leagues_matches[, c(2, 3, 19)]),
    by = c("club" = "club_name_home", "league" =
             "league_name")
  )

# load club statistics data
all_leagues_club_stats <- tbl(con, "all_leagues_club_stats") %>%
  data.frame()

colnames_club_stats <- colnames(all_leagues_club_stats)

colnames_club_stats <-
  str_replace_all(colnames_club_stats, pattern = "\\.", "_")

colnames(all_leagues_club_stats) <- colnames_club_stats

# fill the na value
all_leagues_club_stats[all_leagues_club_stats ==-999 | all_leagues_club_stats ==-999.00] <- NA

### load player tab data
# load player statistics
all_leagues_player_stats <-
  tbl(con, "all_leagues_player_stats") %>% data.frame()

# load player lineup infos
all_leagues_lineups_tm <-
  tbl(con, "all_leagues_lineups_tm") %>% data.frame()

# load fixture stats infos
all_leagues_fixture_stats <-
  tbl(con, "all_leagues_fixture_stats") %>% data.frame()

# load fifa rating stats
all_leagues_fifa_team_stats <-
  tbl(con, "all_leagues_fifa_team_stats") %>% data.frame()

# load fifa player infos
all_leagues_fifa_squads <-
  tbl(con, "all_leagues_fifa_squads") %>% data.frame()

# load the player transfer infos
all_leagues_team_transfers <-
  tbl(con, "all_leagues_team_transfers") %>% data.frame()

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
  summarise_if(is.numeric, sum) %>% ungroup() %>% select(
    league_season:team_name,
    games_captain:offsides,
    tackles_total:tackles_interception,
    dribbles_attempts:cards_red
  )

# get the player stats (calculate mean for some variable like games_minutes) per season
y <-
  filter_player_stats %>% group_by(league_season, player_name, league_name, team_name, team_logo) %>%
  summarise_if(is.numeric, mean) %>% ungroup() %>% select(league_season:team_name,
                                                          games_minutes,
                                                          contains("passes"),
                                                          contains("duels"))

# merge two player stats
player_stats <-
  inner_join(x,
             y,
             by = c("league_season", "player_name", "league_name", "team_name")) %>% select(-contains("x"))

# filter the season larger than 2015 so that we don't have many missing values
# as the player stats starts from 2015
filter_all_leagues_squads_tm <-
  all_leagues_squads_tm %>% filter(season >= 2016)


# merge the stats with the basic infos of players
player_tab_data <-
  player_stats %>% inner_join(
    filter_all_leagues_squads_tm,
    by = c(
      "team_name" = "club",
      "player_name" = "player_name",
      "league_season" = "season"
    )
  )

### load home tab data
all_leagues_venue_information <-
  tbl(con, "all_leagues_venue_information") %>% data.frame()


### load model tab data
# historical prediction results without lineup
all_leagues_historical_predictions <-
  tbl(con, "all_leagues_historical_predictions") %>%
  data.frame() %>%
  right_join(
    all_leagues_matches,
    by = c(
      "fixture_id",
      "league_id",
      "league_season",
      "league_round",
      "club_id_home",
      "club_id_away"
    )
  )

# historical prediction results with lineup 
all_leagues_historical_lineups_predictions <-
  tbl(con, "all_leagues_historical_lineups_predictions") %>% data.frame() %>% right_join(
    all_leagues_matches,
    by = c(
      "fixture_id",
      "league_id",
      "league_season",
      "league_round",
      "club_id_home",
      "club_id_away"
    )
  )


