### script should update the spi data set daily ###
# get the current csv from the 538 web page 
all_leagues_spi_538_matches <- 
  read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv",
           encoding = "UTF-8") %>%
  # filter for only those leagues that are important to us
  filter(league %in% c("Barclays Premier League", "French Ligue 1",
                       "German Bundesliga", "German 2. Bundesliga")) %>%
  # drop the league id because we do not need it
  select(-league_id) %>%
  # rename the leagues to names we can work with
  mutate(league = ifelse(league == "Barclays Premier League",
                         "Premier League",
                               ifelse(league == "French Ligue 1",
                                      "Ligue 1",
                                      ifelse(league == "German Bundesliga",
                                             "Bundesliga",
                                             ifelse(league == "German 2. Bundesliga",
                                                    "Bundesliga 2",
                                                    league)))),
         # convert the date variable into an actual date
         date = ymd(date)) %>%
  # filter for only those matches that are in the past
  filter(date < Sys.Date()) %>%
  # reorder (and rename) all the variables we want to use
  select(league, season, date, home_team = team1, away_team = team2,
         home_team_spi = spi1, away_team_spi = spi2, home_team_win_prob = prob1,
         away_team_win_prob = prob2, draw_prob = probtie, 
         home_team_projected_score = proj_score1, 
         away_team_projected_score = proj_score2,
         home_team_importance = importance1, away_team_importance = importance2,
         home_team_goals = score1, away_team_goals = score2,
         home_team_expected_goals = xg1, away_team_expected_goals = xg2,
         home_team_non_shot_expected_goals = nsxg1, 
         away_team_non_shot_expected_goals = nsxg2,
         home_team_adjusted_goals = adj_score1, 
         away_team_adjusted_goals = adj_score2)

# map the club names (home and away) with the club_name_mapping function
all_leagues_spi_538_matches$home_team <- sapply(all_leagues_spi_538_matches$home_team,
                                                club_name_mapping)

all_leagues_spi_538_matches$away_team <- sapply(all_leagues_spi_538_matches$away_team,
                                                club_name_mapping)

# filter for only those matches that are not already in the data base
all_leagues_spi_538_matches_new <- all_leagues_spi_538_matches %>%
  # use an anti join to get all of those matches that are not in the data base
  anti_join(all_leagues_spi_538_available_matches, 
            by = c("season", "date", "league",
                   "home_team", "away_team")) %>%
  # filter for only those that are in the past
  filter(date < Sys.Date())

# only if there are new matches we do not have in the data base already
# we write them (by appending) it to the data base
if(nrow(all_leagues_spi_538_matches_new) != 0){
  
  dbWriteTable(con, "all_leagues_spi_538", all_leagues_spi_538_matches_new,
               overwrite = FALSE, append = TRUE)
}