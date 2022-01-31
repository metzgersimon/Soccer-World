
spi_538_available_matches2 <- tbl(con, "spi_538_matches") %>%
  data.frame()

spi_538_matches <- 
  read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv",
           encoding = "UTF-8") %>%
  filter(league %in% c("Barclays Premier League", "Italy Serie A",
                       "Spanish Primera Division", "French Ligue 1",
                       "German Bundesliga", "German 2. Bundesliga")) %>%
  select(-league_id) %>%
  mutate(league = ifelse(league == "Barclays Premier League",
                         "Premier League",
                         ifelse(league == "Italy Serie A",
                                "Serie A",
                                ifelse(league == "Spanish Primera Division",
                                       "La Liga",
                                       ifelse(league == "French Ligue 1",
                                              "Ligue 1",
                                              ifelse(league == "German Bundesliga",
                                                     "Bundesliga",
                                                     ifelse(league == "German 2. Bundesliga",
                                                            "Bundesliga 2",
                                                            league)))))),
         date = ymd(date)) %>%
  filter(date < Sys.Date()) %>%
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

spi_538_matches_new <- spi_538_matches %>%
  anti_join(spi_538_available_matches, by = c("season", "date", "league",
                                              "home_team", "away_team")) %>%
  filter(date < Sys.Date())

if(nrow(spi_538_matches_new) != 0){
  
  dbWriteTable(con, "spi_538_matches", spi_538_matches_new,
               overwrite = FALSE, append = TRUE)
}
  
# spi_538_past_matches_all_important_leagues <- spi_538_past_matches

# dbWriteTable(con, name = "spi_538_past_matches_all_important_leagues", spi_538_past_matches_all_important_leagues)

# spi_538_buli <- spi_538_past_matches %>% filter(league == "Bundesliga")
# spi_538_buli$team1 <- sapply(spi_538_buli$team1, club_name_mapping)
# spi_538_buli$team2 <- sapply(spi_538_buli$team2, club_name_mapping)


# spi_538_global_ranking <- 
#   read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv",
#            encoding = "UTF-8") %>%
#   filter(str_detect(str_to_lower(league), pattern = "premier league|bundesliga|serie a|ligue 1|primera division"))
# 
# global_ranking_buli1 <- spi_538_global_ranking %>%
#   filter(league == "German Bundesliga") %>%
#   mutate(league = "Bundesliga") %>%
#   select(spi_rank = rank, team_name = name,
#          league, spi_off = off, spi_def = def,
#          spi)
# 
# global_ranking_buli1$team_name <- sapply(global_ranking_buli1$team_name, 
#                                          club_name_mapping)
# 
# global_ranking_buli2 <- spi_538_global_ranking %>%
#   filter(league == "German 2. Bundesliga") %>%
#   mutate(league = "Bundesliga 2") %>%
#   select(spi_rank = rank, team_name = name,
#          league, spi_off = off, spi_def = def,
#          spi)
# 
# global_ranking_buli2$team_name <- sapply(global_ranking_buli2$team_name, 
#                                          club_name_mapping)
# 
# buli_buli2_spi_rankings <- bind_rows(global_ranking_buli1,
#                                      global_ranking_buli2)

# global_ranking_pl <- spi_538_global_ranking %>%
#   filter(league == "Barclays Premier League")
# 
# global_ranking_serie_a <- spi_538_global_ranking %>%
#   filter(league == "Italy Serie A")
# 
# global_ranking_la_liga <- spi_538_global_ranking %>%
#   filter(league == "Spanish Primera Division")
# 
# global_ranking_ligue1 <- spi_538_global_ranking %>%
#   filter(league == "French Ligue 1")

# test <- read_html(base_url)
