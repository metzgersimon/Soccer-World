current_season <- 2021

buli_stats_kicker <- tbl(con, "buli_stats_kicker_2013_to_2021") %>%
  # filter(league_season == current_season) %>%
  data.frame()

# check for the latest available matchday
latest_matchday <- buli_stats_kicker %>%
  filter(season_start_year == current_season) %>%
  filter(matchday == max(matchday)) %>%
  select(matchday) %>%
  pull() %>%
  unique()

# get the newest matchday
newest_matchday <- latest_matchday + 1

# new matchday stats
newest_matchday_stats <- get_fixture_stats_kicker(league = "bundesliga",
                                                  season = current_season,
                                                  matchday = newest_matchday)

