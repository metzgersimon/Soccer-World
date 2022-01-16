########### get the new fixtures #########
# current matches in the data base
current_season <- 2021


# write_new_data_in_db <- function()

buli_matches <- tbl(con, "buli_matches_2010_2021") %>%
  # filter(league_season == current_season) %>%
  data.frame()

# check for the latest available matchday
latest_matchday <- buli_matches %>%
  filter(league_season == current_season,
         !is.na(fulltime_score_home) &
           !is.na(fulltime_score_away)) %>%
  filter(league_round == max(league_round)) %>%
  select(league_round) %>%
  pull() %>%
  unique()

# get the newest matchday
newest_matchday <- latest_matchday + 1


newest_matchday_data <- get_fixtures_in_league_by_season(league_id = 78,
                                                         season = current_season,
                                                         matchday = newest_matchday)

buli_matches_new <- buli_matches %>%
  filter(league_season == current_season,
         league_round != newest_matchday) %>%
  bind_rows(newest_matchday_data) %>%
  arrange(league_round, fixture_date, fixture_time)

buli_matches_2010_2021 <- buli_matches %>%
  filter(league_season != current_season) %>%
  bind_rows(buli_matches_new)

# write the new data in the data base (overwrite the old one)
dbWriteTable(con, name = "buli_matches_2010_2021", buli_matches_2010_2021,
             overwrite = TRUE)



############## get new match stats ##################

# get all the match stats for the new matches
newest_matchday_stats <- NULL

buli_match_stats <- tbl(con, "buli_match_stats_2015_2021") %>%
  # filter(league_season == current_season) %>%
  data.frame()

for(i in 1:nrow(newest_matchday_data)){
  match_i_id <- newest_matchday_data$fixture_id[i]
  match_i_stats <- get_fixture_stats(match_i_id)
  
  newest_matchday_stats <- bind_rows(newest_matchday_stats,
                                     match_i_stats)
  
}

buli_match_stats <- bind_rows(buli_match_stats,
                              newest_matchday_stats)
