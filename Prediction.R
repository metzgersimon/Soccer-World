###################### aggregated stats data ###############################
buli_stats_kicker_2013_to_2021_agg <- buli_stats_kicker_2013_to_2021 %>%
  group_by(team_name) %>%
  mutate(goals_moving_avg_2_cum = runMean(goals, n = 2, cumulative = TRUE),
         goals_moving_avg_2 = runMean(goals, n = 2),
         
         shots_on_goal_moving_avg_2_cum = runMean(shots_on_goal, n = 2, cumulative = TRUE),
         shots_on_goal_moving_avg_2 = runMean(shots_on_goal, n = 2),
         
         passes_moving_avg_2_cum = runMean(passes, n = 2, cumulative = TRUE),
         passes_moving_avg_2 = runMean(passes, n = 2),
         
         passes_accurate_moving_avg_2_cum = runMean(passes_accurate, n = 2, cumulative = TRUE),
         passes_accurate_moving_avg_2 = runMean(passes_accurate, n = 2),
         
         passes_failed_moving_avg_2_cum = runMean(passes_failed, n = 2, cumulative = TRUE),
         passes_failed_moving_avg_2 = runMean(passes_failed, n = 2),
         
         passing_accuracy_moving_avg_2_cum = runMean(passing_accuracy, n = 2, cumulative = TRUE),
         passing_accuracy_moving_avg_2 = runMean(passing_accuracy, n = 2),
         
         possession_moving_avg_2_cum = runMean(possession, n = 2, cumulative = TRUE),
         possession_moving_avg_2 = runMean(possession, n = 2),
         
         duel_quota_moving_avg_2_cum = runMean(duel_quota, n = 2, cumulative = TRUE),
         duel_quota_moving_avg_2 = runMean(duel_quota, n = 2),
         
         fouls_committed_moving_avg_2_cum = runMean(fouls_committed, n = 2, cumulative = TRUE),
         fouls_committed_moving_avg_2 = runMean(fouls_committed, n = 2),
         
         fouls_against_moving_avg_2_cum = runMean(fouls_against, n = 2, cumulative = TRUE),
         fouls_against_moving_avg_2 = runMean(fouls_against, n = 2),
         
         offside_moving_avg_2_cum = runMean(offside, n = 2, cumulative = TRUE),
         offside_moving_avg_2 = runMean(offside, n = 2),
         
         corners_against_moving_avg_2_cum = runMean(corners, n = 2, cumulative = TRUE),
         corners_against_moving_avg_2 = runMean(corners, n = 2),
         
         distance_ran_km_moving_avg_2_cum = runMean(distance_ran_km, n = 2, cumulative = TRUE),
         distance_ran_km_moving_avg_2 = runMean(distance_ran_km, n = 2)) %>%
  ungroup()


############ join stats with match data #########################
fixtures_bundesliga_2013_2021 <- fixtures_bundesliga_2010_2021 %>%
  filter(league_season >= 2013) %>%
  mutate(goal_diff = fulltime_score_home - fulltime_score_away,
         points_home = ifelse(goal_diff > 0,
                              3,
                              ifelse(goal_diff == 0,
                                     1,
                                     0)),
         points_away = ifelse(goal_diff < 0,
                              3,
                              ifelse(goal_diff == 0,
                                     1,
                                     0))) 

test <- buli_stats_kicker_2013_to_2021_agg %>%
  group_by(match_group = rep(row_number(), length.out = n(), each = 2)) %>%
  ungroup() 

home_frame <- test %>%
  group_by(match_group) %>%
  filter(row_number(desc(match_group)) == 1)

away_frame <- test %>%
  group_by(match_group) %>%
  filter(row_number(desc(match_group)) == 2)

all_frame <- home_frame %>%
  inner_join(away_frame, by = c("match_group", "league", "season_start_year", 
                                "season_end_year", "matchday", "date", "time")) %>%
  rename(club_name_home = `team_name.x`,
         club_name_away = `team_name.y`)

all_frame$club_name_home <- sapply(all_frame$club_name_home,
                                   club_name_mapping)

all_frame$club_name_away <- sapply(all_frame$club_name_away,
                                   club_name_mapping)



fixtures_with_stats <- fixtures_bundesliga_2013_2021 %>%
  mutate(fixture_date = ymd(fixture_date)) %>%
  left_join(all_frame, by = c("fixture_date" = "date",
                              "fixture_time" = "time",
                              "league_season" = "season_start_year",
                              "league_round" = "matchday",
                              "club_name_home" = "club_name_home",
                              "club_name_away" = "club_name_away")) %>%
  select(-referee, -venue_id,
         -fixture_id, -league, -`is_home_team.x`,
         -fixture_time, -fixture_date,
         -`is_home_team.y`, - timezone,
         -status_long, -status_short, -status_elapsed,
         -league_id, -league_name, -league_country, -league_logo,
         -league_flag, -is_winner_home, -is_winner_away,
         -halftime_score_away, -halftime_score_home, -fulltime_score_home,
         -fulltime_score_away, -points_home, -points_away,
         -c(goals.x,
         shots_on_goal.x, 
         passes.x,
         passes_accurate.x, 
         passes_failed.x,
         passing_accuracy.x,   
         possession.x,
         duel_quota.x,
         fouls_committed.x,
         fouls_against.x, 
         offside.x,
         corners.x, 
         distance_ran_km.x,
         goals.y,
         shots_on_goal.y, 
         passes.y,
         passes_accurate.y, 
         passes_failed.y,
         passing_accuracy.y,   
         possession.y,
         duel_quota.y,
         fouls_committed.y,
         fouls_against.y, 
         offside.y,
         corners.y, 
         distance_ran_km.y))



# past_matches <- fixtures_with_stats %>%
#   .past_matches[complete.cases(past_matches),]
past_matches <- fixtures_with_stats %>%
  drop_na()
# past_matches <- fixtures_with_stats[complete.cases(fixtures_with_stats),]
  # filter(!is.na(goal_diff),
  #        !is.na(offside_moving_avg_2.x))

future_matches <- fixtures_with_stats %>%
  filter(is.na(goal_diff))


# create training and test sets with 80% training and 20% testing
set.seed(42)
train_index <-  createDataPartition(past_matches$goal_diff, p = .8, 
                                    list = FALSE, 
                                    times = 1)

train_data <- past_matches[train_index, ]
test_data <- past_matches[-train_index, ]


# create the parameter tuning with 10-fold cross validation
train_control <- trainControl(
  method = "cv",
  number = 10)#,
## repeated ten times
#repeats = 10)

set.seed(42)
# normal linear regression
buli_model <- train(goal_diff ~ .,
                    data = train_data,
                    method = "lm",
                    trControl = train_control)

summary(buli_model)

# random forest


# train_data2 <- select(train_data, -c(league_season, league_round))
# buli_model <- lm(goal_diff ~ ., data = train_data2)

buli_prediction <- predict(buli_model, test_data)

