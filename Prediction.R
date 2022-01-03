# current_fixtures_2021 <- get_fixtures_in_league_by_season(78, 2021)
# fixtures_bundesliga_2010_2021 <- fixtures_bundesliga_2010_2021 %>%
#   filter(league_season != 2021) %>%
#   bind_rows(current_fixtures_2021) %>%
#   mutate(goal_diff = fulltime_score_home - fulltime_score_away) %>%

model_data <- fixtures_bundesliga_2010_2021 %>%
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

# %>%
#   mutate(last_5_matches_win_pct = sum(goal_diff > 0))


bayern_data <- model_data %>%
  filter(club_id_home == 157 |
           club_id_away == 157)

past_data <- model_data %>%
  filter(league_season < 2021) %>%
  group_by(league_season, venue_name) %>%
  mutate(home_winning_pct = sum((goal_diff > 0)/17) * 100,
         away_winning_pct = sum((goal_diff > 0)/17) * 100)

model_data <- fixtures_bundesliga_2010_2021 %>%
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
                                     0))) %>%
  group_by(league_season, club_id_home) %>%
  mutate(cum_points_home = cumsum(points_home),
         cum_goals_home = cumsum(fulltime_score_home)) %>%
  ungroup() %>%
  group_by(league_season, club_id_away) %>%
  mutate(cum_points_away = cumsum(points_away)) %>%
  ungroup() %>%
  


# join the regeneration days
model_data <- model_data %>%
  mutate(fixture_date = ymd(fixture_date)) %>%
  left_join(buli_2000_2021_regen_days_only_buli, by = c("league_season" = "season_start_year",
                                              "league_round" = "matchday",
                                              "fixture_date" = "date",
                                              "club_name_home" = "home_team",
                                              "club_name_away" = "away_team"))


         # exp_weighted_avg_points_home2 = runMedian(points_home, n = 1))#,

model_data$exp_weighted_avg_points_home <- movavg(model_data$points_home, n = 3, type = "e")
         #rolling_avg_points_home = movavg(points_home, n = 00, type = "e")) %>%
   
  
bayern_data <- model_data %>% filter(club_id_home == 157 | club_id_away == 157)
  
# # add another variable which is the goal difference
model_data <- model_data %>%
  # mutate(goal_diff = fulltime_score_home - fulltime_score_away) %>%
  select(#fixture_date, fixture_time,
    venue_name, venue_city, #league_id, league_name,
    league_season, league_round, club_id_home, #club_name_home,
    
    
         club_id_away, #club_name_away, 
    #points_home, points_away, 
    cum_points_home, cum_points_away,
    #regeneration_days,
    #halftime_score_home, halftime_score_away,
   # fulltime_score_home, fulltime_score_away,
    goal_diff) %>%
  filter(#!is.na(fulltime_score_away),
         #!is.na(fulltime_score_home),
         !is.na(league_round)) #%>%
  # mutate(halftime_score_away = ifelse(is.na(halftime_score_away),
  #                                     0,
  #                                     halftime_score_away),
  #        halftime_score_home = ifelse(is.na(halftime_score_home),
  #                                     0,
  #                                     halftime_score_home))

past_matches <- model_data %>%
  filter(!is.na(goal_diff))

future_matches <- model_data %>%
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

test_data2 <- cbind(test_data, buli_prediction) %>%
  mutate(buli_prediction = round(buli_prediction)) %>%
  summarize(accuracy = sum(goal_diff == buli_prediction) / nrow(.) * 100)



postResample(pred = buli_prediction, obs = test_data$goal_diff)


############## prediction und unmplayed games ###############
buli_prediction_unplayed <- predict(buli_model, future_matches)

future_matches_with_prediction <- cbind(future_matches, buli_prediction_unplayed) %>%
  mutate(buli_prediction_unplayed = round(buli_prediction_unplayed, 0))












################################################################################
################################# test #########################################
model_data <- buli_2000_2021_regen_days_only_buli %>%
  separate(col = result, into = c("home_goals", "away_goals"), sep = ":") %>%
  mutate(home_goals = as.numeric(home_goals),
         away_goals = as.numeric(away_goals),
         goal_diff = home_goals - away_goals) %>%
  select(season_start_year, matchday, date, time, home_team,
         away_team, goal_diff, regeneration_days)

past_matches <- model_data %>%
  filter(!is.na(goal_diff),
         !is.na(regeneration_days))

future_matches <- model_data %>%
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
  filter(league_season >= 2013)

buli_matches_with_stats <- fixtures_bundesliga_2013_2021 %>%
  inner_join(buli_stats_kicker_2013_to_2021_agg, by = c("club_name_home" = "team_name",
                                                        "club_name_away" = "team_name",
                                                        "league_season" = "season_start_year",
                                                        "league_round" = "matchday"))
