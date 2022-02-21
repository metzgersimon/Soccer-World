
# this function should train a linear regression with the data
train_regression <- function(){
  
  # get the data we need for the model
  model_data <- get_model_data()
  
  # drop columns we do not need
  model_data_all2 <- model_data %>%
    select(-c(league_name.x.x, fixture_date.x, fixture_time.x,
              league_logo, league_flag, venue_name, venue_city, referee, 
              status_long, status_short, status_elapsed, club_name_home, club_name_away,
              club_logo_home, league_id.y, fixture_date.y, fixture_time.y,
              team_id_home, team_id_away, team_name_home, team_name_away,
              team_logo_home, team_logo_away, league_name.x, league_name.y,
              league_name.y.y, league_id.y, league_country, 
              fixture_id, venue_id, halftime_score_home, halftime_score_away,
              fulltime_score_home.x, fulltime_score_away.x, home_points, away_points,
              club_name,  fulltime_score_away.y, fulltime_score_home.y, club_name_away, matchday,
              league_country_home, league_country_away, league_logo_home,
              league_logo_away, league_flag_home, league_flag_away, team_name_home_home,
              team_name_away_away, contains("cards"), contains("current_form"),
              matches("goals.*minute"))) 
  
  # case 1 -> replace all missing values with a value that should represent
  # NA in a numeric way (-999)
  model_data_all_no_NA <- model_data_all2 %>%
    mutate(across(c(home_team_spi:failed_to_score_total_away),
                  ~ replace_na(.x, -999)))
  
  # apple z-score standardization to the interests
  league_and_ids <- select(model_data_all_no_NA, league_id.x, club_id_home,
                           league_season, league_round,
                           club_id_away)
  
  # now make dummy variables from the ids and league information
  league_and_ids_dummy <- league_and_ids %>%
    # convert the variables into factors
    mutate(across(c(league_id.x:club_id_away), as.factor)) %>%
    fastDummies::dummy_columns(.) %>%
    select(-c(league_id.x, club_id_home,
              league_season, league_round,
              club_id_away))
  
  # take only the statistic values
  model_data_only_numerics <- select(model_data_all_no_NA, -c(league_id.x, club_id_home,
                                                              league_season, league_round,
                                                              club_id_away))
  
  # scale the numeric values
  model_data_all_no_NA_scaled <- as.data.frame(lapply(model_data_only_numerics, scale))
  
  
  # combine the scaled data with the league ids again
  model_data_normalized <- bind_cols(league_and_ids_dummy, model_data_all_no_NA_scaled)
  
  
  
  ###### custom feature selection ####
  
  # case 1 -> replace all missing values with a value that should represent
  # NA in a numeric way (-999)
  model_data_all_no_NA <- model_data_all2 %>%
    mutate(across(c(home_team_spi:failed_to_score_total_away),
                  ~ replace_na(.x, -999)))
  
  
  ### impute missing values
  library(Hmisc)
  model_data_all_no_NA <- model_data_all2 %>%
    mutate(across(c(home_team_spi:failed_to_score_total_away),
                  ~ impute(.x, "mean")))
  
  
  # select only specific features
  model_custom_features <- model_data_all_no_NA %>%
    select(league_id.x, league_season, league_round,
           club_id_home, club_id_away, home_team_spi, away_team_spi,
           contains("fifa"), home_team_points:away_team_loss_pct,
           shots_total_home, shots_total_away, contains("average"),
           goal_diff)
  
  
  # apple z-score standardization to the data
  league_and_ids <- select(model_custom_features, league_id.x, club_id_home,
                           league_season, league_round,
                           club_id_away)
  
  # now make dummy variables from the ids and league information
  league_and_ids_dummy <- league_and_ids %>%
    # convert the variables into factors
    mutate(across(c(league_id.x:club_id_away), as.factor)) %>%
    fastDummies::dummy_columns(.) %>%
    select(-c(league_id.x, club_id_home,
              league_season, league_round,
              club_id_away))
  
  model_custom_features <- model_custom_features %>%
    select(-c(league_id.x, club_id_home,
              league_season, league_round,
              club_id_away))
  
  # combine the scaled data with the league ids again
  model_custom_features <- bind_cols(league_and_ids_dummy, model_custom_features)
  
  set.seed(42)
  train_index <-  createDataPartition(model_custom_features$goal_diff, p = .8,
                                      list = FALSE,
                                      times = 1)
  # 
  train_data <- model_custom_features[train_index, ]
  test_data <- model_custom_features[-train_index, ]
  
  
  train_control <- trainControl(
    method = "cv",
    number = 20)
  
  
  # normal linear regression
  regr_model <- train(goal_diff ~ .,
                      data = train_data,
                      method = "lm",
                      trControl = train_control)
  
  summary(regr_model)
  
  regr_predict <- predict(regr_model, select(test_data, -goal_diff))
  
  regr_acc <- sum((sign(regr_predict) == sign(test_data$goal_diff))) / length(regr_predict)
  
  
  
}




train_buli_model <- function(){
  # past_matches <- model_data_all %>%
  #   drop_na()
  
}
