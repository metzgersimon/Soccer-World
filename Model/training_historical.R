
# function trains the historical lineup model (with a xgb)
# once and write the resulting table to the data base
train_xgb_historical_lineups <- function(){
  # get the data for the lineup model
  data_lineups <- get_model_data_lineups()
  
  # drop columns we do not want
  historical_lineups_predictions <- data_lineups %>%
    select(-c("fixture_date.x", "league_name.x.x",
              "fixture_date.y", "fixture_time.x", "league_name.x",
              "venue_id", "club_name_home", "club_name_away",
              "status_elapsed", "season", "matchday",
              "team_id_home", "team_id_away",
              "club_name" ,"halftime_score_home","halftime_score_away", 
              "fulltime_score_home.x", "fulltime_score_away.x",
              "home_points", "away_points",
              "home_team_goal_diff", "away_team_goal_diff",
              "fulltime_score_home.y", "fulltime_score_away.y"))
  
  # also drop all character columns
  historical_lineups_predictions <- 
    historical_lineups_predictions[, !sapply(historical_lineups_predictions, is.character)]
  
  # set the seeds
  set.seed(42)
  
  # create a train/test split
  train_index <-  createDataPartition(historical_lineups_predictions$goal_diff, p = .8,
                                      list = FALSE,
                                      times = 1)
  # extract the train and test data
  train_data <- historical_lineups_predictions[train_index, ]
  test_data <- historical_lineups_predictions[-train_index, ]
  
  
  # create the trainControl 
  train_control <- trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE)
  
  
  lab <- as.numeric(train_data[, "goal_diff"])
  # create a data matrix with the training data and remove the goal_diff column
  train_data <- data.matrix(train_data[, !(colnames(train_data) %in%
                                             c("goal_diff", "fixture_id"))])
  
  # train the xgb model and using the squarederror as objective function
  m1_xgb <-
    xgboost::xgboost(
      data =  train_data,
      label = as.numeric(lab),
      nrounds = 5000,
      objective = "reg:squarederror",
      early_stopping_rounds = 3,
      max_depth = 2,
      eta = .15
    )  
  
  # feature importance 
  importance_matrix <- xgb.importance(colnames(data.matrix(train_data[, !(colnames(train_data) %in%
                                                                            c("goal_diff", "fixture_id"))])), model = m1_xgb)
  xgb.plot.importance(importance_matrix[1:20])
  
  # out of sample rmse 
  results <- predict(m1_xgb, data.matrix(test_data[, colnames(test_data) != "goal_diff"]))
  
  RMSE(results, test_data$goal_diff)
  
  
  ####### historical predictions #######
  historical_lineups_predictions$prediction <- 
    predict(m1_xgb, data.matrix(historical_lineups_predictions[, !(colnames(historical_lineups_predictions) %in%
                                                             c("goal_diff", "fixture_id"))]))
  
  # only take important variables
  historical_lineups_predictions <- historical_lineups_predictions %>%
    select(league_id = league_id.x, league_season,
           league_round, fixture_id, club_id_home, club_id_away,
           prediction)

  dbWriteTable(con, "all_leagues_historical_lineups_predictions", historical_lineups_predictions)
}




# function trains the historical model (with a xgb)
# once and write the resulting table to the data base
train_xgb_historical <- function(){
  # get the data for the plain model
  model_data <- get_model_data()
  
  # drop columns we do not want
  historical_predictions <- model_data %>%
    select(-c("fixture_date.x", "league_name.x.x",
              "fixture_date.y", "fixture_time.x", "league_name.x",
              "venue_id", "club_name_home", "club_name_away",
              "status_elapsed", "season", "matchday",
              "team_id_home", "team_id_away",
              "club_name" ,"halftime_score_home","halftime_score_away", 
              "fulltime_score_home.x", "fulltime_score_away.x",
              "home_points", "away_points",
              "home_team_goal_diff", "away_team_goal_diff",
              "fulltime_score_home.y", "fulltime_score_away.y"))
  
  # also drop all character columns
  historical_predictions <- 
    historical_predictions[, !sapply(historical_predictions, is.character)]
  
  # set the seeds
  set.seed(42)
  
  # create a train/test split
  train_index <-  createDataPartition(historical_predictions$goal_diff, p = .8,
                                      list = FALSE,
                                      times = 1)
  # extract the train and test data
  train_data <- historical_predictions[train_index, ]
  test_data <- historical_predictions[-train_index, ]
  
  
  # create the trainControl 
  train_control <- trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE)
  
  lab <- as.numeric(train_data[, "goal_diff"])
  # create a data matrix with the training data and remove the goal_diff column
  train_data <- data.matrix(train_data[, !(colnames(train_data) %in%
                                             c("goal_diff", "fixture_id"))])
  
  # train the xgb model and using the squarederror as objective function
  m1_xgb <-
    xgboost::xgboost(
      data =  train_data,
      label = as.numeric(lab),
      nrounds = 5000,
      objective = "reg:squarederror",
      early_stopping_rounds = 3,
      max_depth = 2,
      eta = .15
    )  
  
  # feature importance 
  importance_matrix <- xgb.importance(colnames(data.matrix(train_data[, !(colnames(train_data) %in%
                                                                             c("goal_diff", "fixture_id"))])), model = m1_xgb)
  xgb.plot.importance(importance_matrix[1:20])
  
  # out of sample rmse 
  results <- predict(m1_xgb, data.matrix(test_data[, !(colnames(test_data) %in%
                                                         c("goal_diff", "fixture_id"))]))
  
  RMSE(results, test_data$goal_diff)
  
  
  ####### historical predictions #######
  historical_predictions$prediction <- 
    predict(m1_xgb, data.matrix(historical_predictions[, !(colnames(historical_predictions) %in%
                                                                     c("goal_diff", "fixture_id"))]))
  
  # only take important variables
  historical_predictions <- historical_predictions %>%
    select(league_id = league_id.x, league_season,
           league_round, fixture_id, club_id_home, club_id_away,
           prediction)
  
  dbWriteTable(con, "all_leagues_historical_predictions", historical_predictions,
               overwrite = TRUE)
}






# function should regularly train on the data to predict future matches
train_xgb_lineups <- function(){
  data_lineups <- get_model_data_lineups()
  
  data_lineups <- data_lineups %>%
    select(-c("fixture_date.x", "league_name.x.x",
              "fixture_date.y", "fixture_time.x", "league_name.x",
              "venue_id", "club_name_home", "club_name_away",
              "status_elapsed", "season", "matchday",
              "team_id_home", "team_id_away",
              "club_name" ,"halftime_score_home","halftime_score_away", 
              "fulltime_score_home.x", "fulltime_score_away.x",
              "home_points", "away_points",
              "home_team_goal_diff", "away_team_goal_diff",
              "fulltime_score_home.y", "fulltime_score_away.y"))
  
  data_lineups <- data_lineups[, !sapply(data_lineups, is.character)]
  
  set.seed(42)
  train_index <-  createDataPartition(data_lineups$goal_diff, p = .8,
                                      list = FALSE,
                                      times = 1)

  train_data <- data_lineups[train_index, ]
  test_data <- data_lineups[-train_index, ]
  
  
  
  train_control <- trainControl(
    method = "cv",
    number = 10)
  
  
  lab <- as.numeric(train_data[, "goal_diff"])
  train_data <- data.matrix(train_data[, colnames(train_data) != "goal_diff"])
  
  
  
  
  m1_xgb <-
    xgboost::xgboost(
      data =  train_data,
      label = as.numeric(lab),
      nrounds = 5000,
      objective = "reg:squarederror",
      early_stopping_rounds = 3,
      max_depth = 2,
      eta = .15
    )  
  
  # feature importance 
  importance_matrix <- xgb.importance(colnames(data.matrix(train_data[, colnames(train_data) != "goal_diff"])), model = m1_xgb)
  xgb.plot.importance(importance_matrix[1:20])
  
  # out of sample rmse 
  results <- predict(m1_xgb, data.matrix(test_data[, colnames(test_data) != "goal_diff"]))
 
  RMSE(results, test_data$goal_diff)
  
  
  ####### historical predictions #######
  historical_predictions$prediction <- 
    predict(m1_xgb, data.matrix(historical_predictions[, !(colnames(historical_predictions) %in%
                                                             c("goal_diff", "fixture_id"))]))
  
  # only take important variables
  historical_predictions <- historical_predictions %>%
    select(league_id = league_id.x, league_season,
           league_round, fixture_id, club_id_home, club_id_away,
           prediction)
  
  
  # choose cutoff points based on traing data
  tar <- sign(lab)
  ex <- predict(m1_xgb, data.matrix(train_data))
  why <- data.frame(tar, ex)
  
  # from the previos decision tree we estimate the cutoff points 0.63 and -0.58
  cutoff <- rpart(tar ~ ex, data = why, method = "class")
  rpart.plot::rpart.plot(cutoff)
  rpart.plot(cutoff)
  
  
  tttt <- sum(sign(results) == sign(test_data$goal_diff)) / length(results)
  
  tt <- predict(cutoff , newdata = data.frame(ex = results), type = "class")
  ttt <- sum(as.numeric(as.character(tt)) == sign(test_data$goal_diff)) / length(results)
}