train_xgb <- function(){
  model_data <- get_model_data()
  
  model_data <- model_data %>%
    select(-c("fixture_date.x",
              "fixture_date.y", "fixture_time.x", "league_name.x",
              "team_name_home", "team_name_away",
              "fulltime_score_home.x", "fulltime_score_away.x",
              "fulltime_score_home.y", "fulltime_score_away.y")) %>%
    rename(goal_diff = goal_diff.x)
  
  model_data <- model_data[, !sapply(model_data, is.character)]
  
  set.seed(42)
  train_index <-  createDataPartition(model_data$goal_diff, p = .8,
                                      list = FALSE,
                                      times = 1)
  
  train_data <- model_data[train_index, ]
  test_data <- model_data[-train_index, ]
  
  lab <- as.numeric(train_data[, "goal_diff"])
  train_data <- data.matrix(train_data[, !(colnames(train_data) %in%
                                             c("goal_diff", "fixture_id"))])
  
  
  
  
  m1_xgb <-
    xgboost::xgboost(
      data =  train_data,
      label = as.numeric(lab),
      nrounds = 1000,
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
  future_predictions <- model_data
  future_predictions$prediction <- 
    predict(m1_xgb, data.matrix(future_predictions[, !(colnames(future_predictions) %in%
                                                             c("goal_diff", "fixture_id"))]))
  
  # only take important variables
  future_predictions <- future_predictions %>%
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
  
  return(future_predictions)
}