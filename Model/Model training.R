
train_buli_model <- function(){
  # past_matches <- model_data_all %>%
  #   drop_na()
  future_matches <- model_data_all %>%
    filter(is.na(goal_diff))
  
  past_matches <- model_data_all %>%
    filter(!(fixture_id %in% future_matches$fixture_id))
  
  
  
  # create training and test sets with 80% training and 20% testing
  set.seed(42)
  train_index <-  createDataPartition(past_matches$goal_diff, p = .8,
                                      list = FALSE,
                                      times = 1)
  # 
  train_data <- past_matches[train_index, ]
  test_data <- past_matches[-train_index, ]
  
  
  train_control <- trainControl(
    method = "cv",
    number = 10)
  
  set.seed(42)
  # normal linear regression
  buli_model <- train(goal_diff ~ .,
                      data = train_data,
                      method = "lm",
                      trControl = train_control)
  
  summary(buli_model)
}