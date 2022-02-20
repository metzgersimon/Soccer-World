
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
  
  set.seed(45)
  # normal linear regression
  buli_model <- train(goal_diff ~ .,
                      data = train_data,
                      method = "lm",
                      trControl = train_control)
  
  summary(buli_model)
}

library(rpart)
library(rpart.plot)
library(xgboost)


xgbtest <- function() {

train_data <- past_matches[train_index, ]
test_data <- past_matches[-train_index, ]

train_data <- train_data %>%
                  select(-c("fixture_date.x", "fixture_id", "league_name.x.x",
                            "fixture_date.x", "fixture_time.x", "league_name.x",
                            "venue_id", "club_name_home", "club_name_away",
                            "club_name" ,"halftime_score_home","halftime_score_away", 
                            "fulltime_score_home.x", "fulltime_score_away.x",
                            "home_points", "away_points",
                            "home_team_importance",
                            "home_team_goal_diff", "away_team_goal_diff"))

train_data <- train_data[, !sapply(train_data, is.character)]

test_data <- test_data %>%
                          select(-c("fixture_date.x", "fixture_id", "league_name.x.x",
                                    "fixture_date.x", "fixture_time.x", "league_name.x",
                                    "venue_id", "club_name_home", "club_name_away",
                                    "club_name" ,"halftime_score_home","halftime_score_away", 
                                    "fulltime_score_home.x", "fulltime_score_away.x",
                                    "home_points", "away_points", 
                                     "home_team_importance", "home_team_goal_diff", 
                                    "away_team_goal_diff"))

test_data <- test_data[, !sapply(test_data, is.character)]

lab <- as.numeric(train_data[, "goal_diff"])
train_data <- data.matrix(train_data[, colnames(train_data) != "goal_diff"])




m1_xgb <-
  xgboost::xgboost(
    data =  train_data,
    label = as.numeric(lab),
    nrounds = 50000,
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

# performance across years
try <- test_data
try$correct <- (as.numeric(as.character(tt)) == sign(test_data$goal_diff))
try %>% 
  group_by(league_season) %>%
  summarise(s = sum(correct), n = n()) %>%
  mutate(freq = s / n)

# performance across matchdays 
sh <- try %>% 
  group_by(league_round) %>%
  summarise(s = sum(correct), n = n()) %>%
  mutate(freq = s / n)


test_data %>% count(league_season)


}

# all ================

model_data_all_pred <-  model_data_all %>%
  select(-c("fixture_date.x", "fixture_id", "league_name.x.x",
            "fixture_date.x", "fixture_time.x", "league_name.x",
            "venue_id", "club_name_home", "club_name_away",
            "club_name" ,"halftime_score_home","halftime_score_away", 
            "fulltime_score_home.x", "fulltime_score_away.x",
            "home_points", "away_points",
            "home_team_importance",
            "home_team_goal_diff", "away_team_goal_diff"))

model_data_all_pred <- model_data_all_pred[, !sapply(model_data_all_pred, is.character)]
model_data_all_pred <- data.matrix(model_data_all_pred[, colnames(model_data_all_pred) != "goal_diff"])

model_data_all$prediction <- predict(m1_xgb, model_data_all_pred )

model_data_all_mer <- model_data_all %>% select(c(prediction, fixture_id))

finaltest <- all_leagues_matches %>% left_join(model_data_all_mer, by = c("fixture_id" = "fixture_id" ))

dbWriteTable()
dbWriteTable(con, "all_leagues_matches", finaltest, overwrite = TRUE )

finaltest2 <- finaltest %>% filter(is.na(prediction))                 

xgbtest()

dbWriteTable()
db_write_table()
