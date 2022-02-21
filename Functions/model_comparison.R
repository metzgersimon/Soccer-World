# function returns a data set with the running accuracy of the 538
# predictions for the matches
spi_prediction_acc <- function(){
  spi_predictions <- all_leagues_spi_538 %>%
    select(league, season, date, home_team, away_team,
           home_team_win_prob, draw_prob, away_team_win_prob) %>%
    inner_join(all_leagues_matches,
               by = c("league" = "league_name", 
                      "season" = "league_season",
                      "date" = "fixture_date",
                      "home_team" = "club_name_home",
                      "away_team" = "club_name_away")) %>%
    # remove all matches that have been postponed or cancelled
    filter(status_long == "Match Finished") %>%
    mutate(prediction = ifelse(home_team_win_prob > draw_prob &
                                 home_team_win_prob > away_team_win_prob,
                               "home",
                               ifelse(draw_prob > home_team_win_prob &
                                        draw_prob > away_team_win_prob,
                                      "draw",
                                      ifelse(away_team_win_prob > home_team_win_prob &
                                               away_team_win_prob > draw_prob,
                                             "away",
                                             NA))),
           # add a variable for the correct predictions
           correct_predicted = ifelse(prediction == "home" &
                                        home_points == 3,
                                      TRUE, ifelse(prediction == "draw" &
                                                     home_points == 1,
                                                   TRUE, ifelse(prediction == "away" &
                                                                  home_points == 0,
                                                                TRUE, FALSE)))) %>%
    # remove rows where the prediction is NA
    filter(!is.na(prediction)) %>%
    mutate(moving_accuracy_spi = runMean(correct_predicted, n = 2, cumulative = TRUE))
  
  
  
  return(spi_predictions)
}



# function returns a data set with the running accuracy of a naive
# benchmark for the predictions. In this case we always predict
# the home team to win
naive_baseline_acc <- function(){
  naive_baseline_acc <- all_leagues_matches %>%
    # only past matches
    filter(status_long == "Match Finished",
           league_season >= 2016) %>%
    # select only important variables
    select(league_name, league_season, league_round, fixture_date,
           club_name_home, club_name_away, home_points) %>%
    # create a variable for our prediction
    mutate(prediction = ifelse(home_points == 3,
                               TRUE, FALSE),
           # compute the moving accuracy 
           moving_accuracy_naive = runMean(prediction, n = 2, cumulative = TRUE))
  
  return(naive_baseline_acc)
  
}


# function returns a data set with the running accuracy of a the odds as
# a benchmark for the predictions.
odds_acc <- function(){
  odds_acc <- buli_all_seasons_odds %>%
    # we use the final odds as a benchmark, i.e., these odds that are
    # given directly before the match
    # select only important variables
    select(league, season, fixture_date, fixture_time, home_team, away_team,
           home_win_odd_end, draw_odd_end, away_win_odd_end) %>%
    inner_join(all_leagues_matches,
               by = c("league" = "league_name", 
                      "season" = "league_season",
                      "fixture_date",
                      "fixture_time",
                      "home_team" = "club_name_home",
                      "away_team" = "club_name_away")) %>%
    # remove all matches that have been postponed or cancelled
    filter(status_long == "Match Finished") %>%
    # create a variable for the prediction
    mutate(prediction = ifelse(home_win_odd_end <= draw_odd_end &
                                 home_win_odd_end <= away_win_odd_end,
                               "home",
                               ifelse(draw_odd_end < home_win_odd_end &
                                        draw_odd_end < away_win_odd_end,
                                      "draw",
                                      ifelse(away_win_odd_end <= home_win_odd_end &
                                               away_win_odd_end <= draw_odd_end,
                                             "away",
                                             NA))),
           # add a variable for the correct predictions
           correct_predicted = ifelse(prediction == "home" &
                                        home_points == 3,
                                      TRUE, ifelse(prediction == "draw" &
                                                     home_points == 1,
                                                   TRUE, ifelse(prediction == "away" &
                                                                  home_points == 0,
                                                                TRUE, FALSE)))) %>%
    # remove rows where the prediction is NA
    filter(!is.na(prediction)) %>%
    mutate(moving_accuracy_odds = runMean(correct_predicted, n = 2, cumulative = TRUE))
  
  return(odds_acc)
  
}
