# subserver for the model tab in the prediction menu item
prediction_model_server <- function(input, output, session){
  
  output$prediction_model_comparison<- renderPlotly({
    spi_prediction_data <- spi_prediction_acc()
    naive_baseline_data <- naive_baseline_acc()
    
    spi_prediction_data %>%
      inner_join(naive_baseline_data, by = c("league" = "league_name", 
                                             "season" = "league_season",
                                             "date" = "fixture_date",
                                             "home_team" = "club_name_home",
                                             "away_team" = "club_name_away")) %>%
      group_by(league) %>%
      plot_ly(x = ~ date, group = ~league, color = ~league) %>%
      add_lines(y = ~moving_accuracy_spi) %>%
      add_lines(y = ~moving_accuracy_naive)
  })
  
}