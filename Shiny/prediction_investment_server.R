# subserver for the investment tab in the prediction menu item
prediction_investment_server <- function(input, output, session){
  
  output$prediction_investment_plot<- renderPlotly({
    # test_data2 %>%
    #   plot_ly(x = ~goal_diff, y = ~buli_prediction) %>%
    #   add_markers()
  })
  
}