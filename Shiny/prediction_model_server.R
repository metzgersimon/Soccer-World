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

# output$prediction_model_historical_results <- renderReactable({
#   req(input$information_model_league_selection)
#   req(input$information_model_season_selection)
#   req(input$information_league_matchday_selection)
#   req(input$information_player_season_selection)      
#   
#   player_stats() %>%
#     select(fixture_date,
#            contains("goals")) %>%
#     reactable(
#       defaultColDef = colDef(
#         align = "center",
#         minWidth = 150,
#         headerStyle = list(background = "darkblue")
#       ),
#       searchable = TRUE,
#       striped = TRUE,
#       highlight = TRUE,
#       borderless = TRUE,
#       # set the theme for the table
#       theme = reactableTheme(
#         borderColor = "#000000",
#         color = "#000000",
#         backgroundColor = "#004157",
#         highlightColor = "#2f829e",
#         cellPadding = "8px 12px",
#         style = list(color = "white"),
#         searchInputStyle = list(width = "100%",
#                                 color = "black")
#       )
#       ,
#       # modify the layout and names of the columns
#       columns = list(
#         fixture_date = colDef(name = "Date",
#                               align = "left"),
#         goals_total = colDef(name = "Total goals",
#                              align = "center"),
#         goals_conceded = colDef(name = "Conceded goals",
#                                 align = "center"),
#         goals_assists = colDef(name = "Assist goals",
#                                align = "center"),
#         goals_saves = colDef(name = "Saved goals",
#                              align = "center")
#         
#       )
#     )
# })




