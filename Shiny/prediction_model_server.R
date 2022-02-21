# subserver for the model tab in the prediction menu item
prediction_model_server <- function(input, output, session){
  
  # update the select input team according to the league selection
  observeEvent(input$information_model_league_selection, {
    updateSelectizeInput(
      session,
      inputId = "information_model_season_selection",
      choices = c("",
                  paste0(
                    unique(
                      all_leagues_historical_predictions %>%
                        filter(
                            league_name == input$information_model_league_selection
                        ) %>%
                        select(league_season) %>%
                        unlist() %>%
                        unname()
                    )
                    ,
                    "/",
                    unique(
                      all_leagues_historical_predictions %>% filter(
                          league_name == input$information_model_league_selection
                      ) %>%
                        select(league_season) %>%
                        unlist() %>%
                        unname()
                    ) + 1
                  )),
      selected = ""
    )
  })
  
  # create an observer to display for the season selection to display
  # only those players that are present in the selected club
  observeEvent(input$information_model_season_selection, {
    updateSelectizeInput(session, 
                         inputId = "information_model_matchday_selection",
                         choices = unique(all_leagues_historical_predictions %>%
                                            filter(league_name == input$information_model_league_selection &
                                                     league_season ==   as.numeric(
                                                       str_split(input$information_model_season_selection,
                                                                 pattern = "/")[[1]][1])) %>%
                                            select(league_round) %>%
                                            unlist() %>%
                                            unname()),
                         selected = ""
    )
  })
  
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
  


output$prediction_model_historical_results <- renderReactable({
  req(input$information_model_league_selection)
  req(input$information_model_season_selection)
  req(input$information_model_matchday_selection)
  req(input$prediction_model_type)
  
  if (input$prediction_model_lineups == FALSE) {
  result <- all_leagues_historical_predictions %>% 
    filter(league_name == input$information_model_league_selection,
           league_season == as.numeric(
             str_split(input$information_model_season_selection,
                       pattern = "/")[[1]][1]),
           league_round == input$information_model_matchday_selection) %>%
    mutate(game_score = paste0(fulltime_score_home, ":", 
                               fulltime_score_away),
           actual_difference = fulltime_score_home-fulltime_score_away,
           prediction = round(prediction,2)) %>%
    select(fixture_date,club_name_home,club_name_away, game_score,actual_difference,prediction) 
    
  result %>%
  reactable(
      defaultColDef = colDef(
        align = "center",
        minWidth = 150,
        headerStyle = list(background = "darkblue")
      ),
      # searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      borderless = TRUE,
      # set the theme for the table
      theme = reactableTheme(
        borderColor = "#000000",
        color = "#000000",
        backgroundColor = "#004157",
        highlightColor = "#2f829e",
        cellPadding = "8px 12px",
        style = list(color = "white")
        # searchInputStyle = list(width = "100%",
        #                         color = "black")
      )
      ,
      # modify the layout and names of the columns
      columns = list(
        fixture_date = colDef(name = "Date",
                              align = "left"),
        club_name_home = colDef(name = "Home club",
                             align = "center"),
        club_name_away = colDef(name = "Away club",
                                align = "center"),
        game_score = colDef(name = "Result",
                               align = "center"),
        actual_difference = colDef(name = "Difference",
                             align = "center"),
        prediction = colDef(name = "Prediction without lineup",
                                   align = "center")

      )
    )
  }else if (input$prediction_model_lineups == TRUE) {
    prediction_with_lineup <- all_leagues_historical_lineups_predictions %>% 
      filter(league_name == input$information_model_league_selection,
             league_season == as.numeric(
               str_split(input$information_model_season_selection,
                         pattern = "/")[[1]][1]),
             league_round == input$information_model_matchday_selection)  %>%
      mutate(prediction = round(prediction,2)) %>% select(prediction) %>% rename("prediction_withlineup"="prediction")
    
    result <- all_leagues_historical_predictions %>% 
      filter(league_name == input$information_model_league_selection,
             league_season == as.numeric(
               str_split(input$information_model_season_selection,
                         pattern = "/")[[1]][1]),
             league_round == input$information_model_matchday_selection) %>%
      mutate(game_score = paste0(fulltime_score_home, ":", 
                                 fulltime_score_away),
             actual_difference = fulltime_score_home-fulltime_score_away,
             prediction = round(prediction,2)) %>%
      select(fixture_date,club_name_home,club_name_away, game_score,actual_difference,prediction) 
    
    result %>% cbind(prediction_with_lineup) %>% 
      reactable(
      defaultColDef = colDef(
        align = "center",
        minWidth = 150,
        headerStyle = list(background = "darkblue")
      ),
      # searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      borderless = TRUE,
      # set the theme for the table
      theme = reactableTheme(
        borderColor = "#000000",
        color = "#000000",
        backgroundColor = "#004157",
        highlightColor = "#2f829e",
        cellPadding = "8px 12px",
        style = list(color = "white")
        # searchInputStyle = list(width = "100%",
        #                         color = "black")
      )
      ,
      # modify the layout and names of the columns
      columns = list(
        fixture_date = colDef(name = "Date",
                              align = "left"),
        club_name_home = colDef(name = "Home club",
                                align = "center"),
        club_name_away = colDef(name = "Away club",
                                align = "center"),
        game_score = colDef(name = "Result",
                            align = "center"),
        actual_difference = colDef(name = "Difference",
                                   align = "center"),
        prediction = colDef(name = "Prediction without lineup",
                            align = "center"),
        prediction_withlineup = colDef(name = "Prediction with lineup",
                            align = "center")
      )
    )
  }
})
# 
# output$prediction_model_historical_results_lineup <- renderReactable({
#   req(input$information_model_league_selection)
#   req(input$information_model_season_selection)
#   req(input$information_model_matchday_selection)
#   req(input$prediction_model_type)
#   
# if (input$prediction_model_lineups == TRUE) {
#   prediction_with_lineup <- all_leagues_historical_lineups_predictions %>% 
#     filter(league_name == input$information_model_league_selection,
#            league_season == as.numeric(
#              str_split(input$information_model_season_selection,
#                        pattern = "/")[[1]][1]),
#            league_round == input$information_model_matchday_selection)  %>%
#     mutate(prediction = round(prediction,2)) %>% select(prediction)
#   
#   
#        %>%
#     reactable(
#       defaultColDef = colDef(
#         align = "center",
#         minWidth = 150,
#         headerStyle = list(background = "darkblue")
#       ),
#       # searchable = TRUE,
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
#         style = list(color = "white")
#         # searchInputStyle = list(width = "100%",
#         #                         color = "black")
#       )
#       ,
#       # modify the layout and names of the columns
#       columns = list(
#         fixture_date = colDef(name = "Date",
#                               align = "left"),
#         club_name_home = colDef(name = "Home club",
#                                 align = "center"),
#         club_name_away = colDef(name = "Away club",
#                                 align = "center"),
#         game_score = colDef(name = "Result",
#                             align = "center"),
#         actual_difference = colDef(name = "Difference",
#                                    align = "center"),
#         prediction = colDef(name = "Prediction",
#                             align = "center")
#         
#       )
#     )
#   
# }
# })

}