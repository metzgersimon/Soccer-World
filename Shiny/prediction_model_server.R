# subserver for the model tab in the prediction menu item
prediction_model_server <- function(input, output, session) {
  # update the select input team according to the league selection
  observeEvent(
    input$prediction_model_league_selection,
    {
      updateSelectizeInput(
        session,
        inputId = "prediction_model_season_selection",
        choices = c("",
                    paste0(
                      unique(
                        all_leagues_historical_predictions %>%
                          filter(league_name == input$prediction_model_league_selection) %>%
                          select(league_season) %>%
                          unlist() %>%
                          unname()
                      )
                      ,
                      "/",
                      unique(
                        all_leagues_historical_predictions %>% 
                          filter(league_name == input$prediction_model_league_selection) %>%
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
  observeEvent(input$prediction_model_season_selection, {
    updateSelectizeInput(
      session,
      inputId = "prediction_model_matchday_selection",
      choices = unique(
        all_leagues_historical_predictions %>%
          filter(
            league_name == input$prediction_model_league_selection &
              league_season ==   as.numeric(
                str_split(input$prediction_model_season_selection,
                          pattern = "/")[[1]][1]
              )
          ) %>%
          select(league_round) %>%
          unlist() %>%
          unname() %>%
          na.omit()
      ),
      selected = ""
    )
  })
  
  output$prediction_model_comparison <- renderPlotly({
    req(input$prediction_model_bm)
    req(input$prediction_model_league_selection)
    
    bm_selection <- input$prediction_model_bm
    league_selection <- input$prediction_model_league_selection
    
    # bm_selection <- "FiveThirtyEight"
    # league_selection <- "Bundesliga"
    
    # based on the lineup checkbox get the matching model accuracy
    if(input$prediction_model_lineups){
      model_acc <- lineup_model_acc()
    } else {
      model_acc <- plain_model_acc()
    }
    
    # select the benchmark based on the user selection
    if(bm_selection == "FiveThirtyEight"){
      bm_prediction_data <- spi_prediction_acc() %>%
        rename(moving_accuracy_bm = moving_accuracy_spi)
      
      plot_data <- model_acc %>%
        inner_join(
          bm_prediction_data,
          by = c(
            "league_name" = "league",
            "league_season" = "season",
            "fixture_date" = "date",
            "club_name_home" = "home_team",
            "club_name_away" = "away_team"
          )
        )
      
    } else if(bm_selection == "Always Home"){
      bm_prediction_data <- naive_baseline_acc() %>%
        rename(moving_accuracy_bm = moving_accuracy_naive)
      
      plot_data <- model_acc %>%
        inner_join(
          bm_prediction_data,
          by = c(
            "league_name",
            "league_season",
            "league_round",
            "fixture_date",
            "club_name_home",
            "club_name_away"
          )
        )
      
    } else {
      bm_prediction_data <- odds_acc() %>%
        rename(moving_accuracy_bm = moving_accuracy_odds)
      
      plot_data <- model_acc %>%
        inner_join(
          bm_prediction_data,
          by = c(
            "league_name" = "league",
            "league_season" = "season",
            "league_round",
            "fixture_date",
            "club_name_home" = "home_team",
            "club_name_away" = "away_team"
          )
        )
    } 
    
    plot_data %>%
      filter(league_name == league_selection) %>%
      group_by(league_name) %>%
      plot_ly(x = ~ fixture_date,
              group = ~ league_name,
              color = ~ league_name) %>%
      add_lines(y = ~ moving_accuracy_model,
                name = paste0("Model ", league_selection),
                line = list(color = "#1b9e77")) %>%
      add_lines(y = ~ moving_accuracy_bm,
                name = paste0("Benchmark ", league_selection),
                line = list(color = "#d95f02")) %>%
      layout(font = list(color = "white"),
             plot_bgcolor = "#567895",
             paper_bgcolor = "#567895",
             fig_bg_color = "#567895",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Accuracy"),
             title = list(text = "Accuracy over time", y = 0.95, x = 0.5))
  })
  
  
  
  output$prediction_model_historical_results <- renderReactable({
    req(input$prediction_model_league_selection)

    if (input$prediction_model_lineups == FALSE) {
      result <- all_leagues_historical_predictions %>%
        filter(
          league_name == input$prediction_model_league_selection,
          league_season == as.numeric(
            str_split(input$prediction_model_season_selection,
                      pattern = "/")[[1]][1]
          ),
          league_round == input$prediction_model_matchday_selection
        ) %>%
        mutate(
          game_score = paste0(fulltime_score_home, ":",
                              fulltime_score_away),
          actual_difference = fulltime_score_home - fulltime_score_away,
          prediction = round(prediction, 0)
        ) %>%
        select(
          fixture_date,
          club_name_home,
          club_name_away,
          game_score,
          actual_difference,
          prediction
        )
      
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
          defaultPageSize = 30,
          showPageInfo = FALSE,
          showPagination = FALSE,
          # set the theme for the table
          theme = reactableTheme(
            borderColor = "#BFC9D1",
            color = "#BFC9D1",
            backgroundColor = "#567895",
            highlightColor = "#CE8B65",
            cellPadding = "8px 12px",
            style = list(color = "white"),
            searchInputStyle = list(width = "100%")
          )
          ,
          # modify the layout and names of the columns
          columns = list(
            fixture_date = colDef(name = "Date",
                                  align = "left"),
            club_name_home = colDef(name = "Home team",
                                    align = "center"),
            club_name_away = colDef(name = "Away team",
                                    align = "center"),
            game_score = colDef(name = "Result",
                                align = "center"),
            actual_difference = colDef(name = "Goal Difference",
                                       align = "center"),
            prediction = colDef(name = "Predicted Goal Difference",
                                align = "center")
            
          )
        )
    } else if (input$prediction_model_lineups == TRUE) {
      prediction_with_lineup <-
        all_leagues_historical_lineups_predictions %>%
        filter(
          league_name == input$prediction_model_league_selection,
          league_season == as.numeric(
            str_split(input$prediction_model_season_selection,
                      pattern = "/")[[1]][1]
          ),
          league_round == input$prediction_model_matchday_selection
        )  %>%
        mutate(prediction = round(prediction, 2)) %>% select(prediction) %>% rename("prediction_withlineup" =
                                                                                      "prediction")
      
      result <- all_leagues_historical_predictions %>%
        filter(
          league_name == input$prediction_model_league_selection,
          league_season == as.numeric(
            str_split(input$prediction_model_season_selection,
                      pattern = "/")[[1]][1]
          ),
          league_round == input$prediction_model_matchday_selection
        ) %>%
        mutate(
          game_score = paste0(fulltime_score_home, ":",
                              fulltime_score_away),
          actual_difference = fulltime_score_home - fulltime_score_away,
          prediction = round(prediction, 2)
        ) %>%
        select(
          fixture_date,
          club_name_home,
          club_name_away,
          game_score,
          actual_difference,
          prediction
        )
      
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
          defaultPageSize = 30,
          showPageInfo = FALSE,
          showPagination = FALSE,
          # set the theme for the table
          theme = reactableTheme(
            borderColor = "#BFC9D1",
            color = "#BFC9D1",
            backgroundColor = "#567895",
            highlightColor = "#CE8B65",
            cellPadding = "8px 12px",
            style = list(color = "white"),
            searchInputStyle = list(width = "100%")
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
  
  
  
  output$prediction_model_future_results <- renderReactable({
    req(input$prediction_model_league_selection)

    if (input$prediction_model_lineups == FALSE) {
      result <- all_leagues_historical_predictions %>%
        filter(
          league_name == input$prediction_model_league_selection,
          fixture_date >= Sys.Date(),
          is.na(prediction) == FALSE
        ) %>%
        mutate(
          prediction = round(prediction, 0)
        ) %>%
        select(
          fixture_date,
          club_name_home,
          club_name_away,
          prediction
        )
      
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
          pagination = FALSE,
          defaultPageSize = 30,
          showPageInfo = FALSE,
          showPagination = FALSE,
          # set the theme for the table
          theme = reactableTheme(
            borderColor = "#BFC9D1",
            color = "#BFC9D1",
            backgroundColor = "#567895",
            highlightColor = "#CE8B65",
            cellPadding = "8px 12px",
            style = list(color = "white"),
            searchInputStyle = list(width = "100%")
          )
          ,
          # modify the layout and names of the columns
          columns = list(
            fixture_date = colDef(name = "Date",
                                  align = "left"),
            club_name_home = colDef(name = "Home team",
                                    align = "center"),
            club_name_away = colDef(name = "Away team",
                                    align = "center"),
            prediction = colDef(name = "Predicted Goal Difference",
                                align = "center")
            
          )
        )
    } else if (input$prediction_model_lineups == TRUE) {
      prediction_with_lineup <-
        all_leagues_historical_lineups_predictions %>%
        filter(
          league_name == input$prediction_model_league_selection,
          fixture_date >= Sys.Date(),
          is.na(prediction) == FALSE
        )  %>%
        mutate(prediction = round(prediction, 0)) %>% select(prediction) %>% rename("prediction_withlineup" =
                                                                                      "prediction")
      
      result <- all_leagues_historical_predictions %>%
        filter(
          league_name == input$prediction_model_league_selection,
          fixture_date >= Sys.Date(),
          is.na(prediction) == FALSE
        ) %>%
        mutate(
          prediction = round(prediction, 0)
        ) %>%
        select(
          fixture_date,
          club_name_home,
          club_name_away,
          prediction
        )
      
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
          defaultPageSize = 30,
          showPageInfo = FALSE,
          showPagination = FALSE,
          # set the theme for the table
          theme = reactableTheme(
            borderColor = "#BFC9D1",
            color = "#BFC9D1",
            backgroundColor = "#567895",
            highlightColor = "#CE8B65",
            cellPadding = "8px 12px",
            style = list(color = "white"),
            searchInputStyle = list(width = "100%")
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
            prediction = colDef(name = "Prediction without lineup",
                                align = "center"),
            prediction_withlineup = colDef(name = "Prediction with lineup",
                                           align = "center")
          )
        )
    }
  })
  
  
  
}