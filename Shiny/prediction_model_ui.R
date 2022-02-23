# sub-ui for the model tab in the prediction menu item
tab_prediction_model_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "prediction-model",
          fluidRow(
            column(
              width = 3,
              align = "center",
              selectizeInput(
                "prediction_model_league_selection",
                label = "League",
                choices = c("Bundesliga",  # four choices for leagues
                            "Bundesliga 2",
                            "Premier League",
                            "Ligue 1")
              )
            ),
            column(
              width = 3,
              align = "center",
              selectizeInput(
                "prediction_model_season_selection",
                label = "Season",
                choices = seasons,
                selected = seasons[1]
              )
            ),
            column(
              width = 3,
              align = "center",
              selectizeInput(
                "prediction_model_matchday_selection",
                label = "Matchday",
                choices = c("All"),
                selected = ""
              )
            )
          ),  fluidRow(column(
            width = 2,
            align = "center",
            div(
              style = "margin-top: 20px;",
              selectInput(
                "prediction_model_bm",
                "Benchmark",
                choices = c("FiveThirtyEight",
                            "Always Home",
                            "Odds"),
                selected = "Odds"
              )
            )
          ),
          column(
            width = 2,
            align = "center",
            
            div(
              style = "margin-top: 20px;",
              checkboxInput("prediction_model_lineups",
                            "Integrate Lineups",
                            value = FALSE)
            )
          )),  
          # tabs for information regarding the player
          tabsetPanel(
            # overview tab is for useful information and statistics
            # about the player
            tabPanel(
              "Moving Accuracy",tabPanel("Model",
                                            
                                         fluidRow(
                                           column(width = 10,
                                                  align = "center",
                                                  plotlyOutput("prediction_model_comparison")))
              )),
            tabPanel(
              "Past Matches",
              fluidRow(column(
                width = 12,
                align = "center",     
                reactableOutput("prediction_model_historical_results") %>%
                  withSpinner(color = "black")
              # ),column(
              #   width = 10,
              #   align = "center",     
              #   reactableOutput("prediction_model_historical_results_lineup") %>%
              #     withSpinner(color = "black")
              )
              )
            ),
            tabPanel(
              "Future Matches",
              fluidRow(column(
                width = 12,
                align = "center",     
                reactableOutput("prediction_model_future_results") %>%
                  withSpinner(color = "black")
              ))
            )
              
  ))
  
}