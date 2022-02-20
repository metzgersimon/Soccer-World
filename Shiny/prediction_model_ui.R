# sub-ui for the model tab in the prediction menu item
tab_prediction_model_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "prediction-model",
          tabPanel("Model",
          fluidRow(
            column(width = 2,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectInput("prediction_model_type",
                                   "Model",
                                   choices = c("XGBoost",
                                               "Linear Regression",
                                               "Best"),
                                   selected = "Best"))),
            column(width = 2,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       checkboxInput("prediction_model_lineups",
                                     "Integrate Lineups",
                                     value = FALSE)))),      
          fluidRow(
            column(width = 10,
                   align = "center",
                   plotlyOutput("prediction_model_comparison")))
          )
  )
  
}