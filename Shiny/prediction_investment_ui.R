# sub-ui for the investment tab in the prediction menu item
tab_prediction_investment_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "prediction-investment",
          fluidRow(
            # create one column with an offset of 4 (to be in the center)
            # for the club selection
            column(width = 3, 
                   offset = 4,
                   align = "center",
                   # create a selectizeinput which makes it possible to 
                   # select a club from a list but also to type in the name
                   numericInput(inputId = "prediction_investment_value",
                                label = "Investment value",
                                value = "",
                                min = 0
                                
                   )
                   
            )
          ),
          fluidRow(
            column(width = 12,
                   align = "center",
                   div(style = paste0("border: solid 1px #000000;",
                                      "margin-top: 20px;"),
                       plotlyOutput("prediction_investment_plot") %>%
                         withSpinner(color = "blue")
                   )
            )
          )
  )
  
  
}