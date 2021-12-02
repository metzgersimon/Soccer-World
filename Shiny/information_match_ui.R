# sub-ui for the match tab in the information menu item
tab_information_match_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "information-match",
          fluidRow(
            column(width = 3,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectInput("info_match_season",
                                   "Season",
                                   choices = seasons,
                                   selected = seasons[1])
                   )
            ),
            column(width = 3,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectInput("info_match_home_team",
                                   "Home Team",
                                   choices = c("", "Borussia Dortmund"),
                                   selected = NULL
                       )
                   )
            ),
            column(width = 3,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectInput("info_match_away_team",
                                   "Away Team",
                                   choices = c("", "FC Schalke 04"),
                                   selected = NULL
                       )
                   )
            ),
            column(width = 3,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectInput("info_match_season_half",
                                   "Season half",
                                   choices = c("First half", "Second half"),
                                   selected = "First half"
                       )
                   )
            )
          ),
          fluidRow(
            column(width = 12,
                   align = "center",
                   div(style = "border: solid 2px #000000; margin-top: 20px;",
                       plotlyOutput("info_match_match_stats") %>%
                         withSpinner(color = "blue")
                   )
            )
          )
  )
  
}