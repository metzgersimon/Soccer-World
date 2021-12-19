# sub-ui for the match tab in the information menu item
tab_information_league_match_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "information-league-match",
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
                       selectInput("info_match_team1",
                                   "Team",
                                   choices = c("", "Borussia Dortmund"),
                                   selected = NULL
                       )
                   )
            ),
            column(width = 3,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectInput("info_match_team2",
                                   "Team",
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
          tabsetPanel(
            tabPanel(
              "Overview",
              fluidRow(
                column(width = 12,
                       align = "center",
                       div(style = "border: solid 2px #000000; margin-top: 20px;",
                           plotlyOutput("info_match_match_events") %>%
                             withSpinner(color = "blue")
                       )
                )
              ),
              # fluidRow(
              #   column(width = 6,
              #          align = "center",
              #          div(style = "border: solid 2px #000000; margin-top: 20px;",
              #              # plotlyOutput("info_match_match_lineups_overview") %>%
              #              d3Output("test") %>%
              #                withSpinner(color = "blue")
              #          )
              #   )
              # )
              fluidRow(
                column(width = 12,
                       align = "center",
                       div(style = "border: solid 2px #000000; margin-top: 20px;",
                           # plotlyOutput("info_match_match_lineups_overview") %>%
                           htmlOutput("test", width = "10%") %>%
                             withSpinner(color = "blue")
                       )
                )
              )
            ),
            tabPanel("Match stats",
                     fluidRow(
                       column(width = 12,
                              align = "center",
                              div(style = "border: solid 2px #000000; margin-top: 20px;",
                                  plotlyOutput("info_match_match_stats") %>%
                                    withSpinner(color = "blue")
                              )
                       )
                     )
            ),
            tabPanel("Lineups",
                     fluidRow(
                       column(width = 6,
                              align = "center",
                              div(style = "border: solid 2px #000000; margin-top: 20px;",
                                  reactableOutput("info_match_match_lineups_home") %>%
                                    withSpinner(color = "blue")
                                  )
                              ),
                       column(width = 6,
                              align = "center",
                              div(style = "border: solid 2px #000000; margin-top: 20px;",
                                  reactableOutput("info_match_match_lineups_away") %>%
                                    withSpinner(color = "blue")
                              )
                       )
                       
                     )
            ),
            tabPanel(
              "Head to head",
              fluidRow(
                column(width = 12,
                       align = "center",
                       div(style = "border: solid 2px #000000; margin-top: 20px;",
                           reactableOutput("info_league_match_h2h") %>%
                             withSpinner(color = "blue")
                       )
                )
              )
            ),
            tabPanel(
              "Bets",
              fluidRow(
                column(width = 12,
                       align = "center",
                       div(style = "border: solid 2px #000000; margin-top: 20px;",
                           reactableOutput("info_league_match_bets") %>%
                             withSpinner(color = "blue")
                       )
                )
              )
            )
          )
          
  )
  
}