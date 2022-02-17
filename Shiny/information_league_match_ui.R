# sub-ui for the match tab in the information menu item
tab_information_league_match_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "information-league-match",
          fluidRow(
            column(width = 2,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectInput("info_match_league",
                                   "League",
                                   choices = c("Bundesliga",
                                               "Bundesliga 2",
                                               "Premier League",
                                               "Ligue 1")
                       ),
                                   selected = "Bundesliga")
                   ),
            column(width = 2,
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
                                   "Team 1",
                                   choices = c(""),
                                   selected = NULL
                       )
                   )
            ),
            column(width = 3,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectInput("info_match_team2",
                                   "Team 2",
                                   choices = c(""),
                                   selected = NULL
                       )
                   )
            ),
            column(width = 2,
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
                       div(style = paste0("border: solid 1px #000000;",
                                          "margin-top: 20px;",
                                          "background-color: #004157;"),
                           textOutput("info_match_match_date")
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       align = "center",
                       div(style = "border: solid 2px #000000; margin-top: 20px;",
                           reactableOutput("info_match_match_events") %>%
                             withSpinner(color = "blue")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       align = "center",
                       div(style = "border: solid 2px #000000; margin-top: 20px; background-color: #004157;",
                           textOutput("info_match_match_lineups_overview_home_text")
                       )
                ),
                column(width = 6,
                       align = "center",
                       div(style = "border: solid 2px #000000; margin-top: 20px; background-color: #004157;",
                           textOutput("info_match_match_lineups_overview_away_text")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       align = "center",
                       div(style = paste0("border: solid 1px #000000;",
                                          "margin-top: 20px;",
                                          "background-color: #004157;"),
                           textOutput("info_match_match_formation_home")
                       )
                ),
                column(width = 6,
                       align = "center",
                       div(style = paste0("border: solid 1px #000000;",
                                          "margin-top: 20px;",
                                          "background-color: #004157;"),
                           textOutput("info_match_match_formation_away")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       align = "center",
                       div(style = paste0("border: solid 1px #000000;",
                                          "margin-top: 20px;"),
                           plotlyOutput("info_match_match_lineups_overview_home")
                       )
                ),
                column(width = 6,
                       align = "center",
                       div(style = paste0("border: solid 1px #000000;",
                                          "margin-top: 20px;"),
                           plotlyOutput("info_match_match_lineups_overview_away")
                       )
                )
              )
            ),
            tabPanel("Match stats",
                     fluidRow(
                       column(width = 12,
                              align = "center",
                              div(style = "border: solid 1px #000000; margin-top: 20px;",
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
                              div(style = "border: solid 2px #000000; margin-top: 20px; background-color: #004157;",
                                  textOutput("info_match_match_lineups_home_text")
                              )
                       ),
                       column(width = 6,
                              align = "center",
                              div(style = "border: solid 2px #000000; margin-top: 20px; background-color: #004157;",
                                  textOutput("info_match_match_lineups_away_text")
                              )
                       )
                     ),
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
                     )#,
                     # fluidRow(
                     #   column(width = 6,
                     #          align = "center",
                     #          div(style = "border: solid 2px #000000; margin-top: 20px;",
                     #              reactableOutput("info_match_match_substitutes_home") %>%
                     #                withSpinner(color = "blue")
                     #          )
                     #   ),
                     #   column(width = 6,
                     #          align = "center",
                     #          div(style = "border: solid 2px #000000; margin-top: 20px;",
                     #              reactableOutput("info_match_match_substitutes_away") %>%
                     #                withSpinner(color = "blue")
                     #          )
                     #   )
                     # )
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
            )#,
            # tabPanel(
            #   "Bets",
            #   fluidRow(
            #     column(width = 12,
            #            align = "center",
            #            div(style = "border: solid 2px #000000; margin-top: 20px;",
            #                reactableOutput("info_league_match_bets") %>%
            #                  withSpinner(color = "blue")
            #            )
            #     )
            #   )
            # )
          )
          
  )
  
}