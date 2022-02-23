# sub-ui for the match tab in the information menu item
tab_information_league_match_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "information-league-match",
          # first row for the selection
          fluidRow(
            column(width = 2,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectizeInput("info_match_league", # league selection
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
                       selectizeInput("info_match_season", # season selection
                                   "Season",
                                   choices = seasons,
                                   selected = seasons[1])
                   )
            ),
            column(width = 3,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectizeInput("info_match_team1", # team 1 selection
                                   "Team 1",
                                   choices = c(""),
                                   selected = NULL
                       )
                   )
            ),
            column(width = 3,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectizeInput("info_match_team2", # team 2 selection
                                   "Team 2",
                                   choices = c(""),
                                   selected = NULL
                       )
                   )
            ),
            column(width = 2,
                   align = "center",
                   div(style = "margin-top: 20px;",
                       selectizeInput("info_match_season_half", # select season period 
                                   "Season half",
                                   choices = c("First half", "Second half"),
                                   selected = "First half"
                       )
                   )
            )
          ),
          tabsetPanel(
            tabPanel( # for the overview of one match
              "Overview",
              fluidRow(
                column(width = 12,
                       align = "center",
                       div(style = paste0("margin-top: 20px;",
                                          "background-color: #F4F4F4;"),
                           textOutput("info_match_match_date")
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       align = "center",
                       div(style = "margin-top: 20px;",
                           reactableOutput("info_match_match_events") %>%
                             withSpinner(color = "blue")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       align = "center",
                       div(style = "margin-top: 20px; background-color: #F4F4F4;",
                           textOutput("info_match_match_lineups_overview_home_text")
                       )
                ),
                column(width = 6,
                       align = "center",
                       div(style = "margin-top: 20px; background-color: #F4F4F4;",
                           textOutput("info_match_match_lineups_overview_away_text")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       align = "center",
                       div(style = paste0("margin-top: 20px;",
                                          "background-color: #567895;"),
                           textOutput("info_match_match_formation_home")
                       )
                ),
                column(width = 6,
                       align = "center",
                       div(style = paste0("margin-top: 20px;",
                                          "background-color: #567895;"),
                           textOutput("info_match_match_formation_away")
                       )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  align = "center",
                  div(
                    style = "margin-top: 20px;",
                    plotlyOutput("info_match_match_lineups_overview_home") %>%
                      withSpinner(color = "blue")
                  )
                ),
                column(
                  width = 6,
                  align = "center",
                  div(
                    style = "margin-top: 20px;",
                    plotlyOutput("info_match_match_lineups_overview_away") %>%
                      withSpinner(color = "blue")
                  )
                )
              )
            ),
            tabPanel("Match stats",
                     fluidRow(
                       column(width = 12,
                              align = "center",
                              div(style = "margin-top: 20px;",
                                  plotlyOutput("info_match_match_stats") %>%
                                    withSpinner(color = "blue")
                              )
                       )
                     )
            ),
            tabPanel("Aggregated Lineup Statistics",
                     fluidRow(
                       column(width = 6,
                              align = "center",
                              div(style = "margin-top: 20px; background-color: #F4F4F4;",
                                  textOutput("info_match_match_lineups_home_text")
                              )
                       ),
                       column(width = 6,
                              align = "center",
                              div(style = "margin-top: 20px; background-color: #F4F4F4;",
                                  textOutput("info_match_match_lineups_away_text")
                              )
                       )
                     ),
                     fluidRow(
                       column(width = 6,
                              align = "center",
                              div(style = "margin-top: 20px;",
                                  reactableOutput("info_match_match_lineups_home") %>%
                                    withSpinner(color = "blue")
                                  )
                              ),
                       column(width = 6,
                              align = "center",
                              div(style = "margin-top: 20px;",
                                  reactableOutput("info_match_match_lineups_away") %>%
                                    withSpinner(color = "blue")
                              )
                       )
                     )
            ),
            tabPanel(
              "Head to head",
              fluidRow(br(), # value box for the player fast facts
                       valueBoxOutput("Aggregate_wins_1", 
                                      width = 6),
                       valueBoxOutput("Aggregate_wins_2",
                                      width = 6)
              ),
              fluidRow( # head to head tab 
                column(width = 12,
                       align = "center",
                       div(style = "border: solid 2px #000000; margin-top: 20px;",
                           reactableOutput("info_league_match_h2h") %>%
                             withSpinner(color = "blue")
                       )
                )
              )
            )
          )
          
  )
  
}