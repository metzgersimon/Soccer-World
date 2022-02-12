# subui for the team tab in the information menu item
tab_information_team_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "information-team",
          fluidRow(
            column(width = 3, align = "center",
                   selectizeInput("info_team_league_selection",
                               label = "League",
                               choices = c("Bundesliga 1",
                                           "Bundesliga 2",
                                           "Premier League",
                                           "Ligue 1")
                   )
                   ),
            # create one column with an offset of 4 (to be in the center)
            # for the club selection
            column(width = 3,
                   align = "center",
                   # create a selectizeinput which makes it possible to 
                   # select a club from a list but also to type in the name
                   selectizeInput(inputId = "info_team_club_selection",
                                  label = "Club",
                                  multiple = TRUE,
                                  selected = NULL,
                                  choices = player_team_join %>%
                                    filter(season == as.numeric(
                                      str_split(seasons[1],
                                                pattern = "/")[[1]][1])) %>%
                                    select(team_name) %>%
                                    unlist() %>%
                                    unname() %>%
                                    unique(),
                                  options = list(
                                    placeholder = "",
                                    maxItems = 1
                                    )
                                  ), 
                   
                   ),
            # create one column with an offset of 5 (to be next to the club 
            # selection) for the selection of the season
            column(width = 3,
                   align = "center",
                   selectInput(
                     "info_team_season_selection", 
                     label = "Season", 
                     selected = NULL,
                     choices = seasons
                   )
            )
          ), 
          # creat different tabs to divide the data into appropriate pages
          tabsetPanel(
            # create the overview tab which contains 
            # general information about the team such as 
            # squad size and the average age of the squad
            tabPanel("Overview",
                     fluidRow(
                       # table output for the team information
                       column(width = 4, 
                              #offset = 3,
                              align = "center",
                              div(style = "margin-top: 20px;",#"border: solid 2px #FFFFFF; margin-top: 20px;",
                                  tableOutput("info_team_team_name")
                              )
                       ),
                       column(width = 4, 
                              #offset = 3,
                              align = "center",
                              div(style = "margin-top: 20px;",#"border: solid 2px #FFFFFF; margin-top: 20px;",
                                  tableOutput("info_team_venue_image")
                              )
                       ),
                       # ui output to display the logo of the club
                       column(width = 3,
                              offset = 1,
                              align = "right",
                              div(style = "margin-top: 20px;",
                                  uiOutput("info_team_team_logo",
                                           width = "2px")
                              )
                       )
                     )
            ),
            # season tab which should contain information about the current season
            # such as form, previous matches, future matches, etc.
            tabPanel("Match & Stats",
                     tabsetPanel(
                       type = "pills",
                       tabPanel("Matches",
                                fluidRow(column(
                                  width = 12,
                                  align = "center",
                                  div(
                                    style = paste0(
                                      "border: solid 1px #000000;",
                                      "margin-top: 20px;",
                                      "background-color: #004157;"
                                    ),
                                    p(
                                      "Upcoming Match",
                                      style = paste0(
                                        "background-color: #004157;",
                                        "color: white;",
                                        "margin-top: 10px;",
                                        "font-size: 18px;",
                                        "font-weight: bold;"
                                      )
                                    ),
                                    reactableOutput("info_team_next_match")
                                  )
                                )),
                                fluidRow(column(
                                  width = 12,
                                  align = "center",
                                  div(
                                    style = paste0(
                                      "border: solid 1px #000000;",
                                      "margin-top: 20px;",
                                      "background-color: #004157;"
                                    ),
                                    p(
                                      "Past Matches",
                                      style = paste0(
                                        "background-color: #004157;",
                                        "color: white;",
                                        "margin-top: 10px;",
                                        "font-size: 18px;",
                                        "font-weight: bold;"
                                      )
                                    ),
                                    reactableOutput("info_team_season")
                                  )
                                ))),
                       tabPanel("Statistics",
                                fluidRow(
                                  column(
                                    width = 12,
                                    align = "center",
                                    div(style = "margin-top: 20px;",
                                        reactableOutput("info_team_stats"))
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 12,
                                    div(#style = "margin-top: 20px;",
                                        verbatimTextOutput("info_team_summary"))
                                  )
                                )
                                )
                     )), 
            # squad panel to see the current squad of the club
            tabPanel("Squad",
                     #reactableOutput("info_team_squad")
                     fluidRow(
                       column(width = 12,
                              align = "center",
                              div(style = paste0("border: solid 1px #000000;",
                                                 "margin-top: 20px;",
                                                 "background-color: #004157;"),
                                  p("Attack", style = paste0("background-color: #004157;",
                                                             "color: white;",
                                                             "margin-top: 10px;",
                                                             "font-size: 18px;",
                                                             "font-weight: bold;")),
                                  reactableOutput("info_team_squad_attack")
                                  )
                              )),
                     fluidRow(
                       column(width = 12,
                              align = "center",
                              div(style = paste0("border: solid 1px #000000;",
                                                 "margin-top: 20px;",
                                                 "background-color: #004157;"
                                                 ),
                                  p("Midfield", style = paste0("background-color: #004157;",
                                                               "color: white;",
                                                               "margin-top: 10px;",
                                                               "font-size: 18px;",
                                                               "font-weight: bold;")),
                                  reactableOutput("info_team_squad_midfield")
                                  )
                              )
                       ),
                     fluidRow(
                       column(width = 12,
                              align = "center",
                              div(style = paste0("border: solid 1px #000000;",
                                                 "margin-top: 20px;",
                                                 "background-color: #004157;"),
                                  p("Defense", style = paste0("background-color: #004157;",
                                                              "color: white;",
                                                              "margin-top: 10px;",
                                                              "font-size: 18px;",
                                                              "font-weight: bold;")),
                                  reactableOutput("info_team_squad_defense")
                              )
                       )),
                    fluidRow(
                       column(width = 12,
                              align = "center",
                              div(style = paste0("border: solid 1px #000000;",
                                                 "margin-top: 20px;",
                                                 "background-color: #004157;"),
                                  p("Goal", style = paste0("background-color: #004157;",
                                                           "color: white;",
                                                           "margin-top: 10px;",
                                                           "font-size: 18px;",
                                                           "font-weight: bold;")),
                                  reactableOutput("info_team_squad_goalkeepers")
                                  )
                              )
                       )
                     ),
            tabPanel(
              "Over Time",
              tabsetPanel(
                type = "pills",
                tabPanel(
                  "Market value",
                  fluidRow(
                    column(width = 12,
                           align = "center",
                           div(style = paste0("border: solid 1px #000000;",
                                              "margin-top: 20px;"),
                               plotlyOutput("info_team_market_value_over_time") %>%
                                 withSpinner(color = "blue")
                               )
                           )
                    )
                  ),
                tabPanel(
                    "FIFA Rating",
                    fluidRow(
                      column(width = 12,
                             align = "center",
                             div(style = paste0("border: solid 1px #000000;",
                                                "margin-top: 20px;"),
                                 plotlyOutput("info_team_fifa_rating_over_time") %>%
                                   withSpinner(color = "blue")
                             )
                      )
                    )
                  ),
                tabPanel(
                  "Transfers",
                  fluidRow(
                    column(width = 12,
                           align = "center",
                           div(style = paste0("border: solid 1px #000000;",
                                              "margin-top: 20px;"),
                               reactableOutput("info_team_transfers_over_time") %>%
                                 withSpinner(color = "blue")
                               )
                           )
                    )
                  ) 
                )
              )
            )
  )
}