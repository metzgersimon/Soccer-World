# subui for the team tab in the information menu item
tab_information_team_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "information-team",
          fluidRow(
            # select input for league choices
            column(width = 3, align = "center",
                   selectizeInput("info_team_league_selection",
                               label = "League",
                               choices = c("Bundesliga",
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
                                  choices = all_infos_club %>%
                                    filter(season == as.numeric(
                                      str_split(seasons[1],
                                                pattern = "/")[[1]][1])) %>%
                                    select(club) %>%
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
                   selectizeInput(
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
            tabPanel(
              "Overview",
              fluidRow(
                br(),
                # insert four value boxes for the overview infos of team
                valueBoxOutput("info_team_team_age", width = 3),
                valueBoxOutput("info_team_team_height", width = 3),
                valueBoxOutput("info_team_team_size", width = 3),
                valueBoxOutput("info_team_team_venue", width = 3)
              ),
              fluidRow(
                # insert four value boxes for the overview infos of team
                valueBoxOutput("info_team_team_players", width = 3),
                valueBoxOutput("info_team_team_rightfoot", width = 3),
                valueBoxOutput("info_team_team_leftfoot", width = 3),
                valueBoxOutput("info_team_team_german", width = 3)
              ),
              fluidRow(br(),
                # table output for the team logo
                column(
                  width = 4,
                  align = "center",
                  div(style = "margin-top: 20px;", #"border: solid 2px #FFFFFF; margin-top: 20px;",
                      tableOutput("info_team_team_logo"))
                ),
                # display image for the venue of team
                column(
                  width = 8,
                  align = "left",
                  div(style = "margin-top: 20px;", #"border: solid 2px #FFFFFF; margin-top: 20px;",
                      tableOutput("info_team_venue_image"))
                )
            )),
            # Match & Stats tab which should contain information about the current season
            # such as form, previous matches, future matches, etc.
            tabPanel("Match & Stats",
                     tabsetPanel(type = "pills",
                       tabPanel(div("Matches", style = "color: LightSteelBlue"),
                                fluidRow(column(
                                  width = 12,
                                  align = "center",
                                  div(
                                    style = paste0(
                                      "margin-top: 20px;",
                                      "background-color: #E2DFD8;"
                                    ),
                                    p(
                                      "Upcoming Match", # present the upcoming match
                                      style = paste0(
                                        "background-color: #E2DFD8;",
                                        "margin-top: 10px;",
                                        "font-size: 18px;",
                                        "font-weight: bold;"
                                      )
                                    ),
                                    reactableOutput("info_team_next_match")%>%
                                      withSpinner(color = "blue")
                                  )
                                )),
                                fluidRow(column(
                                  width = 12,
                                  align = "center",
                                  div(
                                    style = paste0( 
                                      "margin-top: 20px;",
                                      "background-color: #E2DFD8;"
                                    ),
                                    p(
                                      "Past Matches", # display the past matches
                                      style = paste0(
                                        "background-color: #E2DFD8;",
                                        "margin-top: 10px;",
                                        "font-size: 18px;",
                                        "font-weight: bold;"
                                      )
                                    ),
                                    reactableOutput("info_team_season")%>%
                                      withSpinner(color = "blue")
                                  )
                                ))),
                       tabPanel(div("Statistics", style = "color: LightSteelBlue"),br(),
                                fluidRow(
                                  # Frontpage - boxes - start 
                                  # insert four value boxes for the infos of team
                                  valueBoxOutput(
                                   "total_played", # how many total plays of this team
                                    width = 3),
                                  valueBoxOutput(
                                    "total_wins", # how many total wins of this team
                                    width = 3),
                                  valueBoxOutput(
                                    "total_draws",# how many total draws of this team
                                    width = 3),
                                  valueBoxOutput(
                                   "total_loses", # how many total loses of this team
                                    width = 3)
                                ),
                                fluidRow(
                                  valueBoxOutput(
                                     "total_for_goals", # how many total fo goals of this team
                                    width = 3),
                                  valueBoxOutput(
                                    "total_against_goals", # how many total against goal of this team
                                    width = 3),
                                  valueBoxOutput(
                                    "total_failed_score", # how many total failed to score of this team
                                    width = 3),
                                  valueBoxOutput(
                                    "total_penalty", # how many total penalty of this team
                                    width = 3)
                                  # Frontpage - boxes - end
                                ),br(),
                                fluidRow(  # box for the biggest streak infos for the team
                                  box(title = "Biggest streak infos", 
                                      status = "primary", width = 4,
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      reactableOutput("info_team_stats") %>%
                                        withSpinner(color = "blue")    
                                  ), # box for the biggest difference as home team
                                  box(title = "As home team - biggest difference", 
                                      status = "primary", width = 4,
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      reactableOutput("info_team_home") %>%
                                        withSpinner(color = "blue")    
                                  ),  # box for the biggest difference as away team
                                  box(title = "As away team - biggest difference", 
                                      status = "primary", width = 4,
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      reactableOutput("info_team_away") %>%
                                        withSpinner(color = "blue")    
                                  )
                                ),br(),
                                fluidRow(
                                  # plots - start
                                  tabBox( # time series infos as home team
                                    width = 12,
                                    tabPanel(
                                      status = "primary",
                                      title = "stats as home team",
                                      withSpinner(plotlyOutput("ts_home_stats", height = "250px"),color = "blue")
                                    ),
                                    tabPanel( # time series infos as away team
                                      status = "success",
                                      title = "stats as away team",
                                      withSpinner(plotlyOutput("ts_away_stats", height = "250px"),color = "blue")
                                    )
                                  )
                                  # plots - end 
                                )
                                )
                     )), 
            # squad panel to see the current squad of the club
            tabPanel("Squad",
                     fluidRow( # attack table
                       column(width = 12,
                              align = "center",
                              div(style = "margin-top: 20px;",
                                  p("Attack", style = paste0("background-color: #F4F4F4;",
                                                             "margin-top: 10px;",
                                                             "font-size: 18px;",
                                                             "font-weight: bold;")),
                                  reactableOutput("info_team_squad_attack")%>%
                                    withSpinner(color = "blue")
                                  )
                              )),
                     fluidRow(# Midfield table
                       column(width = 12,
                              align = "center",
                              div(style =  "margin-top: 20px;", 
                                  p("Midfield", style = paste0("background-color: #F4F4F4;",
                                                               "margin-top: 10px;",
                                                               "font-size: 18px;",
                                                               "font-weight: bold;")),
                                  reactableOutput("info_team_squad_midfield")%>%
                                    withSpinner(color = "blue")
                                  )
                              )
                       ),
                     fluidRow( # Defense table
                       column(width = 12,
                              align = "center",
                              div(style = "margin-top: 20px;",
                                  p("Defense", style = paste0("background-color: #F4F4F4;",
                                                              "margin-top: 10px;",
                                                              "font-size: 18px;",
                                                              "font-weight: bold;")),
                                  reactableOutput("info_team_squad_defense")%>%
                                    withSpinner(color = "blue")
                              )
                       )),
                    fluidRow( # Goal table
                       column(width = 12,
                              align = "center",
                              div(style = paste0("margin-top: 20px;",
                                                 "background-color: #F4F4F4;"),
                                  p("Goal", style = paste0("background-color: #F4F4F4;",
                                                           "margin-top: 10px;",
                                                           "font-size: 18px;",
                                                           "font-weight: bold;")),
                                  reactableOutput("info_team_squad_goalkeepers")%>%
                                    withSpinner(color = "blue")
                                  )
                              )
                       )
                     ),
            tabPanel(
              "Over Time", # time series data for the team
              tabsetPanel(
                type = "pills",
                tabPanel(
                  div("Market value", style = "color: LightSteelBlue"), # market value over time 
                  fluidRow(
                    column(width = 12,
                           align = "center",
                           div(style = "margin-top: 20px;",
                               plotlyOutput("info_team_market_value_over_time") %>%
                                 withSpinner(color = "blue")
                               )
                           )
                    )
                  ),
                tabPanel(
                  div("FIFA Rating", style = "color: LightSteelBlue"), # fifa rating over time
                    fluidRow(
                      column(width = 12,
                             align = "center",
                             div(style = "margin-top: 20px;",
                                 plotlyOutput("info_team_fifa_rating_over_time") %>%
                                   withSpinner(color = "blue")
                             )
                      )
                    )
                  ),
                tabPanel(
                  div("Transfers", style = "color: LightSteelBlue"), # transfer over time
                  fluidRow(
                    column(width = 12,
                           align = "center",
                           div(style = "margin-top: 20px;",
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