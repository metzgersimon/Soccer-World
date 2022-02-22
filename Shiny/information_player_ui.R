# sub - ui for the player tab in the information menu item
tab_information_player_ui <- function() {
  # set the tabname to reference it from the main ui
  tabItem(tabName = "information-player",
          # create a fluid row to arrange all selectizeInput
          # for the league, club and player in one row
          fluidRow(
            column(
              width = 3,
              align = "center",
              selectizeInput(
                "information_player_league_selection",
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
                "information_player_team_selection",
                label = "Club",
                choices = unique(all_leagues_squads_tm$club), # the choice would be updated after the user select league
                selected = unique(all_leagues_squads_tm$club)[1]
              )
              
            ),
            column(
              width = 3,
              align = "center",
              selectizeInput(
                "information_player_season_selection", # the choice would be updated after the user select league and club
                label = "Season",
                choices = seasons,
                selected = seasons[1]
              )
            ),
            column(
              width = 3,
              align = "center",
              selectizeInput(
                "information_player_player_selection", # the choice would be updated after the user select league, club and season
                label = "Player",
                choices = c("")
              )
              
            )
            
          ), 
          # tabs for information regarding the player
          tabsetPanel(
            # overview tab is for useful information and statistics
            # about the player
            tabPanel(
              "Overview",
              fluidRow(
                br(), # value box for the player fast facts
                valueBoxOutput("age", 
                               width = 3),
                valueBoxOutput("height",
                               width = 3),
                valueBoxOutput("joining_date",
                               width = 3),
                valueBoxOutput("contract_date",
                               width = 3)
              ),
              # ui output to display the overview, logo of the club and player image
              fluidRow(
                br(),
                box(
                  width = 4,
                  title = "Player Overview",
                  solidHeader = TRUE,
                  status = "primary",
                  div(style = "margin-top: 20px;",
                      htmlOutput("info_player_overview"))
                ),
                box(
                  width = 4,
                  title = "Player Profile",
                  solidHeader = TRUE,
                  status = "primary",
                  div(style = "margin-top: 20px;",
                      htmlOutput("info_player_player_img"))
                ),
                box(
                  width = 4,
                  title = "Player Club Logo",
                  solidHeader = TRUE,
                  status = "primary",
                  div(style = "margin-top: 20px;",
                      htmlOutput("info_player_club_img"))
                )
              )
            ), 
            tabPanel("Statistics",
                     # the tab for the player's detailed stats
                     tabsetPanel(
                       type = "pills",
                       tabPanel(
                         div("General", style = "color: LightSkyBlue"),
                         # with radarplot to see the player summary stats  
                         fluidRow(
                           column(
                             width = 6,
                             height = 200,
                             align = "left",
                             div(
                               style = "margin-top: 20px;",
                               plotOutput(
                                 'info_player_stats_radarplot',
                                 width = "100%",
                                 height = 300
                               ) %>%
                                 withSpinner(color = "black") # loading icon
                             )
                           ),
                           column(
                             width = 6,
                             align = "right",
                             height = 200,
                             br(), # with 8 value boxes to present the numbers
                             valueBoxOutput("game_minute",
                                            width = 3),
                             valueBoxOutput("passes_accuracy",
                                            width = 3),
                             valueBoxOutput("game_substitute",
                                            width = 3),
                             valueBoxOutput("goals_assist",
                                            width = 3)
                           )
                           ,
                           column(
                             width = 6,
                             align = "right",
                             height = 200,
                             br(),
                             valueBoxOutput("card_yellow",
                                            width = 3),
                             valueBoxOutput("card_red",
                                            width = 3),
                             valueBoxOutput("penalty_scored",
                                            width = 3),
                             valueBoxOutput("penalty_saved",
                                            width = 3)
                           )
                         )
                         ,
                         fluidRow(
                           box( # with the boxes to present the stats in more detailed way (for each fixture date in the selected season)
                             title = "Game Infos",
                             status = "primary",
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             reactableOutput("info_player_stats_general_games") %>%
                               withSpinner(color = "black")
                             
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Goals Infos",
                             status = "primary",
                             width = 8,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             reactableOutput("info_player_stats_general_goals") %>%
                               withSpinner(color = "black")
                           ),
                           box(
                             title = "Shots Infos",
                             status = "primary",
                             width = 4,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             reactableOutput("info_player_stats_general_shots") %>%
                               withSpinner(color = "black")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Passes Infos",
                             status = "primary",
                             width = 8,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             reactableOutput("info_player_stats_general_passes") %>%
                               withSpinner(color = "black")
                           ),
                           box(
                             title = "Duel Infos",
                             status = "primary",
                             width = 4,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             reactableOutput("info_player_stats_general_duel") %>%
                               withSpinner(color = "black")
                           )
                         )
                       ),
                       tabPanel( 
                         div("Details", style = "color: LightSkyBlue;"),
                         # more detailed infos for the player stats
                         fluidRow(
                           br(),
                           box(
                             title = "Dribbles and Tackles",
                             status = "primary",
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             reactableOutput("info_player_stats_general_dribbles") %>%
                               withSpinner(color = "black")
                           ),
                           box(
                             title = "Penalty and Cards",
                             status = "primary",
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             reactableOutput("info_player_stats_general_cards") %>%
                               withSpinner(color = "black")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Offsides and Fouls",
                             status = "primary",
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             reactableOutput("info_player_stats_general_offsides") %>%
                               withSpinner(color = "black")
                           )
                         )
                       ),
                       tabPanel( # tab for the fifa player rating
                         div("FIFA Player Rating", style = "color: LightSkyBlue;"),
                         fluidRow(br(), column(
                           width = 12,
                           align = "center",
                           div(
                             style = "margin-top: 20px;",
                             #"border: solid 2px #FFFFFF; margin-top: 20px;",
                             plotlyOutput("info_player_stats_rating") %>%
                               withSpinner(color = "white")
                           )
                         ))
                       )
                     )),  
            tabPanel("Transfers", # tab for the transfer infos of player
                     fluidRow(column(
                       width = 10,
                       offset = 1,
                       align = "center",
                       div(style = "margin-top: 20px;", #"border: solid 2px #FFFFFF; margin-top: 20px;",
                           reactableOutput("info_player_transfers"))
                     )))
            
          )
          
  )
  
}



