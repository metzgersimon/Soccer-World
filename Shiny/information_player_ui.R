# sub-ui for the player tab in the information menu item
tab_information_player_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "information-player",
          # create a fluid row to arrange all selectInputs
          # for the league, club and player in one row
          fluidRow(
            column(
              width = 3,
              align = "center",
              selectizeInput(
                "information_player_league_selection",
                label = "League",
                choices = c(
                  "Bundesliga",
                  "Bundesliga 2",
                  "Premier League",
                  "Ligue 1"
                )
              )
            ),
            
            column(
              width = 3,
              align = "center",
              selectizeInput(
                "information_player_team_selection",
                label = "Club",
                choices = unique(all_leagues_tm_squads$club),
                selected = unique(all_leagues_tm_squads$club)[1]
              )
              
            ),            column(
              width = 3,
              align = "center",
              selectizeInput(
                "information_player_season_selection",
                label = "Season",
                choices = seasons,
                selected = seasons[1]
              )
              
            ),
            column(
              width = 3,
              align = "center",
              selectizeInput(
                "information_player_player_selection",
                label = "Player",
                choices = c("")
              )
              
            )
            
          ),
          # tabs for information regarding the player
          tabsetPanel(# overview tab is for useful information and statistics
            # about the player
            tabPanel("Overview",
                     fluidRow(
                         valueBoxOutput(
                           "age", 
                           width = 3),
                         valueBoxOutput(
                           "height", 
                           width = 3),
                         valueBoxOutput(
                           "joining_date", 
                           width = 3),
                         valueBoxOutput(
                           "contract_date", 
                           width = 3)),
                       # ui output to display the logo of the club and player image
                     fluidRow(box(width = 4,
                                  title = "Player Overview", 
                                  solidHeader = TRUE, 
                                  status = "primary",     
                                  div(style = "margin-top: 20px;",
                                      htmlOutput("info_player_overview")
                                  )
                     ),box(width = 4,
                           title = "Player profile", 
                           solidHeader = TRUE, 
                           status = "primary",     
                           div(style = "margin-top: 20px;",
                               htmlOutput("info_player_player_img")
                           )
                     ),
                     box(width = 4,
                         title = "Player club logo", 
                         solidHeader = TRUE, 
                         status = "primary",     
                         div(style = "margin-top: 20px;",
                             htmlOutput("info_player_club_img")
                         )
                     )
                     # column(width =4 ,
                     #          align = "left",
                     #          div(style = "margin-top: 20px;",
                     #              uiOutput("info_player_player_img",
                     #                       width = "2px")
                     #          )
                     #   ),
                       # column(width = 4,
                       #        align = "left",
                       #        div(style = "margin-top: 20px;",
                       #            uiOutput("info_player_club_img",
                       #                     width = "2px")
                       #        )
                       # )
                     )
            ),
            tabPanel("Statistics",  
                     tabsetPanel(type = "pills",
                                 tabPanel("General",
                                          fluidRow(
                                            box(title = "Game infos", 
                                                status = "primary", width = 12,
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                reactableOutput("info_player_stats_general_games") %>%
                                                withSpinner(color = "black")
                
                                              )),
                                          fluidRow(
                                            box(title = "Goals infos", 
                                                status = "primary", width = 8,
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                reactableOutput("info_player_stats_general_goals") %>%
                                                  withSpinner(color = "black")    
                                            ),
                                            box(title = "Shots infos", 
                                                status = "primary", width = 4,
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                reactableOutput("info_player_stats_general_shots") %>%
                                                  withSpinner(color = "black")    
                                            )
                                            ),
                                          fluidRow(
                                            box(title = "Passes infos", 
                                                status = "primary", width = 8,
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                reactableOutput("info_player_stats_general_passes") %>%
                                                  withSpinner(color = "black")    
                                            ),
                                            box(title = "Duel infos", 
                                                status = "primary", width = 4,
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                reactableOutput("info_player_stats_general_duel") %>%
                                                  withSpinner(color = "black")    
                                            )
                                          )
                                          ),
                                 tabPanel("Details",
                                          fluidRow(
                                            box(title = "Dribbles and tackles", 
                                                status = "primary", width = 12,
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                reactableOutput("info_player_stats_general_dribbles") %>%
                                                  withSpinner(color = "black")    
                                            ),
                                            box(title = "Penalty and cards", 
                                                status = "primary", width = 12,
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                reactableOutput("info_player_stats_general_cards") %>%
                                                  withSpinner(color = "black")    
                                            )
                                          ),
                                          fluidRow(box(title = "Offsides and fouls", 
                                                       status = "primary", width = 12,
                                                       solidHeader = TRUE,
                                                       collapsible = TRUE,
                                                       reactableOutput("info_player_stats_general_offsides") %>%
                                                         withSpinner(color = "black")    
                                          ))),
                                 tabPanel("FIFA Player rating",
                                          fluidRow(column(
                                            width = 12,
                                            # offset = 3,
                                            align = "center",
                                            div(
                                              style = "margin-top: 20px;",
                                              #"border: solid 2px #FFFFFF; margin-top: 20px;",
                                              plotlyOutput("info_player_stats_rating") %>%
                                                withSpinner(color = "white")
                                            )
                                          )))
                     )),  
            tabPanel(
              "Transfers",
              fluidRow(
                column(width = 10, 
                       offset = 1,
                       align = "center",
                       div(style = "margin-top: 20px;",#"border: solid 2px #FFFFFF; margin-top: 20px;",
                           reactableOutput("info_player_transfers")
                       )
                )
              )
            )
            
          )
          
  )
  
}



