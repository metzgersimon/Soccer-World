# sub-ui for the player tab in the information menu item
tab_information_player_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "information-player",
          # create a fluid row to arrange all selectInputs
          # for the league, club and player in one row
          fluidRow(
            column(width = 3, align = "center",
                   selectInput("information_player_league_selection",
                               label = "League",
                               choices = c("Bundesliga",
                                           "2. Bundesliga",
                                           "3. Liga")
                   )
                   
            ),
            column(width = 3, align = "center",
                   selectInput("information_player_season_selection",
                               label = "Season",
                               choices = seasons,
                               selected = seasons[1]
                   )
                   
            ),
            column(width = 3, align = "center",
                   selectInput("information_player_team_selection",
                               label = "Club",
                               choices = unique(season_players_joined$club),
                               selected = unique(season_players_joined$club)[1]
                   )
                   
            ),
            column(width = 3, align = "center",
                   selectInput("information_player_player_selection",
                               label = "Player",
                               choices = c("")
                   )
                   
            )
            
          ),
          # tabs for information regarding the player
          tabsetPanel(
            # overview tab is for useful information and statistics
            # about the player
            tabPanel("Overview",
                     fluidRow(
                       column(width = 5, 
                              offset = 3,
                              align = "center",
                              div(style = "margin-top: 20px;",#"border: solid 2px #FFFFFF; margin-top: 20px;",
                                  tableOutput("info_player_player_name")
                              )
                       ),
                       # ui output to display the logo of the club
                       column(width = 1,
                              align = "left",
                              div(style = "margin-top: 20px;",
                                  uiOutput("info_player_player_img",
                                           width = "2px")
                              )
                       )
                     )
            ),
            tabPanel(
              "Stats",
              fluidRow(
                column(width = 5, 
                       offset = 3,
                       align = "center",
                       div(style = "margin-top: 20px;",#"border: solid 2px #FFFFFF; margin-top: 20px;",
                           reactableOutput("info_player_stats")
                       )
                )
              )
            ),
            tabPanel(
              "Transfers",
              fluidRow(
                column(width = 5, 
                       offset = 3,
                       align = "center",
                       div(style = "margin-top: 20px;",#"border: solid 2px #FFFFFF; margin-top: 20px;",
                           tableOutput("info_player_transfers")
                       )
                )
              )
            )
            
          )
          
  )
  
}