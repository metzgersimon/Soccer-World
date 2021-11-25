# sub-ui for the league tab in the information menu item
tab_information_league_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "information-league",
          # create all the tabs for the league pages
          tabsetPanel(
            # create an overview page for the league to show information
            # such as the average age in the league, or the average market
            # value
                 tabPanel(
                   "Overview",
                   fluidRow(
                     column(width = 12,
                            align = "center",
                            div(style = "margin-top: 20px;",#"border: solid 2px #FFFFFF; margin-top: 20px;",
                                tableOutput("info_league_overview_table")
                            )
                     )
                   )
                 ),
                 # create an season specific page for the league to show information
                 # for the matchdays, statistics for a selected season, etc.
                 tabPanel(
                   "Season specific",
                   fluidRow(
                     # to be able to select a range of seasons, we want two
                     # selectInputs to select a start season and an end season
                     column(
                       width = 3, align = "center",
                       selectInput("information_league_season_selection_start", 
                                   label = "Season",
                                   choices = seasons,
                                   selected = seasons[1]
                       )
                     ),
                     # column(
                     #   width = 3, align = "center",
                     #   selectInput("information_league_season_selection_end", 
                     #               label = "Season",
                     #               choices = c(paste0(sort(c(1995:2021), 
                     #                                       decreasing = TRUE),
                     #                                  "/", sort(c(1996:2022), 
                     #                                            decreasing = TRUE))
                     #               )
                     #   )
                     # ),
                     # to make it more user friendly, we want the second season
                     # selection only displayed when the checkbox "Multiple Seasons"
                     # is checked, otherwise, the input wont be visible
                     # default it is hidden
                     # column(
                     #   width = 2, align = "center",
                     #   checkboxInput("information_league_multiple_seasons", 
                     #               label = "Multiple Seasons",
                     #               value = FALSE
                     #               
                     #   )
                     # ),
                     # selection for the club, it is a selectizeInput where
                     # the user can type a name and can also select multiple clubs
                     # if the user want to select all clubs, there is an option
                     # "All" which can also be selected
                     # column(
                     #   width = 3, align = "center",
                     #   selectizeInput("information_league_team_selection", 
                     #               label = "Club",
                     #               choices = c("All"),
                     #               multiple = TRUE
                     #               
                     #   )
                     # )
                     
                     column(
                       width = 3, align = "center",
                       selectInput("information_league_matchday_selection",
                                   label = "Matchday",
                                   choices = c("All"),
                                   selected = ""
                                      
                       )
                     )
                                      
                   ),
                   fluidRow(
                     reactableOutput("information_league_matchday_fixtures")
                   )
                 ),
                   # here, there are the plots for the given selections made
                   # by the user
                 #   fluidRow(
                 #     column(width = 12, align = "center",
                 #            div(style = "border: solid 2px #FFFFFF; margin-top: 20px;",
                 #                plotlyOutput("league_rank_over_season") %>%
                 #                  withSpinner(color = "blue")
                 #            )
                 #     )
                 #   ),
                 #   fluidRow(
                 #     column(width = 12, align = "center",
                 #            div(style = "border: solid 2px #FFFFFF; margin-top: 20px;",
                 #                plotlyOutput("all_seasons_over_time") %>%
                 #                  withSpinner(color = "blue")
                 #            )
                 #     )
                 #   )
                 # ),
                 # this tab shows information over time
                 tabPanel("Over time",
                          fluidRow(
                            column(width = 12, align = "center",
                                   div(style = "border: solid 2px #000000; margin-top: 20px;",
                                       plotlyOutput("market_value_over_time") %>%
                                         withSpinner(color = "blue")
                                   )
                            )
                          ),
                          fluidRow(
                            column(width = 12, align = "center",
                                   div(style = "border: solid 2px #FFFFFF; margin-top: 20px;",
                                       plotlyOutput("most_winners_since") %>%
                                         withSpinner(color = "blue")
                                   )
                            )
                          )
                          
                 )
          )
  )
}
          

########## currently not in use, but important later ############
#           
#           fluidRow(
#             column(
#               width = 3, align = "center",
#               selectInput("information_league_season_start", 
#                           label = "Season",
#                           choices = paste0(sort(c(1995:2021), decreasing = TRUE),
#                                            "/", sort(c(1996:2022), decreasing = TRUE))
#               )
#             ),
#             column(
#               width = 3, align = "center",
#               selectInput("information_league_season_end", 
#                           label = "Season",
#                           choices = paste0(sort(c(1995:2021), decreasing = TRUE),
#                                            "/", sort(c(1996:2022), decreasing = TRUE))
#               )
#             )
#           ),
#           fluidRow(
#             column(width = 12, align = "center",
#                    div(style = "border: solid 2px #FFFFFF; margin-top: 20px;",
#                        plotlyOutput("all_season_over_time") %>%
#                          withSpinner(color = "blue")
#                    )
#             )
#           )
#   )
#   
# }