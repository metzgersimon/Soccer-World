# sub-ui for the league tab in the information menu item
tab_information_league_general_ui <- function(){
  # set the tabname to reference it from the main ui
  tabItem(tabName = "information-league-general",
          # create all the tabs for the league pages
          fluidRow(
            # to be able to select a range of seasons, we want two
            # selectizeInput to select a start season and an end season
            column(
              width = 3,
              align = "center",  
              selectizeInput(       # select league 
                "information_league_league_selection",
                label = "League",
                choices = c("Bundesliga",
                            "Bundesliga 2",
                            "Premier League",
                            "Ligue 1")
              )
            ),
            column(    # select season 
              width = 3,
              align = "center",
              selectizeInput(
                "information_league_season_selection",
                label = "Season",
                choices = seasons,
                selected = seasons[1]
              )
            )
          ),
          tabsetPanel(
            # create an overview page for the league to show information
            # such as the average age in the league, or the average market
            # value
            tabPanel("Overview",
                     fluidRow(
                       br(),
                       column(
                         width = 2,
                         align = "center",
                         div(style = "margin-top: 20px;",
                             htmlOutput("info_league_league_logo", height = 300))
                       ), column(
                         width = 5,
                         align = "center",
                         status = "primary",
                         div(style = "margin-top: 20px;",
                             htmlOutput("info_league_league_country", width = "40%"))
                       ),
                       column(
                         width = 5,
                         align = "center",
                         status = "primary",
                         div(style = "margin-top: 20px;",
                             htmlOutput("info_league_overview_table")%>%
                               withSpinner(color = "blue"))
                       )
                    
                     )),
         
            # create an season specific page for the league to show information
            # for the matchdays, statistics for a selected season, etc.
            tabPanel("Season specific", br(),
                     fluidRow(column(
                       width = 3,
                       align = "center",
                       selectInput(
                         "information_league_matchday_selection",
                         label = "Matchday",
                         choices = c("All"),
                         selected = ""
                       )
                     )),
                     tabsetPanel(
                       type = "pills",
                       tabPanel(
                         div("Matches", style = "color: LightSteelBlue"),
                             fluidRow(column(width = 12,
                                             align = "center",
                                             reactableOutput("information_league_matchday_fixtures")%>%
                                               withSpinner(color = "blue")))
                         
                       ),
                       tabPanel(
                         div("Table", style = "color: LightSteelBlue"),
                             fluidRow(column(
                               width = 12,
                               align = "center",
                               reactableOutput("information_league_matchday_table")%>%
                                 withSpinner(color = "blue"))
                         )
                         
                       )
                     )
                     ),
                    
            # this tab shows market value over time of teams in the selected leagues
            tabPanel("Market value over time",
                     fluidRow(column(
                       width = 12,
                       align = "center",
                       div(
                         style = "margin-top: 20px;",
                         plotlyOutput("market_value_over_time") %>%
                           withSpinner(color = "blue") # change load icon
                       )
                     )))
          )
  )
}
    