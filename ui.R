# create the main-ui for the app with a dashboard-layout
ui <- dashboardPage(
  dashboardHeader(title = "Soccer-Prediction App"),
  dashboardSidebar(
    # create a sidebarmenu with menu items and subitems (subpages)
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Information", tabName = "information", icon = icon("searchengin"),
               startExpanded = TRUE,
               menuItem("League", tabName = "information-league",
                        icon = icon("angle-double-right"),
                        menuSubItem("Match", tabName = "information-league-match")),
               menuItem("Team", tabName = "information-team",
                        icon = icon("angle-double-right")),
               menuItem("Player", tabName = "information-player",
                        icon = icon("angle-double-right")),
               menuItem("Match", tabName = "information-match",
                        icon = icon("angle-double-right"))),
      menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Analysis", tabName = "analysis", icon = icon("accusoft")),
      menuItem("Prediction", tabName = "prediction", icon = icon("battle-net"))
    )
  ),
  dashboardBody(
    # set the theme for the ui
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    # use the shinyjs package to improve the user experience
    # of the app
    useShinyjs(),
    # use the shinyalert package to improve the user experience
    # of the app by making great alert messages (for errors etc.)
    useShinyalert(),
    
    # call the sub-uis for the several pages
    tabItems(
      tab_home_ui(),
      tab_information_league_ui(),
      tab_information_team_ui(),
      tab_information_player_ui(),
      tab_information_match_ui()
    )#,
    # column(
    #   width = 3, align = "center",
    #   submitButton("Apply Changes",
    #                icon = icon("change")
    #   )
    # )
  )
)

