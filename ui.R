# create the main-ui for the app with a dashboard-layout
ui <- dashboardPage(
  dashboardHeader(title = "Soccer-Prediction App"),
  dashboardSidebar(
    # create a sidebarmenu with menu items and subitems (subpages)
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Information", tabName = "information", icon = icon("searchengin"),
               startExpanded = TRUE,
               menuSubItem("League", tabName = "information-league"),
               menuSubItem("Team", tabName = "information-team"),
               menuSubItem("Player", tabName = "information-player")),
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
    
    # call the sub-uis for the several pages
    tabItems(
      tab_home_ui(),
      tab_information_league_ui(),
      tab_information_team_ui(),
      tab_information_player_ui()
    )#,
    # column(
    #   width = 3, align = "center",
    #   submitButton("Apply Changes",
    #                icon = icon("change")
    #   )
    # )
  )
)

