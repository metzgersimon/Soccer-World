# create the main-ui for the app with a dashboard-layout
ui <- dashboardPage(
  dashboardHeader(title = "Soccer-World App"),
  dashboardSidebar(# create a sidebarmenu with menu items and subitems (subpages)
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Wiki", tabName = "Wiki", icon = icon("address-book")),
      menuItem(
        "Information",
        tabName = "information",
        icon = icon("searchengin"),
        startExpanded = TRUE,
        menuItem(
          "League",
          tabName = "information-league",
          icon = icon("angle-double-right"),
          menuSubItem("General", tabName = "information-league-general"),
          menuSubItem("Match", tabName = "information-league-match")
        ),
        menuItem(
          "Team",
          tabName = "information-team",
          icon = icon("angle-double-right")
        ),
        menuItem(
          "Player",
          tabName = "information-player",
          icon = icon("angle-double-right")
        )
      ),
      menuItem(
        "Prediction",
        tabName = "prediction",
        icon = icon("battle-net"),
        startExpanded = TRUE,
        menuSubItem("Model", tabName = "prediction-model")
      ),
      # menuSubItem("Investment", tabName = "prediction-investment")),
      menuItem("About", tabName = "about", icon = icon("address-book"))
    )),
  dashboardBody(
    # tags$head(# change the front for the tab
    #   tags$style("*{font-family: Century Gothic;}")),
    # set the theme for the ui
    shinyDashboardThemes(theme = "grey_light"),
    # use the shinyjs package to improve the user experience
    # of the app
    useShinyjs(),
    
    # call the sub-uis for the several pages
    tabItems(
      tab_home_ui(),
      tab_wiki_ui(),
      tab_information_league_general_ui(),
      tab_information_league_match_ui(),
      tab_information_team_ui(),
      tab_information_player_ui(),
      tab_prediction_model_ui(),
      tab_about_ui()
    )
  )
)
