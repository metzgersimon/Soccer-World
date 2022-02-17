# sub-ui for the home tab

tab_home_ui <- function() {
  tabItem(
    tabName = "home",
    # create a title
   titlePanel("Welcome to the Soccer World!"),
    # sidebarpanel design for select input
    sidebarLayout(sidebarPanel(
      pickerInput(
        "leagues",
        "Select a league:",
        choices = c("Bundesliga",
                    "Bundesliga 2",
                    "Premier League",
                    "Ligue 1"),
        selected = "Bundesliga"
      ),
      tableOutput("home_league_flag")
    ),
    # create main panel of leaflet map
    mainPanel(tabsetPanel(
      tabPanel(
        "Map",
        tags$style('.leaflet {height: 1000px;}'),
        leafletOutput("map", width = "100%", height = "400px")
       
     )
   )))
  )
  
}