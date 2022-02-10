# sub-ui for the home tab

tab_home_ui <- function() {
  tabItem(
    tabName = "home",
   titlePanel("Welcome to the Soccer World!"),
    
    sidebarLayout(sidebarPanel(
      pickerInput(
        "leagues",
        "Select a league:",
        choices = c("Bundesliga 1",
                    "Bundesliga 2",
                    "Premier League",
                    "Ligue 1")
      ),
      tableOutput("home_league_flag")
    ),
    mainPanel(tabsetPanel(
      tabPanel(
        "Map",
        tags$style('.leaflet {height: 1000px;}'),
        leafletOutput("map", width = "100%", height = "400px")
       
     )
   )))
  )
  
}