# sub-ui for the home tab

tab_home_ui <- function() {
  tabItem(tabName = "home",
          # create a title
          titlePanel("Welcome to the Soccer World!"),
          # first row with input selection and the map of league clubs
          fluidRow(
            column(
              width = 3,
              align = "left",
              pickerInput(
                "leagues",
                "Select a league:",
                choices = c("Bundesliga",
                            "Bundesliga 2",
                            "Premier League",
                            "Ligue 1"),
                selected = "Bundesliga"
              ),
              tableOutput("home_league_flag") # league flag
            ),
            column(
              width = 9,
              align = "left",
              tags$style('.leaflet {height: 1000px;}'),
              leafletOutput("map", width = "100%", height = "400px") # map of the clubs of leagues
            )
            
          ))
  
}

# 
# # sub-ui for the home tab
# 
# tab_home_ui <- function() {
#   tabItem(
#     tabName = "home",
#     # create a title
#     titlePanel("Welcome to the Soccer World!"),
#     # sidebarpanel design for select input
#     sidebarLayout(sidebarPanel(
#       pickerInput(
#         "leagues",
#         "Select a league:",
#         choices = c("Bundesliga",
#                     "Bundesliga 2",
#                     "Premier League",
#                     "Ligue 1"),
#         selected = "Bundesliga"
#       ),
#       tableOutput("home_league_flag")
#     ),
#     # create main panel of leaflet map
#     mainPanel(tabsetPanel(
#       # tabPanel(
#       #   "Map",
#       br(),
#       tags$style('.leaflet {height: 1000px;}'),
#       leafletOutput("map", width = "100%", height = "400px")
#       
#       # )
#     )))
#   )
#   
# }