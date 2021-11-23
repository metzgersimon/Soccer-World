ui <- dashboardPage(
  dashboardHeader(title = "Soccer-Prediction App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Information", tabName = "information", icon = icon("searchengin")),
      menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Analysis", tabName = "analysis", icon = icon("accusoft")),
      menuItem("Prediction", tabName = "prediction", icon = icon("battle-net"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),
    tabsetPanel(
      tabPanel(title = "Plot1",
               mainPanel(
                 fluidRow(
                   column(
                     width = 3, align = "center",
                     selectInput("league_selection",
                                 label = "League",
                                 choices = c("Bundesliga", "2. Bundesliga")
                     )
                   ),
                   column(
                     width = 3, align = "center",
                     selectInput("season_selection", 
                                 label = "Season",
                                 choices = paste0(c(2011:2021), "/", c(2012:2022)))
                   ),
                   column(
                     width = 3, align = "center",
                     selectInput("club_selection", 
                                 label = "Club",
                                 choices = c("Bayern Munich", "Borussia Dortmund"))
                   ),
                   column(
                     width = 3, align = "center",
                     selectInput("player_selection", 
                                 label = "Player",
                                 choices = c("Player1", "Player2"))
                   )
                 ),
                 fluidRow(
                   column(width = 12, align = "center",
                          div(style = "border: solid 1px #8999A8; margin-top: 20px;",
                              plotlyOutput("market_value_over_time") %>%
                                withSpinner(color = "blue")
                          )
                   )
                 )
               )
      ),
      tabPanel(title = "Test2",
               mainPanel(
                 fluidRow(
                   column(width = 12, align = "center",
                          div(style = "border: solid 1px #FFFFFF; margin-top: 20px;",
                              plotlyOutput("scoring_ratio") %>%
                                withSpinner(color = "blue")
                          )
                   )
                 )
               )
               
      ),
      tabPanel(title = "Test 3",
               mainPanel(
                 fluidRow(
                   column(width = 12, align = "center",
                          div(style = "border: solid 1px #FFFFFF; margin-top: 20px;",
                              plotlyOutput("scoring_ratio") %>%
                                withSpinner(color = "blue")
                          )
                   )
                 )
               )
               
      )
    )
  )
)