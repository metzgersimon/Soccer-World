# sub-ui for the about tab
tab_about_ui <- function() {
  tabItem(
    tabName = "about",
    fluidPage(fluidRow(column(12,
                               div(
                                 style = paste0(
                                   #  "border: solid 1px #000000;",
                                   "margin-top: 20px; font-family: Century Gothic"
                                 ),
                                 # first paragraph for the project infos and goals
                                 h2(em("Project Information and Goal")),
                                 p(
                                   "This project has been developed as a part of the Master program for Data Science in Business and Economics at the University of Tuebingen (Germany). The project mainly aims to predict the soccer results (the accurate goals) of four famous leagues including German first national league (Bundesliga), German second national league (2. Bundesliga), English Premier League and French Ligue 1. The prediction based on the state-of-the-art machine learning method - XGboost. We also provide linear regression method to compare the results. With hundreds of features from different perspectives of player, team, match levels information, we select the most relative features and try to make the best prediction. We want to compare whether the accuracy of our engine can win the prediction according to the bet quote from the bookmaker. We develop a bwcloud-based shiny app to enable all users to visualize, analyze and interact with the plots, results and models we have. "
                                 ),
                                 br(), # introduce the main data source
                                 h2(em("Main Data Source")),
                                 p(
                                   "1. ",
                                   a(href = "https://www.transfermarkt.com/", "Transfermarkt"),
                                   ": provides detailed informations of leagues, matches, players and clubs, especially for player's transfer infos.",
                                   br(),
                                   "2. ",
                                   a(href = "https://dashboard.api-football.com/", "Football API"),
                                   ": provides detailed informations of leagues, matches, players and clubs.",
                                   br(),
                                   "3. ",
                                   a(href = "https://sofifa.com/", "Sofifa"),
                                   ": provides detailed informations of player's rating and club's rating.",
                                   br(),
                                   "4. ",
                                   a(href = "https://www.wikipedia.org/", "Wikipedia"),
                                   ": provides informations of venue's location including longitude and latitude, venue's image and capacity.",
                                   br(),
                                   "5. ",
                                   a(href = "https://the-odds-api.com/", "OddsAPI"),
                                   ": provides informations of future odds.",
                                   br(),
                                   "6. ",
                                   a(href = "https://www.oddsportal.com/", "Oddsportal"),
                                   ": provides informations of odds and odds comparison of different leagues."
                                 ),br(),
                                 h2(em("Image Credits")), # image credits
                                 p(
                                   "1. ",
                                   a(href = "https://dashboard.api-football.com/", "Football API"),
                                   ": provides venue image, player photos, league logo and club logo.",
                                   br(),
                                   "2. ",
                                   a(href = "https://de.cleanpng.com/png-m8uxqm/", "Football Icon"),
                                   ": provides event icon of football in match tab.",
                                   br(),
                                   "3. ",
                                   a(href = "https://de.cleanpng.com/png-wysgn7/", "Red Cards Icon"),
                                   ": provides event icon of red cards in match tab.",
                                   br(),
                                   "4. ",
                                   a(href = "https://de.cleanpng.com/png-6s8rcs/", "Yellow Cards Icon"),
                                   ": provides event icon of yellow cards in match tab.",
                                   br(),
                                   "5. ",
                                   a(href = "https://de.cleanpng.com/png-rcv77t/", "Change Icon"),
                                   ": provides event icon of change in match tab."
                                 ),
                                 br(),
                                 h2(em("Method")), 
                                 p( # briefly introduce our method
                                   "During the data project, the most challenging part for our project is data retrieving, including data scraping and data from API. We want the features from different aspects as many as possible. Firstly, we use the R selenium to scrape the data from the websites and collect the data from the Football API. Then, we clean and merge all the data we collected in order to create a large data frame which contains all the information of variables to predict the exact soccer goals, including the information of three levels - team, match and players. The prediction algorithms are xgboost and linear regression. "
                                 ),
                                 br(),
                                 h2(em("Remark")), # remark
                                p("This app is developed by ", a(href="https://www.linkedin.com/in/simon-metzger-475bb0186/","Simon Metzger"),", ", a(href="https://www.linkedin.com/in/anton-hoehl/","Anton Hoehl"),", ", a(href="https://www.linkedin.com/in/jingwen-x-254694131/","Jingwen Xu"),". Codes used to generate this Shiny are available on ",a(href= "https://github.com/metzgersimon/Soccer-Prediction-App", icon =("github"), "Github") ,". If you have any feedback and questions, do not hesitate to contact us.")
                              
                                 )
    ))))
}

