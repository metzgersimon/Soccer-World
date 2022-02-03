# sub-ui for the about tab
tab_about_ui <- function() {
  tabItem(
    tabName = "about",
    fluidPage(fluidRow(column(12,
                              div(
                                style = paste0(
                                  #  "border: solid 1px #000000;",
                                  "margin-top: 20px;"
                                  #  "background-color: #004157;"
                                ),
                                h1("About The Soccer Prediction App"),
                                br(),
                                h2(em("Project Information and Goal")),
                                p(
                                  "This project has been developed as a part of the Master program for Data Science in Business and Economics at the University of Tuebingen (Germany). The project mainly aims to predict the soccer results (the accurate goals) of German first national league and German second national league based on the state-of-the-art machine learning method like random forest. With hundreds of features from different perspectives of player, team, match levels information, we select the most relative features and try to make the best prediction. We want to compare whether the accuracy of our engine can win the prediction according to the bet quote from the bookmaker. We develop a cloud-based shiny app to enable all users to visualize, analyze and interact with the plots, results and models we have. "
                                ),
                                br(),
                                h2(em("Data")),
                                p("1.Kicker
              2.Transfermarkt
              3.Football API
            "),
                                br(),
                                h2(em("Method")),
                                p("During the data project, the most challenging part for our project is data retrieving, including data scraping and data from API. We want the features from different aspects as many as possible. Firstly, we use the R selenium to scrape the data from the websites and collect the data from the Football API. Then, we clean and merge all the data we collected in order to create a large data frame which contains all the information of variables to predict the exact soccer goals, including the information of three levels - team, match and players. The prediction algorithms are random forest"),
                                br(),
                                h2(em("Remark")),
                                p("This app is developed by Simon Metzger, Anton Hoehl, Jingwen Xu. If you have any feedback and questions, do not hesitate to contact us.")
                              )
    ))))
}

