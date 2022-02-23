# sub-ui for the about tab
tab_wiki_ui <- function() {
  tabItem(
    tabName = "Wiki",
    fluidPage(fluidRow(column(12,
                              div(
                                style = paste0(
                                  #  "border: solid 1px #000000;",
                                  "margin-top: 20px; font-family: Century Gothic"
                                ),
                                h2(em("Guideline for this Shiny App")),
                                p(
                                  "This wiki document provides the information of the structure for this Shiny app. With the wiki you can have the 'overview' of app structure, and can hopefully find the answers if you have questions about the tabs and contents."
                                ),
                                br(), 
                                h3(em("Information Tab - League")),
                                p(
                                 "In this tab, there are two parts of league information. In general tab, you can select one league of four leagues, then you would get the overview of this selected league with league logo and country flag. After you select a specific season, then you can check the specific match infos in this selected season. The second part is the league-match tab. In this tab, you can see the more detailed infos of chosen matches. You can select the specific league, season, clubs and the season half. Then the events, formation would be firstly presented in the overview tab. The match stats and lineups of this specific match would be presented in next two tabs. In 'head to head' tab, you can compare the different match results of these two chosen clubs within and before the chosen season. The aggregate wins would be displayed so that you can get the feeling which club won more before."
                                ),
                                br(),
                                h3(em("Information Tab - Team")), 
                                p(
                                  "In this tab, you can select one specific club in one specific league and season, then you will see the fast facts with numbers of this club. Then in the next tab you can see the historical matches and upcoming matches of selected club, the match stats of the club. In 'squad' tab, you can see the squads of the club. In 'Over Time' tab, you can see the market value over time, fifa rating and transfer information of the selected club."
                                ),
                                br(),
                                h3(em("Information Tab - Player")), # remark
                                p(
                                  "This tab provides the information of the selected player in the selected season and league. Similarly, there are basic infos and fast facts with number of the selected player. In the 'statistics' tab, the summary of the statistics in the selected season would be presented as number and the radarplot with the player's capability would be displayed. If you want to check more detailed infos of matches of the player in selected season, then you will see the different tables with different types of statistics for the specific fixture date. Lastly, the fifa rating and market value of selected player would be presented as a dynamic plot."
                                ),
                                br(),
                                h3(em("Prediction Tab - Model")), # remark
                                p(
                                  "This tab presents the prediction model and the results. In 'moving accuracy' tab, you can select one benchmark metric, so that you can compare the accuracy of the benchmark and our model. If you want to see the historical or future predicted results, as usual, you can select one specific league, season, and the matchday, then you will have the prediction results without lineup information. You can select checkbox 'integrate lineups', then the prediction with lineups would also displayer in the table. Historical prediction means we use our model to predict the historical matches which shoud has the ground truth and we can calculate the accuracy. The future prediction means we use our model to predict the future matches. "
                                )
                                
                              )
    ))))
}
