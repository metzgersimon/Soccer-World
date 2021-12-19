# subserver for the league-tab in the information menu item
information_league_general_server <- function(input, output, session){
  
  # create an observer to display for the club selection
  # only those clubs that are present in the selected league
  # observeEvent(input$information_league_season_selection_start, {
  #   updateSelectizeInput(session, 
  #                        inputId = "information_league_team_selection",
  #                        choices = c("All", unique(
  #                          all_seasons_scoring_ratio %>%
  #                            filter(season == input$information_league_season_selection_start) %>%
  #                            select(club) %>%
  #                            unlist() %>%
  #                            unname()
  #                        ))
  #   )
  #   
  # })
  
  # create an observer to display for the club selection
  # only those clubs that are present in the selected league
  observeEvent(input$information_league_season_selection, {
    print(input$information_league_season_selection)
    data <- fixtures_bundesliga_2010_2021 %>%
      filter(league_season == 
               as.numeric(str_split(
                 input$information_league_season_selection,
                 pattern = "/")[[1]][1]
               ),
             !is.na(status_elapsed)) %>%
      select(league_round) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    updateSelectizeInput(session, 
                         inputId = "information_league_matchday_selection",
                         choices = c("All", unique(
                           data 
                         )),
                         selected = max(data) ### IMMER MAX MATCHDAY AUSWÄHLEN ###
                         # BEI PAAR SAISONS FUNKTIONIER MAX NICHT, PASST FORMAT NICHT?
                         # CLUB NOCH ALS LISTE VORHANDEN
    )
    
  })
  
  # test <- all_fixtures_bundesliga_2010_2021 %>%
  #   filter(league_season == 
  #            as.numeric(str_split(
  #              season,
  #              pattern = "/")[[1]][1]
  #            ),
  #          !is.na(status_elapsed)) %>%
  #   select(league_round) %>%
  #   unlist() %>%
  #   unname() %>%
  #   unique()
  
  
  ########### NOT READY TO USE ###########
  # create an observer to display for the club selection
  # only those clubs that are present in the selected league
  # observeEvent(input$information_league_season_selection_end, {
  #   all_seasons_scoring_ratio <- all_seasons_scoring_ratio %>%
  #     separate(col = season, into = c("season_start_year", "season_end_year"),
  #              sep = "/") %>%
  #     mutate(season_start_year = as.numeric(season_start_year),
  #            season_end_year = as.numeric(season_end_year))
  #   
  #   print(input$information_league_season_selection_start)
  #   season_start <- input$information_league_season_selection_start %>%
  #     str_split(., pattern = "/") %>%
  #     .[[1]] %>%
  #     .[1]
  #   print(season_start)
  #   
  #   season_end <- input$information_league_season_selection_end %>%
  #     str_split(., pattern = "/") %>%
  #     .[[1]] %>%
  #     .[1]
  #   
  #   print(season_end)
  #   
  #   updateSelectizeInput(session, 
  #                     inputId = "information_league_team_selection",
  #                     choices = c("All", unique(all_seasons_scoring_ratio %>%
  #                                                 filter(between(season_start_year, 
  #                                                                season_start,
  #                                                                season_end)) %>%
  #                                                 select(club) %>%
  #                                                 unlist() %>%
  #                                                 unname()))
  #   )
  # })
  
  
  
  # create an observer for the multiple seasons option
  # if the checkbox multiple seasons is not selected, there is only
  # one select field to select the season
  # otherwise, the user can select a range of seasons
  # observeEvent(input$information_league_multiple_seasons, {
  #   if(input$information_league_multiple_seasons){
  #     # if multiple selected, show the field
  #     shinyjs::show(id = "information_league_season_selection_end")
  #   } else {
  #     # otherwise, hide the field (default is not selected)
  #     shinyjs::hide(id = "information_league_season_selection_end")
  #   }
  # })
  
  
  # create the output for the table on the overview page
  output$info_league_overview_table <- function(){
    req(input$information_league_season_selection)
    
    league_infos <- season_players_joined %>% 
      filter(season_start_year == as.numeric(
        str_split(input$information_league_season_selection,
                  pattern = "/")[[1]][1]))
    
    # extract the name of the league
    name <- unique(league_infos$league)
    
    # extract the country the league takes place in
    country <- unique(league_infos$`country.y`)
    
    # compute the number of teams playing in the league
    number_teams <- length(unique(league_infos$club))
    
    # compute the average age for each club
    avg_age_per_club <- league_infos %>%
      group_by(club) %>%
      summarize(avg_age = round(mean(age, na.rm = TRUE), 2))
    
    # filter the average age for each club 
    # for the club which has the lowest avg age
    min_avg_age <- filter(avg_age_per_club,
                          avg_age == min(avg_age)) 
    
    # filter the average age for each club 
    # for the club which has the highest avg age
    max_avg_age <- filter(avg_age_per_club,
                          avg_age == max(avg_age))
    
    # compute the average age in the whole league
    avg_age_league <- league_infos %>%
      summarize(avg_age = mean(age, na.rm = TRUE)) %>%
      # pull the data because the summarize creates a data frame
      # pull extract the actual value
      pull() %>%
      # round it 
      round(2)
    
    # compute the average market value in the whole league
    avg_market_value <- league_infos %>%
      summarize(avg_market_value = mean(market_value_in_million_euro,
                                        na.rm = TRUE)) %>%
      # pull the data because the summarize creates a data frame
      # pull extract the actual value
      pull() %>%
      # round it 
      round(2)
    
    # find the most valuable player and its information
    most_valuable_player <- filter(league_infos,
                                   market_value_in_million_euro ==
                                     max(market_value_in_million_euro,
                                         na.rm = TRUE)) %>%
      # select player name, club and market value
      select(player_name, club, market_value_in_million_euro) %>%
      unique()
      
    # compute the total number of players playing in the league
    number_players <- league_infos %>%
      # unique player names
      distinct(player_name) %>%
      count()
    
    # create the text blocks for the table
    country_text <- paste0("Country: ", country)
    number_teams_text <- paste0("Number of teams: ", number_teams)
    number_players_text <- paste0("Number of players: ", number_players$n)
    avg_age_league_text <- paste0("Average age: ", avg_age_league)
    min_avg_age_text <- paste0("Joungest team: ", min_avg_age$club, " ", min_avg_age$avg_age)
    max_avg_age_text <- paste0("Oldest team: ", max_avg_age$club, " ", max_avg_age$avg_age)
    avg_market_value_text <- paste0("Average market value: ", avg_market_value)
    most_valuable_player_text <- paste0("Most valuable Player: ", most_valuable_player$player_name,
                                        " ", most_valuable_player$club,
                                        " ", most_valuable_player$market_value_in_million_euro)
    
    # put all the information together into a data frame
    # with 3 columns
    league_info_frame <- data.frame(matrix(c(country_text, number_teams_text, 
                                             number_players_text, avg_age_league_text, 
                                             min_avg_age_text, max_avg_age_text,
                                             avg_market_value_text,
                                             most_valuable_player_text,
                                             ""),
                                           ncol = 3,
                                           byrow = FALSE)
    ) 
    
    # set the NA format in a kable table to an empty string
    options(knitr.kable.NA = "")
    
    # create a kable table with the data
    league_info_frame %>%
      kableExtra::kable("html", row.names = FALSE, col.names = NULL
      ) %>%
      #kable_minimal()
      kable_styling(full_width = F) %>%
      # deparse() possible to extract string of variable name
      # but needs to be cleaned
      # set the name of the league as header
      add_header_above(c("Bundesliga" = 3), bold = TRUE,
                       font_size = 20)
    
    
  }
  
  
  
  
  output$information_league_matchday_fixtures <- renderReactable({
    fixtures_bundesliga_2010_2021 %>%
      filter(league_season == 
               as.numeric(str_split(
                 input$information_league_season_selection,
                 pattern = "/")[[1]][1]
               ),
             league_round == as.numeric(input$information_league_matchday_selection)) %>%
      mutate(game_score = paste0(fulltime_score_home, ":", 
                                 fulltime_score_away)) %>%
      select(fixture_date, fixture_time, club_name_home,
             game_score,
             club_name_away) %>%
      arrange(fixture_date, fixture_time) %>%
      # create the actual table
      reactable(
        # set general options for the table
        # such as the possibility to filter or sort the table
        # but also insert a search field
        sortable = TRUE,
        # searchable = TRUE,
        highlight = TRUE,
        borderless = TRUE, 
        # set the theme for the table
        theme = reactableTheme(
          borderColor = "#000000",
          color = "#000000",
          backgroundColor = "#004157",
          highlightColor = "#2f829e",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          # searchInputStyle = list(width = "100%")
        ), 
        # modify the layout and names of the columns
        columns = list(
          fixture_date = colDef(name = "Date",
                             align = "left"),
          fixture_time = colDef(name = "Time",
                                align = "center"),
          club_name_home = colDef(name = "Club",
                               align = "center"),
          game_score = colDef(name = "Result",
                              align = "center"),
          club_name_away = colDef(name = "Club",
                                  align = "center")
          
        ))
    
  })
  
  
  
  # create a plot for the market value over time
  # for each club
  output$market_value_over_time <- renderPlotly({
    market_values <- market_values_over_time %>%
      # currently we have to filter out these dates
      filter(cut_off_day != "2011-07-01",
             cut_off_day != "2020-07-01",
             cut_off_day != "2014-06-01",
             cut_off_day != "2014-07-01",
             cut_off_day != "2014-08-01",
             cut_off_day != "2014-09-01",
             cut_off_day != "2014-10-01") %>%
      # transform the market value at the given time to a numeric
      mutate(value_then = as.numeric(str_remove_all(value_then,"[\u20AC|m]")) * 1000000) %>%
      group_by(club) %>%
      # create actual plot for the market value over time by club
      plot_ly(x = ~cut_off_day, y = ~value_then, color = ~club,
              colors = colors) %>%
      add_lines() %>%
      layout(title = "Market value over time",
             yaxis = list(title = "Current Market Value"),
             xaxis = list(title = "Year"))
    
    market_values
  })
  
  
  
  # create a plot with animation to show all winners of the league
  # over time
  output$most_winners_since <- renderPlotly({
    
    # selected_season_start <- as.numeric(str_split(input$information_league_season_selection_start, 
    #                                               pattern = "/")[[1]][1])
    # 
    # selected_season_end <- as.numeric(str_split(input$information_league_season_selection_end, 
    #                                             pattern = "/")[[1]][1])
    
    # create the data
    all_seasons_running_winners <- all_seasons_running_table %>%
      # group by season to be able to extract for each season
      # the last matchday to extract the winner of the season
      group_by(season_start_year) %>%
      filter(matchday == max(matchday)) %>%
      # then count the number of wins for each club
      mutate(count = n(),
             # also count the number of teams
             number_of_teams = length(unique(club))) %>%
      # filter those where count is equals to number_of_teams
      # this filters out season that are not completed yet
      filter(count == number_of_teams) %>%
      # create a is_winner variable with TRUE if rank is equals to 1
      # and FALSE otherwise
      mutate(is_winner = ifelse(rank == 1,
                                TRUE,
                                FALSE)) %>%
      ungroup() %>%
      # drop the count
      select(-count) %>%
      group_by(club) %>%
      # compute the cumulative league wins by club
      mutate(cum_league_wins = cumsum(is_winner))
    
    # all_season_number_wins <- all_seasons_running_winners %>%
    #   group_by(club) %>%
    #   mutate(cum_league_wins = cumsum(is_winner))
    
    # plot the winners with animation over the seasons
    all_seasons_running_winners %>%
      plot_ly(x = ~cum_league_wins, y = ~club, color = ~club,
              colors = colors,
              frame = ~season_start_year,
              ids = ~club) %>%
      add_bars() %>%
      layout(title = paste0("Number of league wins"),
             xaxis = list(title = "Season"),
             yaxis = list(title = "# League wins"))
  })
  
  
  
  # create a plot to show the rank of every club over the season
  output$league_rank_over_season <- renderPlotly({
    # convert the selected season into a number
    selected_season <- as.numeric(str_split(input$information_league_season_selection, 
                                            pattern = "/")[[1]][1])
    
    # create the frame for the plot
    all_seasons_running_table %>%
      # filter the season to be the season selected by the user
      filter(season_start_year == selected_season) %>%
      # order them by rank
      arrange(rank) %>%
      # create the actual plot
      plot_ly(x = ~matchday, y = ~rank, color = ~club,
              colors = colors) %>%
      add_lines() %>%
      # set the title to contain the selected season
      layout(title = paste0("Cumulative ranks in season ", 
                            input$information_league_season_selection),
             xaxis = list(title = "Matchday"),
             yaxis = list(title = "Current league position", autorange = "reversed"))
  })
  
  
  # 
  # # create a plot to show all points for each club cumulated over all seasons
  # output$all_seasons_over_time <- renderPlotly({
  #   # convert the season start year into numeric
  #   selected_season_start <- as.numeric(str_split(input$information_league_season_selection_start, 
  #                                           pattern = "/")[[1]][1])
  #   # convert the season end year into numeric
  #   selected_season_end <- as.numeric(str_split(input$information_league_season_selection_end, 
  #                                                 pattern = "/")[[1]][1])
  #   
  # 
  #   plot_data %>%
  #     group_by(club, matchday) %>%
  #     summarize(cum_points_all_seasons = sum(cum_points)) %>%
  #     plot_ly(x = ~matchday, y = ~cum_points_all_seasons, color = ~club,
  #             colors = colors) %>%
  #     add_lines() %>%
  #     layout(title = paste0("Cumulative points all_seasons "),
  #            xaxis = list(title = "Matchday"),
  #            yaxis = list(title = "Cumulative points"))
  # })
  

  
 
  # create a plot for a selected season
  # to show the points over the season for all clubs
  # output$one_season_over_time <- renderPlotly({
  #   # convert the selected season into a number
  #   selected_season <- as.numeric(str_split(input$information_league_season_selection, 
  #                                           pattern = "/")[[1]][1])
  #   # create the data
  #   all_seasons_running_table %>%
  #     # filter the data to only contain data for the selected season
  #     filter(season_start_year == selected_season) %>%
  #     # create actual plot
  #     plot_ly(x = ~matchday, y = ~cum_points, color = ~club,
  #             colors = colors) %>%
  #     add_lines() %>%
  #     layout(title = paste0("Cumulative points in season "), 
  #                           #input$information_league_season_selection),
  #            xaxis = list(title = "Matchday"),
  #            yaxis = list(title = "Cumulative points"))
  # })
  
}