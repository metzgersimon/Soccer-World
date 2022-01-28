# subserver for the match-tab in the information menu item
information_league_match_server <- function(input, output, session){

  # create an observer to display for the club selection (home team) to display
  # only those clubs that are present for the selected season
  observeEvent(input$info_match_season, {
    updateSelectInput(session, 
                      inputId = "info_match_team1",
                      choices = c("",
                                  unique(fixtures_with_stats_2021 %>%
                                           filter(league_season == 
                                                    as.numeric(
                                                      str_split(input$info_match_season,
                                                                pattern = "/")[[1]][1])
                                                    ) %>%
                                    select(club_name_home) %>%
                                    unlist() %>%
                                    unname()
                                  )
                      )
    )
  })
  
  # create an observer to display for the club selection (away team) to display
  # only those clubs that are present for the selected season
  observeEvent(input$info_match_season, {
    updateSelectInput(session, 
                      inputId = "info_match_team2",
                      choices = c("",
                                  unique(fixtures_with_stats_2021 %>%
                                           filter(league_season == 
                                                    as.numeric(
                                                      str_split(input$info_match_season,
                                                                pattern = "/")[[1]][1])
                                           ) %>%
                                           select(club_name_home) %>%
                                           unlist() %>%
                                           unname()
                                  )
                      )
    )
  })
  

  
  # reactive function (is executed every time something changes)
  # that returns the data frame which is needed for the match statistics
  # plot
  matches_reactive <- reactive({
    # need the user inputs before continue
    req(input$info_match_season)
    req(input$info_match_team1)
    req(input$info_match_team2)
    
    # season <- 2021
    # team1 <- "Borussia Monchengladbach"
    # team2 <- "FC Bayern Munich"
    # season_half_selection <- "First half"
    
    # get the inputs in a proper format
    season_half_selection <- input$info_match_season_half
    
    # extract the number of matchdays
    number_of_rounds <- max(fixtures_with_stats_2021$league_round,
                            na.rm = TRUE)
    
    # half the number of matchdays to see how many days are in the
    # first half and how many in the second half of the season
    rounds_segment <- number_of_rounds / 2
    
    
    # extract the data which matches the selected inputs
    fixture_stats <- fixtures_with_stats_2021 %>%
      filter(league_season == 
               as.numeric(
                 str_split(input$info_match_season,
                           pattern = "/")[[1]][1]),
             season == as.numeric(
               str_split(input$info_match_season,
                         pattern = "/")[[1]][1]),
             # take only those rows where the clubs match
             club_name_home %in% c(input$info_match_team1,
                                   input$info_match_team2),
             club_name_away %in% c(input$info_match_team1,
                                   input$info_match_team2)) %>%
      data.frame()
    
    # fixture_stats <- fixtures_with_stats_2021 %>%
    #   filter(league_season == season,
    #          season == 2021,
    #          # take only those rows where the clubs match
    #          club_name_home %in% c(team1,
    #                                team2),
    #          club_name_away %in% c(team1,
    #                                team2))

    # filter the data based on the first half/second half of the season
    if(season_half_selection == "First half"){
      fixture_stats <- fixture_stats %>%
        filter(league_round <= rounds_segment)
    } else {
      fixture_stats <- fixture_stats %>%
        filter(league_round > rounds_segment)
    }
    

    # if there is no data available (no rows)
    # show a error message to inform the user that something went wrong
    if(nrow(fixture_stats) == 0){
      shinyalert(title = "Error: no data avilable for your selection!",
                 text = paste0("Probably the match for the selected teams is not ",
                               "yet played in the given season half."),
                 type = "error"
                 )
      return(fixture_stats)
    }

    # if everything is fine, work with the data
    fixture_stats <- fixture_stats %>%
      # select only the important variables
      select(fixture_date = `fixture_date.x`, fixture_time = `fixture_time.x`, 
             venue_name, venue_city, league_round,
             referee, club_name_home, club_name_away, team_name, halftime_score_home,
             halftime_score_away, fulltime_score_home = `fulltime_score_home.x`, 
             fulltime_score_away = `fulltime_score_away.x`, 
             shots_total, shots_on_goal, shots_off_goal,
             shots_blocked, shots_inside_box, shots_outside_box,
             goalkeeper_saves, fouls, corners,
             offsides, ball_possession, cards_yellow, cards_red,
             passes_total, passes_accurate, passing_accuracy) %>%
      # mutate possession and accuracy in an appropriate display format
      mutate(ball_possession = str_remove(ball_possession, pattern = "%"),
             passing_accuracy = str_remove(passing_accuracy, pattern = "%")) %>%
      # convert all the others into numeric
      mutate(across(c("shots_on_goal", "shots_off_goal", "shots_total",
                      "shots_blocked", "shots_inside_box", "shots_outside_box",
                      "fouls", "corners", "offsides", "ball_possession",
                      "cards_yellow", "cards_red", "goalkeeper_saves",
                      "passes_total", "passes_accurate", "passing_accuracy"),
                    as.numeric)) %>%
      # replace all NAs with 0 
      replace(is.na(.), 0)
    
    # extract the club names
    club_name_home <- fixture_stats$club_name_home[1]
    club_name_away <- fixture_stats$club_name_away[1]
    
    # transform the data into a wide format with appropriate
    # columns we can work with and plot the data later on
    # with pivot_longer we want to get all variables into just two columns
    # with names (variable "statistic") and values (variable "values")
    fixture_plot_data <- pivot_longer(
      fixture_stats, 
      cols = shots_total:passing_accuracy,
      names_to = "statistic",
      values_to = c("values")) %>%
      # reoder the statistic variable for the plot later
      mutate(statistic = factor(statistic, levels = c("shots_total", "shots_on_goal", "shots_off_goal",
                                                      "shots_blocked", "shots_inside_box", "shots_outside_box",
                                                      "goalkeeper_saves", "fouls", "corners",
                                                      "offsides", "ball_possession", "cards_yellow", "cards_red",
                                                      "passes_total", "passes_accurate", "passing_accuracy"))) %>%
      # group by team name and use this as key to spread the data 
      # into wide format again
      group_by(team_name) %>%
      spread(key = team_name,
             value = values) %>%
      # rename the newly generated columns into a dynamic useable format
      # with team_1 and team_2 as names
      rename(team_1 := !!club_name_home,
             team_2 := !!club_name_away) 
    
    
    # rename all columns
    colnames(fixture_plot_data) <- c("fixture_date", "fixture_time", "venue_name",
                                     "venue_city", "league_round", "referee",
                                     "club_name_home", "club_name_away", 
                                     "halftime_score_home", "halftime_score_away",
                                     "fulltime_score_home", "fulltime_score_away",
                                     "statistic","team_1", "team_2")
    
    # compute the relative values of the statistics to show 
    # them later on in the bar chart
    fixture_plot_data <- fixture_plot_data %>%
      mutate(values_team_1_rel = team_1 / (team_1 + team_2),
             values_team_2_rel = team_2 / (team_1 + team_2)) %>%
      # again, replace all NAs with 0
      replace(is.na(.), 0)
    
    
    return(fixture_plot_data)
    
  })
  
  
  # reactive function to prepare the data for the lineups tab
  lineups_reactive <- reactive({
    # need the user inputs before continue
    req(input$info_match_season)
    req(input$info_match_team1)
    req(input$info_match_team2)
    
    # season <- 2021
    # team1 <- "Borussia Monchengladbach"
    # team2 <- "Borussia Dortmund"
    # season_half_selection <- "First half"
    
    # convert the season input into a number
    season <- as.numeric(str_split(input$info_match_season,
                                    pattern = "/")[[1]][1])
    
    season_half_selection <- input$info_match_season_half
    
    # get the data for the year
    current_season_lineups <- #get(paste0("lineups_messy_", season))
      buli_2021_lineups
    
    # extract the number of matchdays
    number_of_rounds <- max(fixtures_with_stats_2021$league_round,
                            na.rm = TRUE)
    
    # number_of_rounds <- 34
    
    # half the number of matchdays to see how many days are in the
    # first half and how many in the second half of the season
    rounds_segment <- number_of_rounds / 2
    
    # based on the season half selected, we only want to observe
    # the selected half of the matchdays, i.e., for the Bundesliga (34 matchdays)
    # for the first season half, matchday 1-17 and for the second 18-34.
    if(season_half_selection == "First half"){
      important_matchdays <- c(1:rounds_segment)
    } else {
      important_matchdays <- c((rounds_segment + 1):number_of_rounds)
    }
    
    # created empty variables to store the data
    selected_match_data <- NULL
    # and to track the loop behavior
    loop_ended <- FALSE
    
    # iterate over all selected matchdays
    # for(matchday in important_matchdays){
    for(i in 1:length(current_season_lineups)){
      # select the matchday for the given iteration
      # curr_matchday <- current_season_lineups[[matchday]]
      curr_match_infos <- current_season_lineups[[i]]
      
      curr_match <- curr_match_infos[[1]]
      
      curr_match_infos <- curr_match_infos[-1]
      
      # iterate over all matches available in the matchday
      # for(j in 1:length(curr_matchday)){
      # for(j in 1:length(curr_match)){
        # select the current match
        # curr_match <- curr_matchday[[j]]
      team_1 <- curr_match[[1]]
      team_1$team_info[[1]]$team_name <- sapply(team_1$team_info[[1]]$team_name,
                                                club_name_mapping)
      team_2 <- curr_match[[2]] 
      team_2$team_info[[1]]$team_name <- sapply(team_2$team_info[[1]]$team_name,
                                                club_name_mapping)
        # check if the teams given via the user input match
        # with the current given team names in the iteration
        # if(input$info_match_team1 %in% curr_match$team_names &
        #    input$info_match_team2 %in% curr_match$team_names){
      
        if(input$info_match_team1 %in% c(team_1$team_info[[1]]$team_name,
                                         team_2$team_info[[1]]$team_name) &
           input$info_match_team2 %in% c(team_1$team_info[[1]]$team_name,
                                         team_2$team_info[[1]]$team_name)){
      
      
          # if(team1 %in% c(team_1$team_info[[1]]$team_name[[1]],
          #                team_2$team_info[[1]]$team_name[[1]]) &
          #    team2 %in% c(team_1$team_info[[1]]$team_name[[1]],
          #                 team_2$team_info[[1]]$team_name[[1]])){

          # if so, get the match data because it is the right match
          selected_match_data <- list(team_1, team_2, curr_match_infos)
          # set the variable to TRUE
          loop_ended <- TRUE
          # and break the inner loop
          break
        # }
      }
      # if the variable is set to TRUE (right match found),
      # we want also to break the outer loop
      if(loop_ended){break}
    }
    
    # if the data is empty, i.e., the match is not in the played matches
    # we want to show a error message to inform the user that something went wrong
    if(length(selected_match_data) == 0){
      shinyalert(title = "Error: no data avilable for your selection!",
                 text = paste0("Probably the match for the selected teams is not ",
                               "yet played in the given season half."),
                 type = "error"
      )
      return(selected_match_data)
    }
    
    # if everything went well, we extract the lineups for both teams
    # starting_lineups <- selected_match_data$starting_lineups
    starting_lineups_home <- selected_match_data[[1]]$starting_players[[1]]
    starting_lineups_away <- selected_match_data[[2]]$starting_players[[1]]
    
    # substitute_lineups <- selected_match_data$substitute_lineups
    substitute_lineups_home <- selected_match_data[[1]]$substitute_players[[1]]
    substitute_lineups_away <- selected_match_data[[2]]$substitute_players[[1]]
    
    # and split them into home and away team
    starting_lineups_home <- select(starting_lineups_home,
                                    player_name,
                                    player_number,
                                    player_grid) %>%
      # add a variable for team name
      mutate(team_name = selected_match_data[[1]]$team_info[[1]]$team_name)
    
    # set the column names appropriately
    colnames(starting_lineups_home) <- c("player_name", "player_number", "player_grid",
                                         "team_name")
    
    starting_lineups_away <- select(starting_lineups_away,
                                    player_name,
                                    player_number,
                                    player_grid) %>%
      # add a variable for team name
      mutate(team_name = selected_match_data[[2]]$team_info[[1]]$team_name)
    
    # set the column names appropriately
    colnames(starting_lineups_away) <- c("player_name", "player_number", "player_grid",
                                         "team_name")
    
    # starting_lineups <- bind_rows(starting_lineups_home,
    #                               starting_lineups_away)
    
    # return the data as a list (home as first element
    # and away as second element)
    return(list(starting_lineups_home,
                starting_lineups_away,
                selected_match_data[[3]],
                selected_match_data#,
               # selected_match_data$date,
                #selected_match_data$time
                )
    )
    
  })
  
  output$info_match_match_date <- renderText({
    req(lineups_reactive)
    
    # get the fourth element of the lineups reactive function
    # which contains the match date and the fifth for the match time
    lineups_data <- lineups_reactive()[[3]]
    date <- lineups_data$date
    time <- lineups_data$time
    
    paste0(date, " - ", time)
  })
  
  
  output$info_match_match_formation_home <- renderText({
    req(lineups_reactive)
    
    # get the third element of the lineups reactive function
    # which contains the starting formations
    lineups_data <- lineups_reactive()[[4]]
    formation <- lineups_data[[1]]$formation[[1]]
    
    formation
  })
  
  output$info_match_match_formation_away <- renderText({
    req(lineups_reactive)
    
    # get the third element of the lineups reactive function
    # which contains the starting formations
    lineups_data <- lineups_reactive()[[4]]
    formation <- lineups_data[[2]]$formation[[1]]
    
    formation
  })
  
  
  
  # creates the plot for the match overview, i.e., the events happened
  # during the match
  output$info_match_match_events <- renderReactable({
    req(input$info_match_season)
    req(input$info_match_team1)
    req(input$info_match_team2)
    req(input$info_match_season_half)

    # get all relevant events for the selection of the user
    relevant_events <- buli_fixture_events_2010_to_2021 %>%#
      # filter for the selected season and get only those matches 
      filter(season == as.numeric(str_split(input$info_match_season,
                                            pattern = "/")[[1]][1]),
             # which are in the correct season half
             ifelse(input$info_match_season_half == "First half",
                    matchday <= 17,
                    matchday > 17),
             # select the fitting team names to make the join faster
             team_name %in% c(input$info_match_team1, 
                              input$info_match_team2)
        ) %>%
      inner_join(buli_matches_2010_2021,
                 by = "fixture_id") %>%
      # now finally filter for the teams to get really only those events
      # that fit to the selected match
      filter(club_name_home %in% c(input$info_match_team1,
                                   input$info_match_team2),
             club_name_away %in% c(input$info_match_team1,
                                   input$info_match_team2)) %>%
      # add a new column with values for the events such that the images can be
      # loaded based on the value
      mutate(event_image = ifelse(str_to_lower(detail) %like% "goal",
                            "goal_icon",
                            ifelse(str_to_lower(detail) %like% "substitut",
                                   "change_icon",
                                   ifelse(str_to_lower(detail) %like% "yellow card",
                                          "yellow_card_icon",
                                          "red_card_icon"))),
             time_extra = ifelse(is.na(time_extra),
                                 0,
                                 time_extra)) %>%
      arrange(desc(time_elapsed), desc(time_extra)) %>%
      mutate(time_display = ifelse(time_extra == 0,
                                   time_elapsed,
                                   paste0(time_elapsed, "+", time_extra))) %>%
      select(#league_id = `league_id.x`,
             #season, matchday, fixture_date = `fixture_date.x`,
             #fixture_time = `fixture_time.x`,
             #time_elapsed, time_extra, #team_id,
             time_display,
             team_logo,
             #club_id_home, club_name_home, club_logo_home, club_id_away, 
             #club_name_away, club_logo_away, 
             event_image, #player_id,
             player_name,
             #assist_id, 
             assist_name, #type, 
             detail, comments)
    
    # test= "First half"
    # relevant_events <- buli_fixture_events_2019_2021_18_12_21 %>%
    #   filter(season == 2021,
    #          ifelse(test == "First half",
    #                 matchday <= 17,
    #                 matchday > 17),
    #          team_name %in% c("FC Bayern Munich",
    #                           "Borussia Dortmund")
    #   ) %>%
    #   inner_join(buli_matches_2010_2021,
    #              by = "fixture_id") %>%
    #   filter(club_name_home %in% c("FC Bayern Munich",
    #                                "Borussia Dortmund"),
    #          club_name_away %in% c("FC Bayern Munich",
    #                                "Borussia Dortmund")) %>%
    #   # add a new column with values for the events such that the images can be
    #   # loaded based on the value
    #   mutate(event_image = ifelse(str_to_lower(detail) %like% "goal",
    #                         "goal_icon",
    #                         ifelse(str_to_lower(detail) %like% "substitut",
    #                                "change_icon",
    #                                ifelse(str_to_lower(detail) %like% "yellow card",
    #                                       "yellow_card_icon",
    #                                       "red_card_icon"))),
    #          time_extra = ifelse(is.na(time_extra),
    #                              0,
    #                              time_extra)) %>%
    #   arrange(desc(time_elapsed), desc(time_extra)) %>%
    #   mutate(time_display = ifelse(time_extra == 0,
    #                                time_elapsed,
    #                                paste0(time_elapsed, "+", time_extra))) %>%
    #   select(time_display, league_id = `league_id.x`,
    #          season, matchday, fixture_date = `fixture_date.x`,
    #          fixture_time = `fixture_time.x`,
    #          time_elapsed, time_extra, team_id, team_name, team_logo,
    #          club_id_home, club_name_home, club_logo_home, club_id_away,
    #          club_name_away, club_logo_away, event_image)

    reactable(relevant_events,
              columns = list(
                time_display = colDef(name = "Time",
                              align = "left"),
                team_logo = colDef(name = "Team",
                                   align = "center",
                                   cell = embed_img()),
                event_image = colDef(name = "Event",
                                     align = "center",
                                     cell = function(value){
                                       img_src <- knitr::image_uri(sprintf("www/%s.png", value))
                                       image <- img(src = img_src, height = "24px", alt = value)
                                       tagList(
                                         div(style = list(display = "inline-block", width = "45px"), image)
                                         )
                                       }),
                player_name = colDef(name = "Player",
                                     align = "center",
                                     ),
                assist_name = colDef(name = "Assist from",
                                     align = "center"),
                detail = colDef("Detail",
                                align = "center"),
                comments = colDef("Reason",
                                  align = "center")
              
              ),
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
                searchInputStyle = list(width = "100%")
              )
    )

    
      
  })
  
  
  
  output$info_match_match_lineups_overview_home_text <- renderText({
    req(lineups_reactive)
    
    # get the third element of the lineups reactive function
    # which contains the starting formations
    lineups_data <- lineups_reactive()[[1]]
    # lineups_data2 <- lineups_data[[1]]
    team_name <- lineups_data$team_name %>% unique()
    
    team_name
  })
  
  output$info_match_match_lineups_overview_away_text <- renderText({
    req(lineups_reactive)
    
    # get the third element of the lineups reactive function
    # which contains the starting formations
    lineups_data <- lineups_reactive()[[2]]
    # lineups_data2 <- lineups_data[[2]]
    team_name <- lineups_data$team_name %>% unique()
    
    team_name
  })
  
  
  
  output$info_match_match_lineups_overview_home <- renderPlotly({
    req(lineups_reactive())

    lineups_data <- lineups_reactive()
    
    # lineups_data <- selected_match_data

    # extract the data for the home and away lineups
    starting_lineups_home <- lineups_data[[1]]
    
    
    # starting_lineups_home2 <- starting_lineups_home 
    
    formation_home <- lineups_data[[4]][[1]]$formation[[1]]
    
    starting_lineups_home$player_grid <- sapply(starting_lineups_home$player_grid,
                                                 map_player_position,
                                                 formation = formation_home)
    
    starting_lineups_home <- starting_lineups_home %>%
      separate(col = player_grid, into = c("row_pos", "col_pos"),
               sep = ":")
    
    # save(starting_lineups_home, file = "test.RData")
    # fixture_id <- 587177
    # lineup_example_2021 <- get_fixture_lineups(fixture_id)
    # lineup <- lineup_example_2021[[1]]$starting_players[[1]]
    # home_lineup <- #lineup %>%
    #   home_lineup %>%
      # separate(col = player_grid, into = c("row_pos", "col_pos"),
      #          sep = ":")

    # plot_ly()

    plot_ly(starting_lineups_home,
            x = ~col_pos,
            y = ~row_pos,
            text = ~player_number,
            hoverinfo = "text",
            hovertext = ~paste0(player_name),
            marker = list(size = 20,
                         color = "black")) %>%
      # add_markers(size = 15) %>%
      add_text(textpostion = "inner") %>%
      layout(#images = list(
        # source = base64enc::dataURI(file = "www/soccer_field.png"),
        # x = 0,
        # y = 1,
        # sizex = 10,
        # sizey = 1.05,
        # layer = "below"),
        xaxis = list(title = "",
                     showticklabels = FALSE,
                     showgrid = FALSE,
                     zeroline = FALSE),
        yaxis = list(title ="",
                     showticklabels = FALSE,
                     showgrid = FALSE,
                     zeroline = FALSE),
        showlegend = FALSE,
        font = list(color = "white"),
        plot_bgcolor = "rgba(11, 138, 33, 10)",
        paper_bgcolor = "rgba(11, 138, 33, 10)",
        fig_bg_color = "rgba(11, 138, 33, 10)"
      )
  })
  
  
  
  output$info_match_match_lineups_overview_away <- renderPlotly({
    req(lineups_reactive())
    
    lineups_data <- lineups_reactive()
    
    # lineups_data <- selected_match_data
    
    # extract the data for the home and away lineups
    starting_lineups_away <- lineups_data[[2]]
    
    
    # starting_lineups_home2 <- starting_lineups_home 
    
    formation_away <- lineups_data[[4]][[2]]$formation[[1]]
    
    starting_lineups_away$player_grid <- sapply(starting_lineups_away$player_grid,
                                                 map_player_position,
                                                 formation = formation_away)
    
    starting_lineups_away <- starting_lineups_away %>%
      separate(col = player_grid, into = c("row_pos", "col_pos"),
               sep = ":")
    
    # save(starting_lineups_away, file = "test.RData")


    
    plot_ly(starting_lineups_away,
            x = ~col_pos,
            y = ~row_pos,
            #size = 4,
            marker = list(size = 20,
                          color = "black"),
            text = ~player_number,
            hoverinfo = "text",
            hovertext = ~paste0(player_name)) %>%
      # add_trace() %>%
      add_text(textpostion = "inner") %>%
      layout(#images = list(
        # source = base64enc::dataURI(file = "www/soccer_field.png"),
        # x = 0,
        # y = 1,
        # sizex = 10,
        # sizey = 1.05,
        # layer = "below"),
        xaxis = list(title = "",
                     showticklabels = FALSE,
                     showgrid = FALSE,
                     zeroline = FALSE),
        yaxis = list(title ="",
                     showticklabels = FALSE,
                     showgrid = FALSE,
                     zeroline = FALSE),
        showlegend = FALSE,
        font = list(color = "white"),
        plot_bgcolor = "rgba(11, 138, 33, 10)",
        paper_bgcolor = "rgba(11, 138, 33, 10)",
        fig_bg_color = "rgba(11, 138, 33, 10)"
      )
  })
  
  
  
  # creates the plot for the match statistic for a selected match
  output$info_match_match_stats <- renderPlotly({
    # wait until the input of the matches_reactive function is given
    req(matches_reactive)
    
    # get the data
    fixture_plot_data <- matches_reactive()
    
    # if there is no data, do nothing (plot remains empty)
    if(nrow(fixture_plot_data) == 0){
      return()
    }
    
    
    # create names just for display in the plot
    plot_labels <- c("Total shots", "Shots on target", "Shots off target", 
                     "Shots blocked", "Shots inside box", "Shots outside box",
                     "Fouls", "Corners", "Offsides", "Possession", "Yellow cards",
                     "Red cards", "Goalkeeper saves", "Total passes", "Accurate passes",
                     "Passing accuracy") #%>%
      #rev()
    
    # add them as new variable
    fixture_plot_data <- fixture_plot_data %>%
      mutate(plot_labels = plot_labels)
    
    # create a list for the ordering of the y axis values because plotly
    # order it just alphabetically
    y_ordering <- list(categoryorder = "array",
                       categoryarray = plot_labels)
    
    
    # store the team names 
    home_team <- fixture_plot_data %>% 
      select(club_name_home) %>%
      pull() %>%
      unique()
    
    away_team <- fixture_plot_data %>% 
      select(club_name_away) %>%
      pull() %>%
      unique()
    
    # otherwise, construct the plot
    # for the first barplot (team 1),
    # we have to convert the values to negative values to show them on
    # the left side of the zero point
    barplot_team_1 <- plot_ly(fixture_plot_data,
                              x = ~ values_team_1_rel * (-1),
                              y = ~ plot_labels,
                              type = "bar",
                              orientation = "h",
                              name = ~ club_name_home,
                              marker = list(color = c("#5cb504")),
                              text = ~ team_1,
                              textposition = 'outside') %>%
      layout(yaxis = y_ordering)
    
    # for the barplot of team 2, we just take the values as they are
    barplot_team_2 <- plot_ly(fixture_plot_data,
                              x = ~ values_team_2_rel,
                              y = ~ plot_labels,
                              type = "bar",
                              orientation = "h",
                              name = ~ club_name_away,
                              marker = list(color = c("#2b71ba")),
                              text = ~ team_2,
                              textposition = 'outside') %>%
      layout(yaxis = y_ordering)
    
    
    # create the two barplots as subplots into one plot
    # let them share the y-axis and set the margin to 0.
    # So, there is no space between the 0 point
    subplot(barplot_team_1, barplot_team_2,
            shareY = TRUE,
            margin = 0) %>%
      # set them to grouped and remove the axis titles because we don't need them
      layout(barmode = 'grouped',
             title = paste0(home_team, "  -     ", away_team),
             xaxis = list(title = "",
                          showticklabels = FALSE),
             xaxis2 = list(title = "",
                           showticklabels = FALSE),
             yaxis = list(title = "",
                          tickfont = list(color = "white")),
             showlegend = FALSE,
             font = list(color = "white"),
             plot_bgcolor = "rgba(0, 65, 87, 10)",
             paper_bgcolor = "rgba(0, 65, 87, 10)",
             fig_bg_color = "rgba(0, 65, 87, 10)")
  })
  
  
  output$info_match_match_lineups_home_text <- renderText({
    req(lineups_reactive)
    
    # get the third element of the lineups reactive function
    # which contains the starting formations
    lineups_data <- lineups_reactive()[[1]]
    # lineups_data2 <- lineups_data[[1]]
    team_name <- lineups_data$team_name %>% unique()
    
    team_name
  })
  
  output$info_match_match_lineups_away_text <- renderText({
    req(lineups_reactive)
    
    # get the third element of the lineups reactive function
    # which contains the starting formations
    lineups_data <- lineups_reactive()[[2]]
    # lineups_data2 <- lineups_data[[2]]
    team_name <- lineups_data$team_name %>% unique()
    
    team_name
  })
  
  
  
  # creates the table for the home match lineups for a selected match
  output$info_match_match_lineups_home <- renderReactable({
    # wait until the input of the lineups_reactive function is given
    req(lineups_reactive())
    
    # get the data
    lineups <- lineups_reactive()
    # lineups <- list(starting_lineups_home,
    #                 starting_lineups_away,
    #                 selected_match_data[[3]],
    #                 selected_match_data)
    
    # if there is no data, do nothing (plot remains empty)
    if(length(lineups) == 0){
      return()
    }
    
    # extract the data for the home lineups
    starting_lineups_home <- lineups[[1]]
    
    
    # create the actual reactable (drop the team names)
    reactable(starting_lineups_home[, -c(3, 4)],
              defaultPageSize = 11,
              # # set general options for the table
              # # such as the possibility to filter or sort the table
              # # but also insert a search field
              # sortable = TRUE,
              # filterable = TRUE,
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
                searchInputStyle = list(width = "100%")
              ), 
              # modify the layout and names of the columns
              columns = list(
                player_name = colDef(name = "Player",
                                     align = "left"),
                player_number = colDef(name = "Number",
                                       align = "center"))
    )
  
  })
  
  
  # creates the table for the away match lineups for a selected match
  output$info_match_match_lineups_away <- renderReactable({
    # wait until the input of the lineups_reactive function is given
    req(lineups_reactive())
    
    # get the data
    lineups <- lineups_reactive()
    
    # if there is no data, do nothing (plot remains empty)
    if(length(lineups) == 0){
      return()
    }
    
    # extract the data for the away lineups
    starting_lineups_away <- lineups[[2]]
    
    # create the actual reactable (drop the team names)
    reactable(starting_lineups_away[, -c(3, 4)],
              defaultPageSize = 11,
              # # set general options for the table
              # # such as the possibility to filter or sort the table
              # # but also insert a search field
              # sortable = TRUE,
              # filterable = TRUE,
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
                searchInputStyle = list(width = "100%")
              ), 
              # modify the layout and names of the columns
              columns = list(
                player_name = colDef(name = "Player",
                                     align = "left"),
                player_number = colDef(name = "Number",
                                         align = "center"))
    )
  
    
  })
  
  
  # # creates the table for the home substitute lineups for a selected match
  # output$info_match_match_lineups_home <- renderReactable({
  #   # wait until the input of the lineups_reactive function is given
  #   req(lineups_reactive())
  #   
  #   # get the data
  #   lineups <- lineups_reactive()
  #   
  #   # if there is no data, do nothing (plot remains empty)
  #   if(length(lineups) == 0){
  #     return()
  #   }
  #   
  #   # extract the data for the home lineups
  #   starting_lineups_home <- lineups[[1]]
  #   
  #   # create the actual reactable (drop the team names)
  #   reactable(starting_lineups_home[, -3],
  #             defaultPageSize = 11,
  #             # # set general options for the table
  #             # # such as the possibility to filter or sort the table
  #             # # but also insert a search field
  #             # sortable = TRUE,
  #             # filterable = TRUE,
  #             # searchable = TRUE,
  #             highlight = TRUE,
  #             borderless = TRUE, 
  #             # set the theme for the table
  #             theme = reactableTheme(
  #               borderColor = "#000000",
  #               color = "#000000",
  #               backgroundColor = "#004157",
  #               highlightColor = "#2f829e",
  #               cellPadding = "8px 12px",
  #               style = list(color = "white"),
  #               searchInputStyle = list(width = "100%")
  #             ), 
  #             # modify the layout and names of the columns
  #             columns = list(
  #               player_name = colDef(name = "Player",
  #                                    align = "left"),
  #               player_position = colDef(name = "Position",
  #                                        align = "center"))
  #   )
  #   
  # })
  # 
  # 
  # # creates the table for the away substitute lineups for a selected match
  # output$info_match_match_lineups_away <- renderReactable({
  #   # wait until the input of the lineups_reactive function is given
  #   req(lineups_reactive())
  #   
  #   # get the data
  #   lineups <- lineups_reactive()
  #   
  #   # if there is no data, do nothing (plot remains empty)
  #   if(length(lineups) == 0){
  #     return()
  #   }
  #   
  #   # extract the data for the away lineups
  #   starting_lineups_away <- lineups[[2]]
  #   
  #   # create the actual reactable (drop the team names)
  #   reactable(starting_lineups_away[, -3],
  #             defaultPageSize = 11,
  #             # # set general options for the table
  #             # # such as the possibility to filter or sort the table
  #             # # but also insert a search field
  #             # sortable = TRUE,
  #             # filterable = TRUE,
  #             # searchable = TRUE,
  #             highlight = TRUE,
  #             borderless = TRUE, 
  #             # set the theme for the table
  #             theme = reactableTheme(
  #               borderColor = "#000000",
  #               color = "#000000",
  #               backgroundColor = "#004157",
  #               highlightColor = "#2f829e",
  #               cellPadding = "8px 12px",
  #               style = list(color = "white"),
  #               searchInputStyle = list(width = "100%")
  #             ), 
  #             # modify the layout and names of the columns
  #             columns = list(
  #               player_name = colDef(name = "Player",
  #                                    align = "left"),
  #               player_position = colDef(name = "Position",
  #                                        align = "center"))
  #   )
  #   
  #   
  # })
  
  
  output$info_league_match_h2h <- renderReactable({
    # wait until the input of the two teams
    req(input$info_match_team1)
    req(input$info_match_team2)
    req(input$info_match_season)
  
    # season <- 2021
    # team1 <- "Borussia Monchengladbach"
    # team2 <- "FC Bayern Munich"
    # season_half_selection <- "First half"
    
  # test <- buli_matches_2010_2021 %>%
  #   filter(league_season <= season,
  #          # take only those rows where the clubs match
  #          club_name_home %in% c(team1,
  #                                team2),
  #          club_name_away %in% c(team1,
  #                                team2),
  #          fixture_date < Sys.Date()) %>%
    
    buli_matches_2010_2021 %>%
      filter(club_name_home %in% c(input$info_match_team1,
                                   input$info_match_team2),
             club_name_away %in% c(input$info_match_team1,
                                   input$info_match_team2),
             league_season <= as.numeric(
               str_split(input$info_match_season,
                         pattern = "/")[[1]][1]),
             fixture_date < Sys.Date()) %>%
      mutate(result_fixture = paste0(fulltime_score_home, ":", fulltime_score_away)) %>%
      select(fixture_date, league_season, 
             league_round,club_name_home, result_fixture, club_name_away) %>%
      arrange(desc(fixture_date)) %>%
    reactable(
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
        searchInputStyle = list(width = "100%")
      ),
      # modify the layout and names of the columns
      columns = list(
        fixture_date = colDef(name = "Date",
                             align = "left"),
        league_season = colDef(name = "Season",
                              align = "center"),
        league_round = colDef(name = "Matchday",
                              align = "center"),
        club_name_home = colDef(name = "Home",
                               align = "center"),
        result_fixture = colDef(name = "Result",
                                align = "center"),
        club_name_away = colDef(name = "Away",
                                align = "center")
      )
    )
  })
  
}