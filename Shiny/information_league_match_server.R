# subserver for the match-tab in the information menu item
information_league_match_server <- function(input, output, session){
  
  # if the league is selected we want to update the season selection and the team 
  # selection
  observeEvent(input$info_match_league, {
    # map the selected league name to the league id
    league_ID <- API_map_league_to_id(input$info_match_league)
    updateSelectizeInput(session, 
                      inputId = "info_match_season",
                      selected = NULL,
                      choices = c("",
                                  paste0(
                                    unique(
                                      all_leagues_fixture_stats %>%
                                        filter(league_id == league_ID
                                        ) %>%
                                        select(season) %>%
                                        unlist() %>%
                                        unname()
                                    )
                                    ,
                                    "/",
                                    unique(
                                      all_leagues_fixture_stats %>% filter(league_id == league_ID
                                      ) %>%
                                        select(season) %>%
                                        unlist() %>%
                                        unname()
                                    ) + 1
                                  )))
  })
  
  observeEvent(input$info_match_season, {
  
  updateSelectizeInput(session, 
                      inputId = "info_match_team1",
                      selected = NULL,
                      choices = c("",unique(all_leagues_matches %>%
                                           filter(league_name == input$info_match_league &
                                                    league_season == 
                                                    as.numeric(
                                                      str_split(input$info_match_season,
                                                                pattern = "/")[[1]][1])) %>%
                                                  
                                           select(club_name_home) %>%
                                           unlist() %>%
                                           unname())))
    
  })
  
  observeEvent(input$info_match_season, {
  updateSelectizeInput(session, 
                      inputId = "info_match_team2",
                      selected = NULL,
                      choices = c("",unique(all_leagues_matches %>%
                                           filter(league_name == input$info_match_league &
                                                    league_season == 
                                                    as.numeric(
                                                      str_split(input$info_match_season,
                                                                pattern = "/")[[1]][1])) %>%
                                           
                                           select(club_name_away) %>%
                                           unlist() %>%
                                           unname())))
  })
  
  # # create an observer to display for the season selection (home team) to display
  # # only those clubs that are present for the selected leagues
  # observeEvent(input$info_match_season, {
  #   # map the selected league name to the league id
  #   league_ID <- API_map_league_to_id(input$info_match_league)
  #   updateSelectizeInput(session, 
  #                     inputId = "info_match_team1",
  #                     choices = c(unique(all_leagues_matches %>%
  #                                          filter(league_name == input$info_match_league &
  #                                                   league_season == 
  #                                                   as.numeric(
  #                                                     str_split(input$info_match_season,
  #                                                               pattern = "/")[[1]][1])
  #                                          ) %>%
  #                                          select(club_name_home) %>%
  #                                          unlist() %>%
  #                                          unname())), selected = NULL)
  # })
  # 
  # 
  # observeEvent(input$info_match_season, {
  #   # map the selected league name to the league id
  #   league_ID <- API_map_league_to_id(input$info_match_league)
  #   updateSelectizeInput(session, 
  #                     inputId = "info_match_team2",
  #                     choices = c(unique(all_leagues_matches %>%
  #                                          filter(league_name == input$info_match_league &
  #                                                   league_season == 
  #                                                   as.numeric(
  #                                                     str_split(input$info_match_season,
  #                                                               pattern = "/")[[1]][1])
  #                                          ) %>%
  #                                          select(club_name_away) %>%
  #                                          unlist() %>%
  #                                          unname())), selected = NULL)
  # })
  # 

  # reactive function (is executed every time something changes)
  # that returns the data frame which is needed for the match statistics
  # plot
  matches_reactive <- reactive({
    # need the user inputs before continue
    req(input$info_match_league)
    req(input$info_match_season)
    req(input$info_match_team1)
    req(input$info_match_team2)
    req(input$info_match_season_half)
    
    # season_n <- 2016
    # team1 <- "SV Werder Bremen"
    # team2 <- "FC Bayern Munich"
    # season_half_selection <- "First half"
    # season_selection <- 2016
    
    
    # get the inputs in a proper format
    season_half_selection <- input$info_match_season_half
    season_selection <- as.numeric(str_split(input$info_match_season,
                                             pattern = "/")[[1]][1])
    
    # map the league name to the league id
    league_ID <- API_map_league_to_id(input$info_match_league)
      
    # # get the fixture id of the selected match
    # fixture_ID <- all_leagues_matches %>%
    #   filter(league_id == league_ID,
    #          league_season == season_selection,
    #          # take only those rows where the clubs match
    #          club_name_home %in% c(team1,
    #                                team2),
    #          club_name_away %in% c(team1,
    #                                team2))
    
    # extract the information for the possible fixture ids
    fixture_ID <- all_leagues_matches %>%
      filter(league_id == league_ID,
             league_season == season_selection,
             # take only those rows where the clubs match
             club_name_home %in% c(input$info_match_team1,
                                   input$info_match_team2),
             club_name_away %in% c(input$info_match_team1,
                                   input$info_match_team2)) 

    # extract the number of matchdays
    number_of_rounds <- all_leagues_matches %>%
      filter(league_id == league_ID,
             league_season == season_selection) %>%
      summarise(number_rounds = max(league_round, na.rm = TRUE)) %>%
      select(number_rounds) %>% 
      pull()
    
    
    # half the number of matchdays to see how many days are in the
    # first half and how many in the second half of the season
    rounds_segment <- number_of_rounds / 2
    
    
    # filter the data based on the first half/second half of the season
    if(season_half_selection == "First half"){
      fixture_ID <- fixture_ID %>%
        filter(league_round <= rounds_segment)
    } else {
      fixture_ID <- fixture_ID %>%
        filter(league_round > rounds_segment)
    }
      
    
    # extract the match that is actually wanted by the user
    fixture_stats <- all_leagues_fixture_stats %>%
      filter(league_id == league_ID,
             season == season_selection,
             fixture_id == fixture_ID$fixture_id)
    
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
    
    # # if everything is fine, work with the data
    # fixture_stats <- fixture_stats %>%
    #   # select only the important variables
    #   select(fixture_date.y , fixture_time.y, 
    #          venue_name, venue_city, matchday,
    #          referee, team_name, halftime_score_home,
    #          halftime_score_away, fulltime_score_home.y, 
    #          fulltime_score_away.y, team_name,
    #          shots_total, shots_on_goal, shots_off_goal,
    #          shots_blocked, shots_inside_box, shots_outside_box,
    #          goalkeeper_saves, fouls, corners,
    #          offsides, ball_possession, cards_yellow, cards_red,
    #          passes_total, passes_accurate, passing_accuracy) %>%
    #   # mutate possession and accuracy in an appropriate display format
    #   mutate(ball_possession = str_remove(ball_possession, pattern = "%"),
    #          passing_accuracy = str_remove(passing_accuracy, pattern = "%")) %>%
    #   # convert all the others into numeric
    #   mutate(across(c("shots_on_goal", "shots_off_goal", "shots_total",
    #                   "shots_blocked", "shots_inside_box", "shots_outside_box",
    #                   "fouls", "corners", "offsides", "ball_possession",
    #                   "cards_yellow", "cards_red", "goalkeeper_saves",
    #                   "passes_total", "passes_accurate", "passing_accuracy"),
    #                 as.numeric)) %>%
    #   # replace all NAs with 0 
    #   replace(is.na(.), 0)
    
    # extract the club names
    club_name_home <- fixture_stats$team_name[1]
    club_name_away <- fixture_stats$team_name[2]
    
    # create a new variable for the score
    # home team
    fixture_stats_home <- fixture_stats[1,] %>%
      mutate(score = fulltime_score_home)
    
    # away team
    fixture_stats_away <- fixture_stats[2,] %>%
      mutate(score = fulltime_score_away)
    
    # bind them back together
    fixture_stats <- bind_rows(fixture_stats_home,
                               fixture_stats_away)
    
    # drop columns we do not need
    fixture_stats <- fixture_stats %>%
      select(team_name, score, shots_on_goal:passing_accuracy)
    
    # transform the data into a wide format with appropriate
    # columns we can work with and plot the data later on
    # with pivot_longer we want to get all variables into just two columns
    # with names (variable "statistic") and values (variable "values")
    print("TEST")
    fixture_plot_data <- pivot_longer(
      fixture_stats, 
      cols = c(shots_on_goal:passing_accuracy,
               score),
      names_to = "statistic",
      values_to = c("values")) %>%
      # reoder the statistic variable for the plot later
      mutate(statistic = factor(statistic, levels = c("score", "shots_total", "shots_on_goal", "shots_off_goal",
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
    
    print("TEST2")
    # rename all columns
    colnames(fixture_plot_data) <- c("statistic","team_1", "team_2")
    
    # compute the relative values of the statistics to show 
    # them later on in the bar chart
    fixture_plot_data <- fixture_plot_data %>%
      mutate(values_team_1_rel = team_1 / (team_1 + team_2),
             values_team_2_rel = team_2 / (team_1 + team_2)) %>%
      # again, replace all NAs with 0
      replace(is.na(.), 0)
    
    # team names
    team_names <- c(club_name_home, club_name_away)
    
    # return the plot data and the team names
    return(list(fixture_plot_data, team_names))
    
  })
  
  ############### match stats tab begins ######################
  # creates the plot for the match statistic for a selected match
  output$info_match_match_stats <- renderPlotly({
    # wait until the input of the matches_reactive function is given
    #req(matches_reactive())
    
    # get the data
    matches_reac_data <- matches_reactive()
    
    # extract the data for the plot
    fixture_plot_data <- matches_reac_data[[1]]
    # fixture_plot_data2 <- info[[1]]
    
    # extract the team names
    team_names <- matches_reac_data[[2]]
    # team_names <- info[[2]]
    
    # if there is no data, do nothing (plot remains empty)
    if(nrow(fixture_plot_data) == 0){
      return()
    }

    # create names just for display in the plot
    plot_labels <- c("Goals", "Total shots", "Shots on target", "Shots off target", 
                     "Shots blocked", "Shots inside box", "Shots outside box",
                     "Fouls", "Corners", "Offsides", "Possession", "Yellow cards",
                     "Red cards", "Goalkeeper saves", "Total passes", "Accurate passes",
                     "Passing accuracy")
    
    # add them as new variable
    fixture_plot_data <- fixture_plot_data %>%
      mutate(plot_labels = plot_labels)
    
    # create a list for the ordering of the y axis values because plotly
    # order it just alphabetically
    y_ordering <- list(categoryorder = "array",
                       categoryarray = plot_labels)
    
    
    # otherwise, construct the plot
    # for the first barplot (team 1),
    # we have to convert the values to negative values to show them on
    # the left side of the zero point
    barplot_team_1 <- plot_ly(fixture_plot_data,
                              x = ~ values_team_1_rel * (-1),
                              y = ~ plot_labels,
                              type = "bar",
                              orientation = "h",
                              name = ~ team_names[1],
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
                              name = ~ team_names[2],
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
             title = paste0(team_names[1], "  -     ", team_names[2]),
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
  
  
  
  # reactive function to prepare the data for the lineups tab
  lineups_reactive <- reactive({
    # need the user inputs before continue
    req(input$info_match_league)
    req(input$info_match_season)
    req(input$info_match_team1)
    req(input$info_match_team2)
    req(input$info_match_season_half)
    
    # season_selection <- 2016
    # team1 <- "FC Bayern Munich"
    # team2 <- "SV Darmstadt 98"
    # season_half_selection <- "First half"
    # league <- "Bundesliga"
    # number_of_rounds <- 34
    
    season_half_selection <- input$info_match_season_half
    season_selection <- input$info_match_season
    
    # get the data for the year
    # match_lineups <- all_leagues_fixture_lineups %>%
    #   filter(league_name == league,
    #          league_season == season_selection,
    # club_name_home %in% c(team1, team2),
    # club_name_away %in% c(team1, team2)) %>%
    #   data.frame()
    
    # get the lineups for the selected match
    match_lineups <- all_leagues_fixture_lineups %>% 
      filter(league_name == input$info_match_league,
             league_season == as.numeric(str_split(input$info_match_season,
                                                   pattern = "/")[[1]][1]),
             club_name_home %in% c(input$info_match_team1, input$info_match_team2),
             club_name_away %in% c(input$info_match_team1, input$info_match_team2)) %>%
      data.frame()
    
    
    # extract the number of matchdays
    number_of_rounds <- all_leagues_matches %>%
      filter(league_name == input$info_match_league,
             league_season == as.numeric(str_split(input$info_match_season,
                                                   pattern = "/")[[1]][1])) %>%
      summarise(number_rounds = max(league_round, na.rm = TRUE)) %>%
      select(number_rounds) %>% 
      pull()
    
    
    # half the number of matchdays to see how many days are in the
    # first half and how many in the second half of the season
    rounds_segment <- number_of_rounds / 2
    
    
    # filter the data based on the first half/second half of the season
    if(season_half_selection == "First half"){
      match_lineups <- match_lineups %>%
        filter(league_round <= rounds_segment)
    } else {
      match_lineups <- match_lineups %>%
        filter(league_round > rounds_segment)
    }
    
    
    # created empty variables to store the data
#    selected_match_data <- NULL
    # and to track the loop behavior
#    loop_ended <- FALSE
    
    # iterate over all selected matchdays
#    for(matchday in important_matchdays){
      # for(i in 1:length(current_season_lineups)){
      # select the matchday for the given iteration
#      curr_matchday <- current_season_lineups[[matchday]]
      # curr_match_infos <- current_season_lineups[[i]]
      
#      curr_match <- curr_match_infos[[1]]
      
#      curr_match_infos <- curr_match_infos[-1]
      
      # iterate over all matches available in the matchday
      # for(j in 1:length(curr_matchday)){
      # for(j in 1:length(curr_match)){
      # select the current match
      # curr_match <- curr_matchday[[j]]
#      team_1 <- curr_match[[1]]
#      team_1$team_info[[1]]$team_name <- sapply(team_1$team_info[[1]]$team_name,
#                                                club_name_mapping)
#      team_2 <- curr_match[[2]] 
#      team_2$team_info[[1]]$team_name <- sapply(team_2$team_info[[1]]$team_name,
#                                                club_name_mapping)
      # check if the teams given via the user input match
      # with the current given team names in the iteration
      # if(input$info_match_team1 %in% curr_match$team_names &
      #    input$info_match_team2 %in% curr_match$team_names){
      
 #     if(input$info_match_team1 %in% c(team_1$team_info[[1]]$team_name,
 #                                      team_2$team_info[[1]]$team_name) &
 #        input$info_match_team2 %in% c(team_1$team_info[[1]]$team_name,
 #                                      team_2$team_info[[1]]$team_name)){
        
        
        # if(team1 %in% c(team_1$team_info[[1]]$team_name[[1]],
        #                team_2$team_info[[1]]$team_name[[1]]) &
        #    team2 %in% c(team_1$team_info[[1]]$team_name[[1]],
        #                 team_2$team_info[[1]]$team_name[[1]])){
        
        # if so, get the match data because it is the right match
#        selected_match_data <- list(team_1, team_2, curr_match_infos)
        # set the variable to TRUE
#        loop_ended <- TRUE
        # and break the inner loop
#        break
        # }
#      }
      # if the variable is set to TRUE (right match found),
      # we want also to break the outer loop
#      if(loop_ended){break}
#    }
    
    # if the data is empty, i.e., the match is not in the played matches
    # we want to show a error message to inform the user that something went wrong
#    if(length(selected_match_data) == 0){
#      shinyalert(title = "Error: no data avilable for your selection!",
#                 text = paste0("Probably the match for the selected teams is not ",
#                               "yet played in the given season half."),
#                 type = "error"
#      )
#      return(selected_match_data)
#    }

    # if there is no data available, throw an error
    if(length(match_lineups) == 0) {
      shinyalert(
        title = "Error: no data avilable for your selection!",
        text = paste0(
          "Probably the match for the selected teams is not ",
          "yet played in the given season half."
        ),
        type = "error"
      )
      return(current_season_lineups)
    }

    
    # extract the home and the away team
    home_team <- unique(match_lineups$club_name_home)
    away_team <- unique(match_lineups$club_name_away)
    
    # get the starting lineups for the home team with the necessary columns
    starting_lineups_home <- match_lineups %>% 
      filter(is_starting_grid == TRUE,
             team_name == home_team) %>%
      select(team_name,
             player_name,
             player_number,
             player_grid,
             # also get the formation
             formation,
             # to be able to join additional information later
             player_id)
    
    # get the starting lineups for the away team with the necessary columns
    starting_lineups_away <- match_lineups %>% 
      filter(is_starting_grid == TRUE,
             team_name == away_team) %>%
      select(team_name,
             player_name,
             player_number,
             player_grid,
             # also get the formation
             formation,
             # to be able to join additional information later
             player_id)
    
    
    
    # create a list with the lineups
    lineups <- list(starting_lineups_home,
                    starting_lineups_away)
    
    # create a vector with the date and time information
    date_info <- list(unique(match_lineups$fixture_date),
                      unique(match_lineups$fixture_time))
    
    
    # return the data as a list lineups contains the home and away lineups
    # and date info the match date and time
    return(list(lineups, date_info))
           
  })
 
  ############### overview tab begins ######################
  output$info_match_match_date <- renderText({
    req(lineups_reactive)
    
    # get the third element of the lineups reactive function
    # which contains the match date and match time
    lineups_data <- lineups_reactive()[[2]]
    
    # paste the date information together
    paste0(lineups_data[[1]], " - ", lineups_data[[2]])
  })
  
  # output the formation of the home team
  output$info_match_match_formation_home <- renderText({
    req(lineups_reactive)

    # get the first element of the lineups reactive function
    # which contains lineups including the formation
    lineups_data <- lineups_reactive()[[1]]
    formation <- unique(lineups_data[[1]]$formation)

    formation
  })
  
  # output the formation of the away team
  output$info_match_match_formation_away <- renderText({
    req(lineups_reactive)

    # get the first element of the lineups reactive function
    # which contains lineups including the formation
    lineups_data <- lineups_reactive()[[1]]
    formation <- unique(lineups_data[[2]]$formation)

    formation
  })
  
  
  
  # creates the plot for the match overview, i.e., the events happened
  # during the match
  output$info_match_match_events <- renderReactable({
    req(input$info_match_league)
    req(input$info_match_season)
    req(input$info_match_team1)
    req(input$info_match_team2)
    req(input$info_match_season_half)
    
    # season <- 2021
    # team1 <- "Borussia Monchengladbach"
    # team2 <- "Borussia Dortmund"
    # season_half_selection <- "First half"
    # league <- "Bundesliga"
    
    # get all relevant events for the selection of the user
    # relevant_events <- all_leagues_fixture_events %>%#
    #   # filter for the selected season and get only those matches
    #   filter(league_season == season,
    #          league_name == "Bundesliga",
    #          # select the fitting team names to make the join faster
    #          club_name_home %in% c(team1,
    #                                team2),
    #          club_name_away %in% c(team1,
    #                                team2))
    
    
    season_half_selection <- input$info_match_season_half
    
    # get all relevant events for the selection of the user
    relevant_events <- all_leagues_fixture_events %>%#
      # filter for the selected season and get only those matches 
      filter(league_season == as.numeric(str_split(input$info_match_season,
                                            pattern = "/")[[1]][1]),
             league_name == input$info_match_league,
             # select the fitting team names to make the join faster
             club_name_home %in% c(input$info_match_team1,
                                   input$info_match_team2),
             club_name_away %in% c(input$info_match_team1,
                                   input$info_match_team2))
    
    
    # number_of_rounds <- all_leagues_matches %>%
    #   filter(league_name == "Bundesliga",
    #          league_season == 2021) %>%
    #   # group_by(league_name, league_season) %>%
    #   summarise(number_rounds = max(league_round, na.rm = TRUE)) %>%
    #   select(number_rounds) %>%
    #   pull()
    
    # extract the number of matchdays
    number_of_rounds <- all_leagues_matches %>%
      filter(league_name == input$info_match_league,
             league_season == as.numeric(str_split(input$info_match_season,
                                                   pattern = "/")[[1]][1])) %>%
      summarise(number_rounds = max(league_round, na.rm = TRUE)) %>%
      select(number_rounds) %>% 
      pull()
    
    
    # half the number of matchdays to see how many days are in the
    # first half and how many in the second half of the season
    rounds_segment <- number_of_rounds / 2
    
    
    # filter the data based on the first half/second half of the season
    if(season_half_selection == "First half"){
      relevant_events <- relevant_events %>%
        filter(league_round <= rounds_segment)
    } else {
      relevant_events <- relevant_events %>%
        filter(league_round > rounds_segment)
    }
      
    # create the table for the events we want to display
    events_table <- relevant_events %>%
      # add a new column with values for the events such that the images can be
      # loaded based on the value
      mutate(event_image = ifelse(str_to_lower(event_detail) %like% "goal",
                                  "goal_icon",
                                  ifelse(str_to_lower(event_detail) %like% "substitut",
                                         "change_icon",
                                         ifelse(str_to_lower(event_detail) %like% "yellow card",
                                                "yellow_card_icon",
                                                "red_card_icon")))) %>%
      # arrange the events descending based on the times
      arrange(desc(time_elapsed), desc(time_extra)) %>%
      # create a variable to display the time in an apropriate format
      mutate(time_display = ifelse(time_extra == 0,
                                   paste0(time_elapsed, "'"),
                                   paste0(time_elapsed, "+", time_extra, "'")),
             # if there is no assist player given we insert a dash
             assist_name = ifelse(is.na(assist_name),
                                  "-", assist_name)) %>%
      # reorder the data frame and drop values
      select(time_display, team_logo, event_image, player_name,
             assist_name, event_detail)#, event_comments)
    
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
    
    # create the reactable table for all the events happened during 
    # the given match
    reactable(events_table,
              # rename the columns and add the images
              columns = list(
                time_display = colDef(name = "Time",
                                      align = "left"),
                team_logo = colDef(name = "Team",
                                   align = "center",
                                   cell = embed_img(height = "30px", width = "30px")),
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
                                     align = "center"),
                assist_name = colDef(name = "Assist from",
                                     align = "center"),
                event_detail = colDef("Detail",
                                      align = "center")#,
                # event_comments = colDef("Reason",
                #                         align = "center")
                
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
    
    # get the first element of the lineups reactive function
    # to get the team name
    lineups_data <- lineups_reactive()[[1]]
    team_name <- lineups_data[[1]]$team_name %>% unique()
    
    team_name
  })
  
  output$info_match_match_lineups_overview_away_text <- renderText({
    req(lineups_reactive)
    
    # get the first element of the lineups reactive function
    # to get the team name
    lineups_data <- lineups_reactive()[[1]]
    team_name <- lineups_data[[2]]$team_name %>% unique()
    
    team_name
  })
  
  
  
  output$info_match_match_lineups_overview_home <- renderPlotly({
    req(lineups_reactive())
    
    lineups_data <- lineups_reactive()[[1]]
    
    # extract the lineups data for the home team
    starting_lineups_home <- lineups_data[[1]]
    # starting_lineups_home <- lineups[[1]]
    
    # drop the player id columns
    starting_lineups_home <- select(starting_lineups_home, -player_id)
    
    # extract the formation for the home team
    formation_home <- unique(starting_lineups_home$formation)
    
    # map the player positions on a grid with the map_player_position function
    starting_lineups_home$player_grid <- sapply(starting_lineups_home$player_grid,
                                                map_player_position,
                                                formation = formation_home)
    
    # split the player_grid columns into two columns named row_pos and col_pos
    # to be able to plot the data on a grid
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
            marker = list(size = 25,
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
    
    lineups_data <- lineups_reactive()[[1]]
    
    # extract the lineups data for the away team
    starting_lineups_away <- lineups_data[[2]]
    # starting_lineups_away <- lineups[[2]]
    
    # drop the player id columns
    starting_lineups_away <- select(starting_lineups_away, -player_id)
    
    # extract the formation for the home team
    formation_away <- unique(starting_lineups_away$formation)
    
    # map the player positions on a grid with the map_player_position function
    starting_lineups_away$player_grid <- sapply(starting_lineups_away$player_grid,
                                                map_player_position,
                                                formation = formation_away)
    
    # split the player_grid columns into two columns named row_pos and col_pos
    # to be able to plot the data on a grid
    starting_lineups_away <- starting_lineups_away %>%
      separate(col = player_grid, into = c("row_pos", "col_pos"),
               sep = ":")
    
    plot_ly(starting_lineups_away,
            x = ~col_pos,
            y = ~row_pos,
            #size = 4,
            marker = list(size = 25,
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
  
  
  ############### lineup tab begins ######################
  # creates the table for the home match lineups for a selected match
  output$info_match_match_lineups_home <- renderReactable({
    # wait until the input of the lineups_reactive function is given
    req(lineups_reactive())
    # also wait for the other inputs
    req(input$info_match_league)
    req(input$info_match_season)
    req(input$info_match_team1)
    req(input$info_match_team2)
    req(input$info_match_season_half)
    
    # get the data
    lineups <- lineups_reactive()[[1]]
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
    
    # player infos
    starting_lineups_home_player_infos <- all_leagues_player_stats %>%
      filter(player_id %in% starting_lineups_home$player_id,
             league_name == input$info_match_league,
             league_season == input$info_match_season)#,
             # club_name_home %in% c(input$info_match_team1,
             #                       input$info_match_team2),
             # club_name_away %in% c(input$info_match_team1,
             #                       input$info_match_team2))
    
    starting_lineups_home_player_infos <- all_leagues_player_stats %>%
      filter(player_id %in% starting_lineups_home$player_id,
             league_name == "Bundesliga",
             league_season == 2016)#,
    # club_name_home %in% c("FC Bayern Munich",
    #                       "SV Darmstadt 98"),
    # club_name_away %in% c("FC Bayern Munich",
    #                       "SV Darmstadt 98"))
    
    # aggregate the data for each player in the starting lineup
    home_player_infos_agg <- starting_lineups_home_player_infos %>%
      group_by(player_id, player_name, games_position, games_number) %>%
      summarize(across(c(games_minutes, games_rating, goals_total, shots_total, passes_total,
                         passes_accuracy, duels_total, duels_won),
                       ~ mean(.x, na.rm = TRUE))) %>%
      ungroup() %>%
      select(-player_id) %>%
      unique()
    
    # convert the position into a factor
    home_player_infos_agg$games_position <- factor(home_player_infos_agg$games_position,
                                                   levels = c("G", "D", "M", "F"))

    # reorder the data set based on this factor
    home_player_infos_agg <- home_player_infos_agg %>%
      arrange(games_position) %>%
      # round the values
      mutate(# create a variable for the successful duels
        successful_duels = (duels_won / duels_total) * 100,
        across(c(games_minutes:successful_duels),
               ~ round(.x, digits = 0)),
        # divide the passing accuracy/duel quota by 100 to present them as percentages
        passes_accuracy = passes_accuracy / 100,
        successful_duels = successful_duels / 100,
        # rename the positions
        games_position = ifelse(games_position == "G",
                                "Goal", 
                                ifelse(games_position == "D",
                                       "Defense", 
                                       ifelse(games_position == "M",
                                              "Midfield",
                                              ifelse(games_position == "F",
                                                     "Attack",
                                                     games_position))))) %>%
      # drop columns
      select(-c(duels_total, duels_won))
    
    # create the actual reactable (drop the team names)
    reactable(home_player_infos_agg,
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
                games_number = colDef(name = "Number",
                                      align = "center"),
                games_position = colDef(name = "Position",
                                     align = "center"),
                games_minutes = colDef(name = "Minutes",
                                     align = "center"),
                games_rating = colDef(name = "Rating",
                                      align = "center"),
                goals_total = colDef(name = "Goals",
                                        align = "center"),
                shots_total = colDef(name = "Shots",
                                        align = "center"),
                passes_total = colDef(name = "Passes",
                                       align = "center"),
                passes_accuracy = colDef(name = "Pass Accuracy",
                                      align = "center",
                                      format = colFormat(percent = TRUE)),
                successful_duels = colDef(name = "Duel Quota",
                                     align = "center", 
                                     format = colFormat(percent = TRUE)))
                
    )
    
  })
  
  
  # creates the table for the away match lineups for a selected match
  output$info_match_match_lineups_away <- renderReactable({
    # wait until the input of the lineups_reactive function is given
    req(lineups_reactive())
    # also wait for the other inputs
    req(input$info_match_league)
    req(input$info_match_season)
    req(input$info_match_team1)
    req(input$info_match_team2)
    req(input$info_match_season_half)
    
    # get the data
    lineups <- lineups_reactive()[[1]]
    
    # if there is no data, do nothing (plot remains empty)
    if(length(lineups) == 0){
      return()
    }
    
    # extract the data for the away lineups
    starting_lineups_away <- lineups[[2]]
    
    # player infos
    starting_lineups_away_player_infos <- all_leagues_player_stats %>%
      filter(player_id %in% starting_lineups_away$player_id,
             league_name == input$info_match_league,
             league_season == input$info_match_season)#,
    # club_name_home %in% c(input$info_match_team1,
    #                       input$info_match_team2),
    # club_name_away %in% c(input$info_match_team1,
    #                       input$info_match_team2))
    
    # starting_lineups_away_player_infos <- all_leagues_player_stats %>%
    #   filter(player_id %in% starting_lineups_away$player_id,
    #          league_name == "Bundesliga",
    #          league_season == 2016)#,
    # club_name_home %in% c("FC Bayern Munich",
    #                       "SV Darmstadt 98"),
    # club_name_away %in% c("FC Bayern Munich",
    #                       "SV Darmstadt 98"))
    
    # aggregate the data for each player in the starting lineup
    away_player_infos_agg <- starting_lineups_away_player_infos %>%
      group_by(player_id, player_name, games_position, games_number) %>%
      summarize(across(c(games_minutes, games_rating, goals_total, shots_total, passes_total,
                         passes_accuracy, duels_total, duels_won),
                       ~ mean(.x, na.rm = TRUE))) %>%
      ungroup() %>%
      select(-player_id) %>%
      unique()
    
    # convert the position into a factor
    away_player_infos_agg$games_position <- factor(away_player_infos_agg$games_position,
                                                   levels = c("G", "D", "M", "F"))
    
    # reorder the data set based on this factor
    away_player_infos_agg <- away_player_infos_agg %>%
      arrange(games_position) %>%
      # round the values
      mutate(# create a variable for the successful duels
        successful_duels = (duels_won / duels_total) * 100,
        across(c(games_minutes:successful_duels),
               ~ round(.x, digits = 0)),
        # divide the passing accuracy/duel quota by 100 to present them as percentages
        passes_accuracy = passes_accuracy / 100,
        successful_duels = successful_duels / 100,
        # rename the positions
        games_position = ifelse(games_position == "G",
                                "Goal", 
                                ifelse(games_position == "D",
                                       "Defense", 
                                       ifelse(games_position == "M",
                                              "Midfield",
                                              ifelse(games_position == "F",
                                                     "Attack",
                                                     games_position))))) %>%
      # drop columns
      select(-c(duels_total, duels_won))
    
    # create the actual reactable (drop the team names)
    reactable(away_player_infos_agg,
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
                games_number = colDef(name = "Number",
                                      align = "center"),
                games_position = colDef(name = "Position",
                                        align = "center"),
                games_minutes = colDef(name = "Minutes",
                                       align = "center"),
                games_rating = colDef(name = "Rating",
                                      align = "center"),
                goals_total = colDef(name = "Goals",
                                     align = "center"),
                shots_total = colDef(name = "Shots",
                                     align = "center"),
                passes_total = colDef(name = "Passes",
                                      align = "center"),
                passes_accuracy = colDef(name = "Pass Accuracy",
                                         align = "center",
                                         format = colFormat(percent = TRUE)),
                successful_duels = colDef(name = "Duel Quota",
                                          align = "center", 
                                          format = colFormat(percent = TRUE)))
              
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
  
  ######################## head to heads begins
  output$Aggregate_wins_1 <- renderValueBox({
    # there has to be a club selected
    req(input$info_match_league)
    req(input$info_match_season)
    req(input$info_match_team1)
    req(input$info_match_team2)
    
    # filter the all leagues matches table 
    wins_as_home <- all_leagues_matches %>%
      # for the club names
      filter(club_name_home == input$info_match_team1 & club_name_away == input$info_match_team2,
             # the selected season (smaller or equal than the current season, i.e.,
             # all matches of these two teams in the past)
             league_season <= as.numeric(
               str_split(input$info_match_season,
                         pattern = "/")[[1]][1]),
             fixture_date < Sys.Date()) %>%
      # add a variable for the result
      mutate(result_fixture = fulltime_score_home-fulltime_score_away) %>%
      # arrange them based on the date
      count(result_fixture>0)  

    # if the wins_as_home has no TRUE row it means team had no wins 
    if (nrow(wins_as_home[wins_as_home$`result_fixture > 0` == TRUE, ])==0) {
      wins_as_home = 0
    } else{ # else set the number to numeric
      wins_as_home <- 
        as.numeric(wins_as_home[wins_as_home$`result_fixture > 0` == TRUE, ]$n)
      
    }
    
    
    wins_as_away <- all_leagues_matches %>%
      # for the club names
      filter(club_name_away == input$info_match_team1& club_name_home == input$info_match_team2,
             # the selected season (smaller or equal than the current season, i.e.,
             # all matches of these two teams in the past)
             league_season <= as.numeric(
               str_split(input$info_match_season,
                         pattern = "/")[[1]][1]),
             fixture_date < Sys.Date()) %>%
      # add a variable for the result
      mutate(result_fixture = fulltime_score_home-fulltime_score_away) %>%
      # arrange them based on the date
      count(result_fixture<0)
    
    if (nrow(wins_as_away[wins_as_away$`result_fixture < 0` == TRUE, ])==0) {
      wins_as_away = 0
    } else{
      wins_as_away <-
        as.numeric(wins_as_away[wins_as_away$`result_fixture < 0` == TRUE, ]$n)
      
    }
    
    valueBox(
      wins_as_away + wins_as_home,
      "Aggregate Wins - Team 1",
      icon = icon("flag"),
      color = "purple",
      width = 3
    )
  })
  
  output$Aggregate_wins_2 <- renderValueBox({
    # there has to be a club selected
    req(input$info_match_league)
    req(input$info_match_season)
    req(input$info_match_team1)
    req(input$info_match_team2)
    
    # filter the all leagues matches table 
    wins_as_home_2 <- all_leagues_matches %>%
      # for the club names
      filter(club_name_home == input$info_match_team2& club_name_away == input$info_match_team1,
             # the selected season (smaller or equal than the current season, i.e.,
             # all matches of these two teams in the past)
             league_season <= as.numeric(
               str_split(input$info_match_season,
                         pattern = "/")[[1]][1]),
             fixture_date < Sys.Date()) %>%
      # add a variable for the result
      mutate(result_fixture = fulltime_score_home-fulltime_score_away) %>%
      # arrange them based on the date
      count(result_fixture>0) 
    
    # if the wins_as_home has no TRUE row it means team had no wins 
    if (nrow(wins_as_home_2[wins_as_home_2$`result_fixture > 0` == TRUE, ])==0) {
      wins_as_home_2 = 0
    } else{
      wins_as_home_2 <-
        as.numeric(wins_as_home_2[wins_as_home_2$`result_fixture > 0` == TRUE, ]$n)
      
    }
    
    wins_as_away_2 <- all_leagues_matches %>%
      # for the club names
      filter(club_name_away == input$info_match_team2& club_name_home == input$info_match_team1,
             # the selected season (smaller or equal than the current season, i.e.,
             # all matches of these two teams in the past)
             league_season <= as.numeric(
               str_split(input$info_match_season,
                         pattern = "/")[[1]][1]),
             fixture_date < Sys.Date()) %>%
      mutate(result_fixture = fulltime_score_home-fulltime_score_away) %>%
      count(result_fixture<0)  # count the wins 
    
    if (nrow(wins_as_away_2[wins_as_away_2$`result_fixture < 0` == TRUE, ])==0) {
      wins_as_away_2 = 0
    } else{
      wins_as_away_2 <-
        as.numeric(wins_as_away_2[wins_as_away_2$`result_fixture < 0` == TRUE, ]$n)
      
    }

    valueBox(
      wins_as_home_2 + wins_as_away_2,
      "Aggregate Wins - Team 2",
      icon = icon("flag"),
      color = "orange",
      width = 3
    )
  })
  
  
  
  
  
  # outputs a table with past matches of the two selected teams
  # in the current or previous seasons (also selected by the user)
  output$info_league_match_h2h <- renderReactable({
    # wait for the input of the two teams
    req(input$info_match_team1)
    req(input$info_match_team2)
    req(input$info_match_season)
    
    # season <- 2021
    # team1 <- "Borussia Monchengladbach"
    # team2 <- "FC Bayern Munich"
    # season_half_selection <- "First half"
    
    
    # filter the all leagues matches table 
    all_leagues_matches %>%
      # for the club names
      filter(club_name_home %in% c(input$info_match_team1,
                                   input$info_match_team2),
             club_name_away %in% c(input$info_match_team1,
                                   input$info_match_team2),
             # the selected season (smaller or equal than the current season, i.e.,
             # all matches of these two teams in the past)
             league_season <= as.numeric(
               str_split(input$info_match_season,
                         pattern = "/")[[1]][1]),
             fixture_date < Sys.Date()) %>%
      # add a variable for the result
      mutate(result_fixture = paste0(fulltime_score_home, ":", fulltime_score_away)) %>%
      # select only the important variables
      select(fixture_date, league_season, 
             league_round, club_name_home, result_fixture, club_name_away) %>%
      # arrange them based on the date
      arrange(desc(fixture_date)) %>%
      # create a table with that input
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