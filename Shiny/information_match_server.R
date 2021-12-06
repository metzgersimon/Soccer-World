# subserver for the match-tab in the information menu item
information_match_server <- function(input, output, session){

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
             # take only those rows where the clubs match
             club_name_home %in% c(input$info_match_team1,
                                   input$info_match_team2),
             club_name_away %in% c(input$info_match_team1,
                                   input$info_match_team2))
    
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
      select(fixture_date, fixture_time, venue_name, venue_city, league_round,
             referee, club_name_home, club_name_away, team_name, halftime_score_home,
             halftime_score_away, fulltime_score_home, fulltime_score_away,
             shots_on_goal, shots_off_goal, shots_total, shots_blocked, 
             shots_inside_box, shots_outside_box, fouls, corners, offsides,
             ball_possession, cards_yellow, cards_red, goalkeeper_saves,
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
      cols = shots_on_goal:passing_accuracy,
      names_to = "statistic",
      values_to = c("values")) %>%
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
    # team2 <- "FC Bayern Munich"
    # season_half_selection <- "First half"
    
    # convert the season input into a number
    season <- as.numeric(str_split(input$info_match_season,
                                    pattern = "/")[[1]][1])
    
    season_half_selection <- input$info_match_season_half
    
    # get the data for the year
    current_season_lineups <- get(paste0("lineups_messy_", season))
    
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
    for(matchday in important_matchdays){
      # print for debugging
      print(paste0("Matchday ", matchday))
      # select the matchday for the given iteration
      curr_matchday <- current_season_lineups[[matchday]]
      
      # iterate over all matches available in the matchday
      for(j in 1:length(curr_matchday)){
        # select the current match
        curr_match <- curr_matchday[[j]]
        # print for debugging
        print(paste0("Match ", j))
        
        # check if the teams given via the user input match
        # with the current given team names in the iteration
        if(input$info_match_team1 %in% curr_match$team_names &
           input$info_match_team2 %in% curr_match$team_names){
          
          # if so, get the match data because it is the right match
          selected_match_data <- curr_match
          # set the variable to TRUE
          loop_ended <- TRUE
          # and break the inner loop
          break
        }
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
    starting_lineups <- selected_match_data$starting_lineups
    
    # and split them into home and away team
    starting_lineups_home <- select(starting_lineups,
                                    player_name_home,
                                    player_position_home) %>%
      # add a variable for team name
      mutate(team_name = selected_match_data$team_names[1])
    
    # set the column names appropriately
    colnames(starting_lineups_home) <- c("player_name", "player_position",
                                         "team_name")
    
    starting_lineups_away <- select(starting_lineups,
                                    player_name_away,
                                    player_position_away) %>%
      # add a variable for team name
      mutate(team_name = selected_match_data$team_names[2])
    
    # set the column names appropriately
    colnames(starting_lineups_away) <- c("player_name", "player_position",
                                         "team_name")
    
    # starting_lineups <- bind_rows(starting_lineups_home,
    #                               starting_lineups_away)
    
    # return the data as a list (home as first element
    # and away as second element)
    return(list(starting_lineups_home,
                starting_lineups_away)
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
    
    # otherwise, construct the plot
    # for the first barplot (team 1),
    # we have to convert the values to negative values to show them on
    # the left side of the zero point
    barplot_team_1 <- plot_ly(fixture_plot_data,
                              x = ~ values_team_1_rel * (-1),
                              y = ~ statistic,
                              type = "bar",
                              orientation = "h",
                              name = ~ club_name_home,
                              text = ~ team_1,
                              textposition = 'outside')
    
    # for the barplot of team 2, we just take the values as they are
    barplot_team_2 <- plot_ly(fixture_plot_data,
                              x = ~ values_team_2_rel,
                              y = ~ statistic,
                              type = "bar",
                              orientation = "h",
                              name = ~ club_name_away,
                              text = ~ team_2,
                              textposition = 'outside') 
    
    
    # create the two barplots as subplots into one plot
    # let them share the y-axis and set the margin to 0.
    # So, there is no space between the 0 point
    subplot(barplot_team_1, barplot_team_2,
            shareY = TRUE,
            margin = 0) %>%
      # set them to grouped and remove the axis titles because we don't need them
      layout(barmode = 'grouped',
             xaxis = list(title = "",
                          showticklabels = FALSE),
             xaxis2 = list(title = "",
                           showticklabels = FALSE),
             yaxis = list(title =""))
  })
  
  
  # creates the table for the home match lineups for a selected match
  output$info_match_match_lineups_home <- renderReactable({
    # wait until the input of the lineups_reactive function is given
    req(lineups_reactive())
    
    # get the data
    lineups <- lineups_reactive()
    
    # if there is no data, do nothing (plot remains empty)
    if(length(lineups) == 0){
      return()
    }
    
    # extract the data for the home lineups
    starting_lineups_home <- lineups[[1]]
    
    # create the actual reactable (drop the team names)
    reactable(starting_lineups_home[, -3],
              defaultPageSize = 11,
              # set general options for the table
              # such as the possibility to filter or sort the table
              # but also insert a search field
              sortable = TRUE,
              filterable = TRUE,
              searchable = TRUE,
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
                player_position = colDef(name = "Position",
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
    reactable(starting_lineups_away[, -3],
              defaultPageSize = 11,
              # set general options for the table
              # such as the possibility to filter or sort the table
              # but also insert a search field
              sortable = TRUE,
              filterable = TRUE,
              searchable = TRUE,
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
                player_position = colDef(name = "Position",
                                         align = "center"))
    )
  
    
  })
  
}