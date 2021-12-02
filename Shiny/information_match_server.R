# subserver for the match-tab in the information menu item
information_match_server <- function(input, output, session){
  
  # create a reactive value to store the current selected
  # home team and the current selected away team
  # selected_home_team <- reactiveVal()
  # selected_away_team <- reactiveVal()
  
  
  
  # create an observer to display for the club selection (home team) to display
  # only those clubs that are present for the selected season
  observeEvent(input$info_match_season, {
    updateSelectInput(session, 
                      inputId = "info_match_home_team",
                      choices = c("",
                                  unique(fixtures_bundesliga_2010_2021 %>%
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
                      inputId = "info_match_away_team",
                      choices = c("",
                                  unique(fixtures_bundesliga_2010_2021 %>%
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
  
  # create an observer to display for the away team selection just
  # all clubs that are not the same as the club selected as home team
  # observe({#input$info_match_home_team, {
  #   updateSelectInput(session,
  #                     inputId = "info_match_away_team",
  #                     choices = unique(fixtures_bundesliga_2010_2021 %>%
  #                                          filter(league_season ==
  #                                                   as.numeric(
  #                                                     str_split(input$info_match_season,
  #                                                               pattern = "/")[[1]][1]),
  #                                                 club_name_away != input$info_match_home_team
  #                                          ) %>%
  #                                          select(club_name_away) %>%
  #                                          unlist() %>%
  #                                          unname()
  #                     )
  #   )
  # })


  # create an observer to display for the home team selection just
  # all clubs that are not the same as the club selected as away team
  # observe({#input$info_match_away_team, {
  #   updateSelectInput(session,
  #                     inputId = "info_match_home_team",
  #                     choices = unique(fixtures_bundesliga_2010_2021 %>%
  #                                          filter(league_season ==
  #                                                   as.numeric(
  #                                                     str_split(input$info_match_season,
  #                                                               pattern = "/")[[1]][1]),
  #                                                 club_name_home != input$info_match_away_team
  #                                          ) %>%
  #                                          select(club_name_home) %>%
  #                                          unlist() %>%
  #                                          unname()
  #                     )
  #   )
  # })
  
  # reactive function (is executed every time something changes)
  # that returns the data frame which is needed for the match statistics
  # plot
  matches_reactive <- reactive({
    req(input$info_match_season)
    req(input$info_match_home_team)
    req(input$info_match_away_team)
    
    season_half_selection <- input$info_match_season_half
    
    number_of_rounds <- max(fixtures_bundesliga_2010_2021$league_round,
                            na.rm = TRUE)
    
    rounds_segment <- number_of_rounds / 2
    
    
    fixture_stats <- fixtures_with_stats_2021 %>%
      filter(league_season == 
               as.numeric(
                 str_split(input$info_match_season,
                           pattern = "/")[[1]][1]),
             club_name_home %in% c(input$info_match_home_team,
                                   input$info_match_away_team),
             club_name_away %in% c(input$info_match_home_team,
                                   input$info_match_away_team))
    
    if(season_half_selection == "First half"){
      fixture_stats <- fixture_stats %>%
        filter(league_round <= rounds_segment)
    } else {
      fixture_stats <- fixture_stats %>%
        filter(league_round > rounds_segment)
    }
    
    print("HAHA")
    if(nrow(fixture_stats) == 0){
      print("I AM IN HERE")
      shinyalert(title = "Error: no data avilable for your selection!",
                 text = paste0("Probably the match for the selected teams is not ",
                               "yet played in the given season half."),
                 type = "error"
                 )
      return(fixture_stats)
    }

    fixture_stats <- fixture_stats %>%
      select(fixture_date, fixture_time, venue_name, venue_city, league_round,
             referee, club_name_home, club_name_away, team_name, halftime_score_home,
             halftime_score_away, fulltime_score_home, fulltime_score_away,
             shots_on_goal, shots_off_goal, shots_total, shots_blocked, 
             shots_inside_box, shots_outside_box, fouls, corners, offsides,
             ball_possession, cards_yellow, cards_red, goalkeeper_saves,
             passes_total, passes_accurate, passing_accuracy) %>%
      mutate(ball_possession = str_remove(ball_possession, pattern = "%"),
             passing_accuracy = str_remove(passing_accuracy, pattern = "%")) %>%
      mutate(across(c("shots_on_goal", "shots_off_goal", "shots_total",
                      "shots_blocked", "shots_inside_box", "shots_outside_box",
                      "fouls", "corners", "offsides", "ball_possession",
                      "cards_yellow", "cards_red", "goalkeeper_saves",
                      "passes_total", "passes_accurate", "passing_accuracy"),
                    as.numeric)) %>%
      replace(is.na(.), 0)
    
    club_name_home <- fixture_stats$club_name_home[1]
    club_name_away <- fixture_stats$club_name_away[1]
    
    
    fixture_plot_data <- pivot_longer(
      fixture_stats, 
      cols = shots_on_goal:passing_accuracy,
      names_to = "statistic",
      values_to = c("values")) %>%
      group_by(team_name) %>%
      spread(key = team_name,
             value = values) %>%
      rename(team_1 := !!club_name_home,
             team_2 := !!club_name_away) %>%
      mutate(values_team_1_rel = team_1 / (team_1 + team_2),
             values_team_2_rel = team_2 / (team_1 + team_2)) %>%
      replace(is.na(.), 0)
    
    
    return(fixture_plot_data)
    
  })
    
  # creates the plot for the match statistic for a selected match
  output$info_match_match_stats <- renderPlotly({
    req(matches_reactive)
    
    fixture_plot_data <- matches_reactive()
    
    if(nrow(fixture_plot_data) == 0){
      print("I AM IN HERE 2")
      return()
    }
    
    barplot_team_1 <- plot_ly(fixture_plot_data,
                              x = ~ values_team_1_rel * (-1),
                              y = ~ statistic,
                              type = "bar",
                              orientation = "h",
                              name = ~ club_name_home,
                              text = ~ team_1,
                              textposition = 'outside')
    
    barplot_team_2 <- plot_ly(fixture_plot_data,
                              x = ~ values_team_2_rel,
                              y = ~ statistic,
                              type = "bar",
                              orientation = "h",
                              name = ~ club_name_away,
                              text = ~ team_2,
                              textposition = 'outside') 
    
    
    subplot(barplot_team_1, barplot_team_2,
            shareY = TRUE,
            margin = 0) %>%
      layout(barmode = 'grouped',
             xaxis = list(title = "",
                          showticklabels = FALSE),
             xaxis2 = list(title = "",
                           showticklabels = FALSE),
             yaxis = list(title =""))
  })
  
}