# subserver for the team-tab in the information menu item
information_team_server <- function(input, output, session) {
  
  # update the select inputs
  observeEvent(input$info_team_league_selection, {
    updateSelectizeInput(session,
                      inputId = "info_team_club_selection",
                      choices = c(
                        "",
                        unique(
                          all_infos_club %>% filter(league == input$info_team_league_selection) %>% select(club) %>%
                            unlist() %>%
                            unname()
                        )
                      ))
  })
  
  # create an observer to display for the season selection
  # only those seasons that are present for the selected club
  # paste("Hello", "world", sep=" ")
  observeEvent(input$info_team_club_selection, {
    updateSelectizeInput(
      session,
      inputId = "info_team_season_selection",
      selected = NULL,
      choices = c("",
                  paste0(
                    unique(
                      all_infos_club %>% filter(league == input$info_team_league_selection,
                                                  club == input$info_team_club_selection) %>%
                        select(season) %>%
                        unlist() %>%
                        unname()
                    )
                    ,
                    "/",
                    unique(
                      all_infos_club %>% filter(league == input$info_team_league_selection,
                                                  club == input$info_team_club_selection) %>%
                        select(season) %>%
                        unlist() %>%
                        unname()
                    ) + 1
                  ))
    )
  })
  
  filter_team_data <- reactive({
  # to gather venue infos
  # <- all_infos_club %>% left_join(all_leagues_venue_information[,c(4,8,10:15)], by=c("club"="team_name"))
  
  # there has to be a club selected
  req(input$info_team_league_selection)
  req(input$info_team_club_selection)
  req(input$info_team_season_selection)
  
  # convert the season in a format we can work with
  season_year <-
    as.numeric(str_split(input$info_team_season_selection,
                         pattern = "/")[[1]][1])
  
  # filter the huge data frame on the club selected
 all_infos_club %>%
    filter(league ==  input$info_team_league_selection,
           club == input$info_team_club_selection,
           season == season_year)
  
  
  })
  
  output$info_team_team_age <- renderValueBox({
    # there has to be a club selected
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      filter_team_data() %>%
        # only unique player_names
        distinct(player_name, .keep_all = TRUE) %>%
        select(player_age) %>%
        summarize(avg_age = mean(player_age, na.rm = TRUE)) %>%
        # extract the value (it is a data frame)
        pull() %>%
        # round the value
        round(digits = 0),
      "Average age",
      icon = icon("user"),
      color = "light-blue",
      width = 3
    )
  })
  
  output$info_team_team_height <- renderValueBox({
    # there has to be a club selected
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      filter_team_data() %>%
        # only unique player_names
        distinct(player_name, .keep_all = TRUE) %>%
        select(player_height) %>%
        summarize(avg_age = mean(player_height, na.rm = TRUE)) %>%
        # extract the value (it is a data frame)
        pull() %>%
        # round the value
        round(digits = 2),
      "Average height",
      icon = icon("arrow-up"),
      color = "purple",
      width = 3
    )
  })
  
  
  
  output$info_team_team_size <- renderValueBox({
    # there has to be a club selected
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      filter_team_data() %>%
        select(player_name) %>%
        unique() %>%
        nrow(),
      "Squad size",
      icon = icon("flag"),
      color = "navy",
      width = 3
    )
  })
  

  output$info_team_team_venue <- renderValueBox({
    # there has to be a club selected
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      all_leagues_venue_information %>% mutate(league = if_else(
        league_id == 78,
        "Bundesliga",
        if_else(
          league_id == 79,
          "Bundesliga 2",
          if_else(
            league_id == 39,
            "Premier League",
            if_else(league_id == 61,
                    "Ligue 1",
                    "none")
          )
        )
      )) %>% filter(
        league ==  input$info_team_league_selection,
        team_name == input$info_team_club_selection,
        season ==  as.numeric(
          str_split(input$info_team_season_selection,
                    pattern = "/")[[1]][1]
        )
      ) %>%
        select(venue_capacity) %>%
        unique() %>% unlist() %>% str_extract(., pattern = "[0-9]+.*") %>% as.numeric()  ,
      "Venue capacity",
      icon = icon("sistrix"),
      color = "blue",
      width = 3
    )
  })
  
  output$info_team_team_players <- renderValueBox({
    # there has to be a club selected
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      filter_team_data() %>%
        # only unique player_names
        distinct(player_name, .keep_all = TRUE) %>%
        count() %>% pull(),
      "Players number",
      icon = icon("fax"),
      color = "navy",
      width = 3
    )
  })
  
  output$info_team_team_rightfoot <- renderValueBox({
    # there has to be a club selected
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      filter_team_data() %>%
        # only unique player_names
        filter(player_foot=="right") %>%
        count() %>% pull(),
      "Right foot players ",
      icon = icon("arrow-right"),
      color = "blue",
      width = 3
    )
  })
  
  output$info_team_team_leftfoot <- renderValueBox({
    # there has to be a club selected
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      filter_team_data() %>%
        # only unique player_names
        filter(player_foot=="left") %>%
        count() %>% pull(),
      "Left foot players",
      icon = icon("arrow-left"),
      color = "light-blue",
      width = 3
    )
  })
  
  output$info_team_team_german <- renderValueBox({
    
    # there has to be a club selected
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      filter_team_data() %>%
        # only unique player_names
        filter(player_nationality=="Germany") %>%
        count() %>% pull(),
      "German players",
      icon = icon("eye"),
      color = "purple",
      width = 3
    )
  })
  
  # output for the club logo
  output$info_team_team_logo <- renderUI({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    
    print(input$info_team_club_selection)
    # extract the logo from the frame
    team_image <- all_leagues_venue_information %>% mutate(league = if_else(
      league_id == 78,
      "Bundesliga",
      if_else(
        league_id == 79,
        "Bundesliga 2",
        if_else(
          league_id == 39,
          "Premier League",
          if_else(
            league_id == 61,
            "Ligue 1",
            "none"
          )
        )
      )
    )) %>%
      filter(
        league == input$info_team_league_selection &
          team_name == input$info_team_club_selection
      ) %>%
      select(logo) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    # set the img on the extracted logo
    tags$img(src = team_image)
  })
  
  
  # output for the club logo
  output$info_team_venue_image <- renderUI({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    
    print(input$info_team_club_selection)
    # extract the logo from the frame
    venue_image <- all_leagues_venue_information %>% mutate(league = if_else(
      league_id == 78,
      "Bundesliga",
      if_else(
        league_id == 79,
        "Bundesliga 2",
        if_else(
          league_id == 39,
          "Premier League",
          if_else(
            league_id == 61,
            "Ligue 1",
            "none"
          )
        )
      )
    )) %>%
      filter(
        league ==  input$info_team_league_selection &
          team_name == input$info_team_club_selection
      ) %>%
      select(venue_image) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    # set the img on the extracted logo
    tags$img(src = venue_image)
  })
  
  ################# Match & Stats Page ######################
  # create the table for the next match of the selected
  # club
  output$info_team_next_match <- renderReactable({
    # we need the user to select a club and a season first
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    selected_club <- input$info_team_club_selection
    selected_season <-  as.numeric(str_split(
      input$info_team_season_selection,
      pattern = "/")[[1]][1]
    )

    
    if (input$info_team_league_selection == "Bundesliga") {
      league_id <- 78
    } else if (input$info_team_league_selection == "Bundesliga 2") {
      league_id <- 79
    } else if (input$info_team_league_selection == "Premier League") {
      league_id <- 39
    } else if (input$info_team_league_selection == "Ligue 1") {
      league_id <- 61
    }
    
    
     get_next_fixture_by_team(league_id = league_id,
                              team_name = selected_club,
                              season = selected_season) %>%
    # only select important columns
      select(c(fixture_date, fixture_time,
               league_round, venue_city,
               club_name_home, club_name_away)) %>%
      reactable(defaultColDef = colDef(
        align = "center",
        minWidth = 150,
        headerStyle = list(background = "darkblue")
      ), borderless = TRUE,
                #searchable = TRUE,
                striped = TRUE,
                highlight = TRUE,
                # set the theme for the table
      theme = reactableTheme(
        borderColor = "#BFC9D1",
        color = "#BFC9D1",
        backgroundColor = "#567895",
        highlightColor = "#CE8B65",
        cellPadding = "8px 12px",
        style = list(color = "white"),
        searchInputStyle = list(width = "100%")
      ), 
                # modify the layout and names of the columns
                columns = list(
                  fixture_date = colDef(name = "Date",
                                       align = "left"),
                  fixture_time = colDef(name = "Time",
                                           align = "center"),
                  venue_city = colDef(name = "City",
                                        align = "center"),
                  league_round = colDef(name = "Matchday",
                                        align = "center"),
                  club_name_home = colDef(name = "Home Club",
                                        align = "center"),
                  club_name_away = colDef(name = "Away Club",
                                        align = "center")
                ))
   
  })
  
  # create the table for the season of the selected
  # club
  output$info_team_season <- renderReactable({
    # we need the user to select a club first 
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)    
    
    selected_club <- input$info_team_club_selection
    # convert the season in a number one can work with
    selected_season <- as.numeric(str_split(
      input$info_team_season_selection,
      pattern = "/")[[1]][1]
    )
      
    # filter the data for the clubs and the season
    all_leagues_matches %>%
      filter(club_name_home == selected_club |
               club_name_away == selected_club,
             league_season == selected_season, 
             league_name == input$info_team_league_selection) %>%
      mutate(game_score = paste0(fulltime_score_home, ":", 
                                 fulltime_score_away)) %>%
      select(fixture_date, fixture_time, club_name_home,
             game_score,club_name_away) %>%
      arrange(fixture_date, fixture_time) %>%
      # create the actual table
      reactable(defaultColDef = colDef(
        align = "center",
        minWidth = 150,
        headerStyle = list(background = "darkblue")
      ),
        # set general options for the table
        # such as the possibility to filter or sort the table
        # but also insert a search field
        sortable = TRUE,
        searchable = TRUE,
        highlight = TRUE,
        borderless = TRUE,
        striped = TRUE,
        # set the theme for the table
      theme = reactableTheme(
        borderColor = "#BFC9D1",
        color = "#BFC9D1",
        backgroundColor = "#567895",
        highlightColor = "#CE8B65",
        cellPadding = "8px 12px",
        style = list(color = "white"),
        searchInputStyle = list(width = "100%")
      ), 
        # modify the layout and names of the columns
        columns = list(
          fixture_date = colDef(name = "Date",
                                align = "left"),
          fixture_time = colDef(name = "Time",
                                align = "center"),
          club_name_home = colDef(name = "Home Club",
                                  align = "center"),
          game_score = colDef(name = "Result",
                              align = "center"),
          club_name_away = colDef(name = "Away Club",
                                  align = "center")
          
        ))
  })

  ###################### stats begins
  stats_select <- reactive ({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    all_leagues_club_stats %>%
      filter(
        league_name == input$info_team_league_selection,
        team_name == input$info_team_club_selection,
        league_season == as.numeric(
          str_split(input$info_team_season_selection,
                    pattern = "/")[[1]][1]
        )
      ) %>% filter(matchday == max(matchday, na.rm = TRUE))
  }) 
 
  # output of the valuebox-total plays
  output$total_played <- renderValueBox({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      value =  stats_select() %>% select(fixtures_played_total)  %>% unlist() %>% str_extract(., pattern = "[0-9]+.*") %>% as.numeric()%>% unique() %>% .[1],
      "Total Plays",
      color = "purple",
      icon = icon("hourglass-half"),
      width = 3
    )
    
  })
  
  # output of the valuebox-total wins
  output$total_wins <- renderValueBox({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      value = stats_select() %>% select(fixtures_wins_total)  %>% unlist() %>% str_extract(., pattern = "[0-9]+.*") %>% as.numeric()%>% unique() %>% .[1],
      "Total Wins",
      color = "light-blue",
      icon = icon("flag"),
      width = 3
    )
  })
  
  # output of the valuebox-total draws
  output$total_draws <- renderValueBox({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      value = stats_select() %>% select(fixtures_draws_total)  %>% unlist() %>% str_extract(., pattern = "[0-9]+.*") %>% as.numeric()%>% unique() %>% .[1],
      "Total Draws",
      color = "blue",
      icon = icon("equals"),
      width = 3)
  })
  
  # output of the valuebox-total loses
  output$total_loses <- renderValueBox({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection) 
    
    valueBox(
      value = stats_select() %>% select(fixtures_loses_total)  %>% unlist() %>% str_extract(., pattern = "[0-9]+.*") %>% as.numeric() %>% unique() %>% .[1],
      "Total Loses",
      color = "navy",
      icon = icon("bomb"),
      width = 3)
  })
  
  # output of the valuebox-total for goals
  output$total_for_goals <- renderValueBox({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      value = stats_select() %>% select(goals_for_total_total)  %>% unlist() %>% str_extract(., pattern = "[0-9]+.*") %>% as.numeric() %>% unique() %>% .[1],
      "Total For Goals",
      color = "light-blue",
      icon = icon("award"),
      width = 3)
  })

  # output of the valuebox-total against goals
  output$total_against_goals <- renderValueBox({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    valueBox(
      value = stats_select() %>% select(goals_against_total_total)  %>% unlist() %>% str_extract(., pattern = "[0-9]+.*") %>% as.numeric()%>% unique() %>% .[1],
      "Total Against Goals",
      color = "blue",
      icon = icon("futbol"),
      width = 3)
  })
  
  # output of the valuebox-total failed score
  output$total_failed_score <- renderValueBox({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
   
    valueBox(
      value = stats_select() %>% select(failed_to_score_total)  %>% unlist() %>% str_extract(., pattern = "[0-9]+.*") %>% as.numeric()%>% unique() %>% .[1],
      "Total Failed to Score",
      color = "purple",
      icon = icon("ban"),
      width = 3)
  })
  
  # output of the valuebox-total penalty
  output$total_penalty <- renderValueBox({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
   
    valueBox(
      value = stats_select() %>% select(penalty_total)  %>% unlist() %>% str_extract(., pattern = "[0-9]+.*") %>% as.numeric()%>% unique() %>% .[1],
      "Total Penalty",
      color = "navy",
      icon = icon("exclamation"),
      width = 3)
  })
  
  
  # more details stats
  output$info_team_stats <- renderReactable({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    # select the data
   data <-
     stats_select()  %>% select(biggest_streak_wins:biggest_streak_loses) %>% unique() %>% .[1,]
    
    # make the table
    data %>%  
      reactable(
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(background = "darkblue")
        ),
        striped = TRUE,
        highlight = TRUE,
        borderless = TRUE,
        pagination = FALSE,
        defaultPageSize = 15,
        showPageInfo = FALSE,
        showPagination = FALSE,
        # set the theme for the table
        theme = reactableTheme(
          borderColor = "#BFC9D1",
          color = "#BFC9D1",
          backgroundColor = "#567895",
          highlightColor = "#CE8B65",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%")
        ) ,       columns = list(
          biggest_streak_wins = colDef(name = "wins",
                                  align = "left"),
          biggest_streak_draws = colDef(name = "draws",
                                  align = "center"),
          biggest_streak_loses = colDef(name = "loses",
                                   align = "center")
                  
      ))
  
    
  })
  
  # more details stats
  output$info_team_home <- renderReactable({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    data <-
      stats_select()  %>% select(contains("home_diff"), clean_sheet_home) %>% unique()%>% .[1,]
    
    
    data %>%  
      reactable(
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(background = "darkblue")
        ),
        striped = TRUE,
        highlight = TRUE,
        borderless = TRUE,
        pagination = FALSE,
        defaultPageSize = 15,
        showPageInfo = FALSE,
        showPagination = FALSE,
        # set the theme for the table
        theme = reactableTheme(
          borderColor = "#BFC9D1",
          color = "#BFC9D1",
          backgroundColor = "#567895",
          highlightColor = "#CE8B65",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%")
        ) ,       columns = list(
          biggest_wins_home_diff  = colDef(name = "wins",
                                       align = "left"),
          biggest_loses_home_diff  = colDef(name = "loses",
                                        align = "center"),
          clean_sheet_home = colDef(name = "clean sheet",
                                        align = "center")
          
        ))
    
    
  })
  
  # more details stats
  output$info_team_away <- renderReactable({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    data <-
      stats_select()  %>% select(contains("away_diff"), clean_sheet_away) %>% unique()%>% .[1,]
    
    
    data %>%  
      reactable(
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(background = "darkblue")
        ),
        striped = TRUE,
        highlight = TRUE,
        borderless = TRUE,
        pagination = FALSE,
        defaultPageSize = 15,
        showPageInfo = FALSE,
        showPagination = FALSE,
        # set the theme for the table
        theme = reactableTheme(
          borderColor = "#BFC9D1",
          color = "#BFC9D1",
          backgroundColor = "#567895",
          highlightColor = "#CE8B65",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%")
        ) ,       columns = list(
          biggest_wins_away_diff   = colDef(name = "wins",
                                           align = "left"),
          biggest_loses_away_diff   = colDef(name = "loses",
                                            align = "center"),
          clean_sheet_away = colDef(name = "clean sheet",
                                    align = "center")
          
        ))
    
    
  })
  
  # time series plot for team as home team stats 
  output$ts_home_stats <- renderPlotly({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    data <- reactive({
      all_leagues_club_stats %>%
        filter(
          league_name == input$info_team_league_selection,
          team_name == input$info_team_club_selection,
          league_season == as.numeric(
            str_split(input$info_team_season_selection,
                      pattern = "/")[[1]][1]
          )
        )

    })
    
    plot_data <- data() %>%
      pivot_longer(
        contains("home"),
        names_to = "variable",
        values_to = "value"
      )
    
    ts_home <- plot_data %>% 
      plot_ly(
        x = ~ matchday,
        y = ~ value,
        color =  ~ variable,
        type = "scatter"
        # visible = "legendonly"
      ) %>%
      layout(
        title = list(text = "stats as home team over matchday", y = 0.95, x = 0.5),
        yaxis = list(title = "Value"),
        xaxis = list(title = "Matchday"),
        font = list(color = "white"),
        plot_bgcolor = "#567895",
        paper_bgcolor = "#567895",
        fig_bg_color = "#567895"
      )
    
    ts_home
  })
  
  # time series plot for team as away team stats
  output$ts_away_stats <- renderPlotly({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    data <- reactive({
      all_leagues_club_stats %>%
        filter(
          league_name == input$info_team_league_selection,
          team_name == input$info_team_club_selection,
          league_season == as.numeric(
            str_split(input$info_team_season_selection,
                      pattern = "/")[[1]][1]
          )
        )
      
    })
    
    plot_data <- data() %>%
      pivot_longer(
        contains("away"),
        names_to = "variable",
        values_to = "value"
      )
    
    ts_away <- plot_data %>% 
      plot_ly(
        x = ~ matchday,
        y = ~ value,
        color =  ~ variable,
        type = "scatter"
        # visible = "legendonly"
      ) %>%
      layout(
        title = list(text = "stats as away team over matchday", y = 0.95, x = 0.5),
        yaxis = list(title = "Value"),
        xaxis = list(title = "Matchday"),
        font = list(color = "white"),
        plot_bgcolor = "#567895",
        paper_bgcolor = "#567895",
        fig_bg_color = "#567895"
      )
    
    ts_away
  })
  
  
############## squads infos #########################
  
  squad_reactive <- reactive({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    if(input$info_team_league_selection=="Bundesliga 1"){
      league <- "Bundesliga"
    } else {league <- input$info_team_league_selection}
    
    squads_filtered <-
      all_leagues_squads_tm %>%
      filter(
        club == input$info_team_club_selection &
          season == as.numeric(
            str_split(input$info_team_season_selection,
                      pattern = "/")[[1]][1]) &
          league == league
      )
    
  })
  
  # function to filter the data for the selected area
  # e.g., defense, order it and return it 
  get_data_for_reactables <- function(filter_data, position_area){
    
    squad_for_positions <- filter_data %>%
      filter(str_detect(player_position, position_area)) %>%
      select(player_name, player_nationality, player_position, player_age,
             player_market_value_in_million_euro) %>%
      arrange(desc(player_position), 
              desc(player_market_value_in_million_euro))
    
    return(squad_for_positions)
    
  }
  
  # attack table
  output$info_team_squad_attack <- renderReactable({
    
    midfield_squad <- get_data_for_reactables(squad_reactive(), "Striker|Winger|Forward")
    
    reactable(midfield_squad,
              columns = list(
                player_name = colDef(name = "Name"),
                player_nationality = colDef(name = "Nationality"),
                player_position = colDef(name = "Position"),
                player_age = colDef(name = "Age"),
                player_market_value_in_million_euro = colDef(name = "Market value",
                                                      format = colFormat(
                                                        prefix = "\u20ac",
                                                        suffix = "M"
                                                      )
                )
              ),
              defaultPageSize = 5,
              sortable = TRUE,
              searchable = TRUE,
              highlight = TRUE,
              borderless = TRUE, 
              # set the theme for the table
              theme = reactableTheme(
                borderColor = "#BFC9D1",
                color = "#BFC9D1",
                backgroundColor = "#567895",
                highlightColor = "#CE8B65",
                cellPadding = "8px 12px",
                style = list(color = "white"),
                searchInputStyle = list(width = "100%")
              )
    )
    
  })
  
  # midfield table
  output$info_team_squad_midfield <- renderReactable({
    midfield_squad <- get_data_for_reactables(squad_reactive(), "Midfield")
    
    reactable(midfield_squad,
              columns = list(
                player_name = colDef(name = "Name"),
                player_nationality = colDef(name = "Nationality"),
                player_position = colDef(name = "Position"),
                player_age = colDef(name = "Age"),
                player_market_value_in_million_euro = colDef(name = "Market value",
                                                      format = colFormat(
                                                        prefix = "\u20ac",
                                                        suffix = "M"
                                                      )
                )
              ),
              defaultPageSize = 5,
              sortable = TRUE,
              searchable = TRUE,
              highlight = TRUE,
              borderless = TRUE, 
              # set the theme for the table
              theme = reactableTheme(
                borderColor = "#BFC9D1",
                color = "#BFC9D1",
                backgroundColor = "#567895",
                highlightColor = "#CE8B65",
                cellPadding = "8px 12px",
                style = list(color = "white"),
                searchInputStyle = list(width = "100%")
              )
    )
    
  })
  
  # defense table
  output$info_team_squad_defense <- renderReactable({
    defense_squad <- get_data_for_reactables(squad_reactive(), "Back")
    
    reactable(defense_squad,
              columns = list(
                player_name = colDef(name = "Name"),
                player_nationality = colDef(name = "Nationality"),
                player_position = colDef(name = "Position"),
                player_age = colDef(name = "Age"),
                player_market_value_in_million_euro = colDef(name = "Market value",
                                                      format = colFormat(
                                                        prefix = "\u20ac",
                                                        suffix = "M"
                                                      )
                )
              ),
              defaultPageSize = 5,
              sortable = TRUE,
              searchable = TRUE,
              highlight = TRUE,
              borderless = TRUE, 
              # set the theme for the table
              theme = reactableTheme(
                borderColor = "#BFC9D1",
                color = "#BFC9D1",
                backgroundColor = "#567895",
                highlightColor = "#CE8B65",
                cellPadding = "8px 12px",
                style = list(color = "white"),
                searchInputStyle = list(width = "100%")
              )
    )
    
  })
  
  # goalkeeper table
  output$info_team_squad_goalkeepers <- renderReactable({
    keeper_squad <- get_data_for_reactables(squad_reactive(), "Goalkeeper")
    
    reactable(keeper_squad,
              columns = list(
                player_name = colDef(name = "Name"),
                player_nationality = colDef(name = "Nationality"),
                player_position = colDef(name = "Position"),
                player_age = colDef(name = "Age"),
                player_market_value_in_million_euro = colDef(name = "Market value",
                                                      format = colFormat(
                                                        prefix = "\u20ac",
                                                        suffix = "M"
                                                      )
                )
              ), 
              defaultPageSize = 5,
              sortable = TRUE,
              searchable = TRUE,
              highlight = TRUE,
              borderless = TRUE, 
              # set the theme for the table
              theme = reactableTheme(
                borderColor = "#BFC9D1",
                color = "#BFC9D1",
                backgroundColor = "#567895",
                highlightColor = "#CE8B65",
                cellPadding = "8px 12px",
                style = list(color = "white"),
                searchInputStyle = list(width = "100%")
              )
              )
      
  })
  
  
 ############### over time ################# 
  # create a plot for the market value over time
  
  market_value_reactive <- reactive({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    
    market_value_filtered <-
      all_leagues_market_values_over_time %>%
      filter(
        club == input$info_team_club_selection &
          league == input$info_team_league_selection
      )
    
  })
  
  output$info_team_market_value_over_time <- renderPlotly({
    market_values <- market_value_reactive() %>%
      plot_ly(x = ~date, y = ~value_then_mil_euro) %>%
      add_lines(color="#CE8B65") %>%
      layout(title = list(text = "Market value over time", y = 0.95, x = 0.5),
             yaxis = list(title = "Market Value (million euro)"),
             xaxis = list(title = "Year"),
             font = list(color = "white"),
             plot_bgcolor = "#567895",
             paper_bgcolor = "#567895",
             fig_bg_color = "#567895")
    
    market_values
  })
  
  
  ############# fifa team rating plot #####################
  fifa_team_reactive <- reactive({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    
    fifa_team_filtered <-
      all_leagues_fifa_team_stats %>%
      filter(
        club == input$info_team_club_selection &
          league == league
      )
    
  })
  # create a plot for the fifa rating over time
  output$info_team_fifa_rating_over_time <- renderPlotly({
    # we need the user to select a club first
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    
    data <- fifa_team_reactive() %>%
      pivot_longer(
        fifa_overall_rating:fifa_international_prestige,
        names_to = "variable",
        values_to = "value"
      )
    
    fifa_rating <- data %>%
      # create actual plot for the market value over time by club
      plot_ly(
        x = ~ date,
        y = ~ value,
        color =  ~ variable,
        type = "scatter",
        visible = "legendonly"
      ) %>%
      layout(
        title = list(text = "Fifa team rating over time", y = 0.95, x = 0.5),
        yaxis = list(title = "Rating"),
        xaxis = list(title = "Year"),
        font = list(color = "white"),
        plot_bgcolor = "#567895",
        paper_bgcolor = "#567895",
        fig_bg_color = "#567895"
      )
    
    fifa_rating
  })
  
  # create a table for past transfers
  output$info_team_transfers_over_time <- renderReactable({
    # we need the user to select a club first 
    req(input$info_team_club_selection)
    
    # test <- major_five_leagues_transfers %>%
    #   filter(from_team_name == "VfB Stuttgart" |
    #            to_team_name == "VfB Stuttgart")
     
    all_leagues_team_transfers %>%
      filter(from_team_name == input$info_team_club_selection |
               to_team_name == input$info_team_club_selection) %>%
      select(transfer_date, player_name, transfer_type, transfer_sum_mil_euro,from_team_name,
             to_team_name) %>%
      arrange(desc(transfer_date)) %>%
      reactable(
        sortable = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        highlight = TRUE,
        borderless = TRUE, 
        # set the theme for the table
        theme = reactableTheme(
          borderColor = "#BFC9D1",
          color = "#BFC9D1",
          backgroundColor = "#567895",
          highlightColor = "#CE8B65",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%")
        ), 
        # modify the layout and names of the columns
        columns = list(
          transfer_date = colDef(name = "Date",
                        align = "left"),
          player_name = colDef(name = "Player",
                               align = "center"),
          transfer_type = colDef(name = "Type",
                        align = "center"),
          from_team_name = colDef(name = "From Team",
                                  align = "center"),
          to_team_name = colDef(name = "To Team",
                                align = "center"),
          transfer_sum_mil_euro = colDef(name = "Money",
                                align = "center")
        )
      )
  })
}