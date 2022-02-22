# subserver for the league-tab in the information menu item
information_league_general_server <- function(input, output, session){
  
  # create an observer to display for the season selection
  # only those clubs that are present in the selected league
   observeEvent(input$information_league_league_selection, {
     updateSelectizeInput(session, 
                          inputId = "information_league_season_selection",
                          choices = c("", paste0(unique(
                            all_leagues_matches %>% filter(
                              league_name == input$information_league_league_selection 
                            ) %>%
                              select(league_season) %>%
                              unlist() %>%
                              unname()
                          )
                          ,"/",unique(
                            all_leagues_matches %>% filter(
                              league_name == input$information_league_league_selection
                            ) %>%
                              select(league_season) %>%
                              unlist() %>%
                              unname()
                          )+1))
     )
     
   })
  
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
  overview_data <- reactive({
    req(input$info_team_league_selection)
    req(input$info_team_season_selection)
    
    overview_filtered <-
      all_leagues_matches %>% left_join(all_leagues_squads_tm, by = c("club_name_home" =
                                                                        "club"))
    
  })
  
  output$info_league_overview_table <- function() {
    req(input$information_league_league_selection)
    req(input$information_league_season_selection)
    
    league_infos <- overview_data() %>%
      filter(
        league_name == input$information_league_league_selection &
          season == as.numeric(
            str_split(input$information_league_season_selection,
                      pattern = "/")[[1]][1]
          )
      )
    
    # extract the name of the league
    name <- unique(league_infos$league_name)[1]
    
    # extract the country the league takes place in
    country <- unique(league_infos$league_country)
    
    # compute the number of teams playing in the league
    number_teams <- length(unique(league_infos$club_id_home))
    
    # compute the average age for each club
    avg_age_per_club <- league_infos %>%
      group_by(club_name_home) %>%
      summarize(avg_age = round(mean(player_age, na.rm = TRUE), 2))
    
    # filter the average age for each club
    # for the club which has the lowest avg age
    min_avg_age <- filter(avg_age_per_club,
                          avg_age == min(avg_age, na.rm = TRUE))
    
    # filter the average age for each club
    # for the club which has the highest avg age
    max_avg_age <- filter(avg_age_per_club,
                          avg_age == max(avg_age, na.rm = TRUE))
    
    # compute the average age in the whole league
    avg_age_league <- league_infos %>%
      summarize(avg_age = mean(player_age, na.rm = TRUE)) %>%
      # pull the data because the summarize creates a data frame
      # pull extract the actual value
      pull() %>%
      # round it
      round(2)
    
    # compute the average market value in the whole league
    avg_market_value <- league_infos %>%
      summarize(avg_market_value = mean(player_market_value_in_million_euro,
                                        na.rm = TRUE)) %>%
      # pull the data because the summarize creates a data frame
      # pull extract the actual value
      pull() %>%
      # round it
      round(2)
    
    # find the most valuable player and its information
    most_valuable_player <- filter(
      league_infos,
      player_market_value_in_million_euro ==
        max(player_market_value_in_million_euro,
            na.rm = TRUE)
    ) %>%
      # select player name, club and market value
      select(player_name,
             club_name_home,
             player_market_value_in_million_euro) %>%
      unique()
    
    # compute the total number of players playing in the league
    number_players <- league_infos %>%
      # unique player names
      distinct(player_name) %>%
      count()
    
    # create the text blocks for the table
    #country_text <- paste0("Country: ", country)
    number_teams_text <- paste0("Number of teams: ", number_teams)
    number_players_text <-
      paste0("Number of players: ", number_players$n)
    avg_age_league_text <- paste0("Average age: ", avg_age_league)
    min_avg_age_text <-
      paste0("Joungest team: ",
             min_avg_age$club_name_home ,
             " (",
             min_avg_age$avg_age, ")")
    max_avg_age_text <-
      paste0("Oldest team: ",
             max_avg_age$club_name_home ,
             " (",
             max_avg_age$avg_age, ")")
    avg_market_value_text <-
      paste0("Average market value: ", avg_market_value, " million")
    most_valuable_player_text <-
      paste0(
        "Most valuable Player: ",
        most_valuable_player$player_name,
        " ","- ",
        most_valuable_player$club_name_home ,
        " (",
        most_valuable_player$player_market_value_in_million_euro," million)"
      )
    
    # put all the information together into a data frame
    # with 3 columns
    league_info_frame <- data.frame(matrix(
      c(
        number_teams_text,
        number_players_text,
        avg_age_league_text,
        min_avg_age_text,
        max_avg_age_text,
        avg_market_value_text,
        most_valuable_player_text
      ),
     # ncol = 3,
      byrow = TRUE
    ))
    
    # set the NA format in a kable table to an empty string
    options(knitr.kable.NA = "")
    
     league_info_frame %>%
        kableExtra::kable("html", row.names = FALSE, col.names = NULL) %>%
        #kable_minimal()
        kable_styling(full_width = F) #%>%
        # deparse() possible to extract string of variable name
        # but needs to be cleaned
        # set the name of the league as header
        #add_header_above(c("Bundesliga" = 3), bold = TRUE,
        #                 font_size = 20)
    
  }

  
  output$info_league_league_logo <- renderUI({
    # we need the user to select a club first
    req(input$information_league_league_selection)
    
    print(input$information_league_league_selection)
    # extract the logo from the frame
    league_logo <-  all_leagues_matches  %>%
      filter(league_name == input$information_league_league_selection) %>%
      select(league_logo) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    # set the img on the extracted logo
    tags$img(src = league_logo)
  })

  output$info_league_country_logo <- renderUI({
    # we need the user to select a club first
    req(input$information_league_league_selection)
    
    print(input$information_league_league_selection)
    # extract the logo from the frame
    country_logo <-  all_leagues_matches  %>%
      filter(league_name == input$information_league_league_selection) %>%
      select(league_flag) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    # set the img on the extracted logo
    tags$img(src = country_logo ,width = "80%")
  })
  
  ################ season specific #################
  # update matchday
  observeEvent(input$information_league_season_selection, {
    data <- all_leagues_matches %>%
      filter(league_name == input$information_league_league_selection & league_season == 
               as.numeric(str_split(
                 input$information_league_season_selection,
                 pattern = "/")[[1]][1]
               )&
               !is.na(status_elapsed)) %>%
      select(league_round) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    updateSelectizeInput(session, 
                         inputId = "information_league_matchday_selection",
                         choices = data,
                         selected = max(data,na.rm = TRUE) ### IMMER MAX MATCHDAY AUSWÄHLEN ###
                         # BEI PAAR SAISONS FUNKTIONIER MAX NICHT, PASST FORMAT NICHT?
                         # CLUB NOCH ALS LISTE VORHANDEN
    )
    
  })
  
  output$information_league_matchday_fixtures <- renderReactable({
    all_leagues_matches %>%
      filter(league_name == input$information_league_league_selection & league_season == 
               as.numeric(str_split(
                 input$information_league_season_selection,
                 pattern = "/")[[1]][1]
               ) &
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
    market_values <- all_leagues_market_values_over_time %>% 
       filter(league==input$information_league_league_selection) %>%
      # create actual plot for the market value over time by club
      plot_ly(x = ~date, y = ~value_then_mil_euro, color = ~club,
              colors = colors) %>%
      add_lines() %>%
      layout(
        title = list(text = "Team market value over time", y = 0.95, x = 0.5),
            yaxis = list(title = "Market Value"),
             xaxis = list(title = "Year"),
             font = list(color = "white"),
             plot_bgcolor = "rgba(0, 65, 87, 10)",
             paper_bgcolor = "rgba(0, 65, 87, 10)",
             fig_bg_color = "rgba(0, 65, 87, 10)")
    
    market_values
  })
  
}