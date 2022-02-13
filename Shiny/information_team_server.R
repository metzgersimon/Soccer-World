# subserver for the team-tab in the information menu item
information_team_server <- function(input, output, session){
  
  observeEvent(input$info_team_league_selection, {
    updateSelectInput(session,
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
    updateSelectInput(session,
                      inputId = "info_team_season_selection",
                      selected = NULL,
                      choices = c(
                        "",
                        paste0(unique(
                          all_infos_club %>% filter(
                            league == input$info_team_league_selection &
                              club == input$info_team_club_selection
                          ) %>%
                            select(season) %>%
                            unlist() %>%
                            unname()
                        )
                        ,"/",unique(
                          all_infos_club %>% filter(
                            league == input$info_team_league_selection &
                              club == input$info_team_club_selection
                          ) %>%
                            select(season) %>%
                            unlist() %>%
                            unname()
                        )+1)))
  })
  
  # create the output for the table on the overview page
  output$info_team_team_name <- function(){
    
    # to have all infos of clubs (to gather country infos)
    all_infos_club <- inner_join(all_leagues_tm_squads, unique(all_leagues_matches[,c(2,3,19)]), by=c("club"="club_name_home", "league"="league_name"))
   
    # to gather venue infos 
    # <- all_infos_club %>% left_join(all_leagues_venue_information[,c(4,8,10:15)], by=c("club"="team_name")) 
    
    # there has to be a club selected
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    req(input$info_team_season_selection)
    
    # convert the season in a format we can work with
    season_year <- as.numeric(str_split(input$info_team_season_selection,
                                        pattern = "/")[[1]][1])
    
    # filter the huge data frame on the club selected
    team_infos <- all_infos_club %>%
      filter(league==  input$info_team_league_selection &club == input$info_team_club_selection &
             season == season_year)

    # extract the club name
    name <- team_infos %>%
      select(club) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    # extract the league of the club
    league <- unique(team_infos$league)
    
    # extract the country of the club
    country <- team_infos %>%
      select(league_country) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    # extract the squad size of the club
    squad_size <- team_infos %>%
      select(player_name) %>%
      unique() %>%
      nrow()
    
    # compute the average age of the players in the club
    avg_age <- team_infos %>%
      # only unique player_names
      distinct(player_name, .keep_all = TRUE) %>%
      select(player_age) %>%
      summarize(avg_age = mean(player_age, na.rm = TRUE)) %>%
      # extract the value (it is a data frame)
      pull() %>%
      # round the value
      round(digits = 2)
    
    # compute the average height of the players in the club
    avg_height <- team_infos %>%
      # only unique player_names
      distinct(player_name, .keep_all = TRUE) %>%
      select(player_height) %>%
      summarize(avg_age = mean(player_height, na.rm = TRUE)) %>%
      # extract the value (it is a data frame)
      pull() %>%
      # round the value
      round(digits = 2)
    
    
    # extract the information for the stadium
    venue_info <- all_leagues_venue_information %>% filter(league==  input$info_team_league_selection &team_name == input$info_team_club_selection &
                                                           season == season_year)%>%
      select(venue_name, venue_city,
             venue_capacity) %>%
      unique()
        
    # combine the selected data (only name, league and country of the club) 
    # to a data frame
    club_info_frame <- data.frame(name, league, country) 
    # create additional rows to be able to merge the frames later
    club_info_frame[, (ncol(club_info_frame)+1):(ncol(club_info_frame)+3)] <- NA
    # transpose the frame such that the columns are now rows
    club_info_frame <- club_info_frame %>% 
      t() 
    
    # create the texts for the table
    squad_size_text <- paste0("Squad size: ", squad_size)
    avg_age_text <- paste0("Average squad age: ", avg_age, " years old")
    avg_height_text <- paste0("Average squad height: ", avg_height, " m")
    venue_name_text <- paste0("Venue: ", venue_info$venue_name)
    venue_city_text <- paste0("\t", venue_info$venue_city)
    venue_capa_text <- paste0("\t", venue_info$venue_capacity, " seats")
    
    # put the texts together into a data frame
    add_info_frame <- data.frame(squad_size_text,
                                 avg_age_text,avg_height_text,
                                 venue_name_text,
                                 venue_city_text,
                                 venue_capa_text) %>%
      # and transpose it (columns to rows)
      t()
    
    
    # combine these two frames by binding them together by column
    suppressMessages(club_frame <- club_info_frame %>%
      bind_cols(add_info_frame))
    
    # set the NA format in a kable table to an empty string
    options(knitr.kable.NA = "")
    
    # create a kable table with the data
    club_frame %>%
      kableExtra::kable("html", row.names = FALSE, col.names = NULL
                        ) %>%
      #kable_minimal()
      kable_styling(full_width = F)
    
  
  }
  
  # output for the club logo
  output$info_team_team_logo <- renderUI({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    
    print(input$info_team_club_selection)
    # extract the logo from the frame
    team_image <- all_leagues_venue_information %>% mutate(league = if_else(
      all_leagues_venue_information$league_id == 78,
      "Bundesliga",
      if_else(
        venues_with_coordinates$league_id == 79,
        "Bundesliga 2",
        if_else(
          all_leagues_venue_information$league_id == 39,
          "Premier League",
          if_else(
            all_leagues_venue_information$league_id == 61,
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
      all_leagues_venue_information$league_id == 78,
      "Bundesliga",
      if_else(
        venues_with_coordinates$league_id == 79,
        "Bundesliga 2",
        if_else(
          all_leagues_venue_information$league_id == 39,
          "Premier League",
          if_else(
            all_leagues_venue_information$league_id == 61,
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

    # selected_club <- "FC Bayern Munich"
    # selected_season <- 2021
    
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
      select(c(fixture_date.x, fixture_time.x,
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
                  fixture_date.x = colDef(name = "Date",
                                       align = "left"),
                  fixture_time.x = colDef(name = "Time",
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
    
    # fixture_test <- paste(as.character(next_match$fixture_date),
    #                       "\n",
    #                       next_match$fixture_time,
    #                       "\n\n")
    # 
    # fixture_test2 <- paste(next_match$venue_name,
    #                       "\n",
    #                       next_match$venue_city)
    # 
    # fixture_time <- next_match$fixture_time
    # venue_name <- next_match$venue_name
    # venue_city <- next_match$venue_city
    
    
    # set the NA format in a kable table to an empty string
    # options(knitr.kable.NA = "")
    # 
    # # create a kable table with the data
    # next_match %>%
    #   select(-c(fixture_date,
    #             fixture_time,
    #             venue_name,
    #             venue_city,
    #             league_round)) %>% 
    #   as.matrix() %>%
    #   kableExtra::kable("html", row.names = FALSE, col.names = NULL
    #   ) %>%
    #   #kable_minimal()
    #   kable_styling(full_width = F, ) %>%
    #   add_header_above(., c(setNames(2, fixture_test)))
    
      
   
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
          borderColor = "#000000",
          color = "#000000",
          backgroundColor = "#004157",
          highlightColor = "#2f829e",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%",
                                  color = "black")
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

  output$info_team_stats <- renderReactable({
    
    # general_shots <- buli_fixture_stats %>% select(contains("shots"), contains("score"))
    #desired_order <- c("fulltime_score_home ", "fulltime_score_away", "shots_on_goal",
    #                   "shots_off_goal", "shots_total ", "shots_blocked",
    #                   "shots_inside_box", "shots_outside_box")
    
    #fixtures <- get_team_stats_cleaned(general_shots$shots, total = TRUE,
    #                                   desired_order)

    
    #desired_order <- c("goals_for", "goals_for_average", "goals_against",
    #                   "goals_against_average")
    
    #goals <- get_team_stats_cleaned(test$goals, total = TRUE,
   #                                 desired_order) 
    
    # goals_diff <- rownames_to_column(goals) %>%
    #  filter(rowname %in% c("goals_for", "goals_against")) %>%
    #  mutate(diff_away = lag(away) - away,
    #         diff_home = lag(home) - home,
    #         diff_total = lag(total) - total) %>%
    #  select(away = diff_away,
    #         home = diff_home,
    #        total = diff_total) %>%
    #  remove_empty("rows")
    
    #rownames(goals_diff) <- "goals_diff"
    
    #goals <- rbind(goals,
    #               goals_diff)
      
    
    #biggest <- get_team_stats_cleaned(test$biggest, total = FALSE)
    
    #desired_order <- "failed_to_score"
    #failed_to_score <- get_team_stats_cleaned(test$failed_to_score, total = TRUE,
    #                                          desired_order)
    
    #desired_order <- "clean_sheet"
    #clean_sheet <- get_team_stats_cleaned(test$clean_sheet, total = TRUE,
    #                                      desired_order)
    
    # penalty <- test$penalty
    # cards <- test$cards
    
    #stats_all <- rbind(fixtures,
    #                   goals,
    #                   #biggest,
    #                   failed_to_score,
    #                   clean_sheet) %>%
    #  select(home, away, total)
    
    #rownames(stats_all) <- c("Games played", "Wins", "Draws", "Loses",
    #                         "Goals for", "Avg Goals for", "Goals agaist",
    #                         "Avg Goals against", "Goal Difference",
    #                         "Failed to score", "Clean sheets")
    league <- if_else(
        input$info_team_league_selection =="Bundesliga 1",
        league_id == 78,
        if_else(
          input$info_team_league_selection =="Bundesliga 2",
          league_id == 79,
          if_else(
            input$info_team_league_selection =="Premier League",
            league_id == 39,
            if_else(
              input$info_team_league_selection =="Ligue 1",
              league_id == 61,
          )
        )
      ))
    
    all_club_stats <-
      left_join(
        all_leagues_club_stats,
        all_leagues_matches,
       by = c("team_name" = "club_name_home", "league_season" = "league_season", "league_id" = "league_id" ,"league_country" = "league_country")
     )
    
    
    all_club_stats %>%
      filter(league_id == league_id &
        team_name == input$info_team_club_selection &
               league_season == as.numeric(
                 str_split(input$info_team_season_selection,
                           pattern = "/")[[1]][1]))  %>%    
      mutate(game_score = paste0(fulltime_score_home, ":", fulltime_score_away)) %>%
 #     select(fixture_date, game_score, contains("home"), passing_accuracy,
 #            ball_possession, fouls, corners, offsides, 
 #            contains("passes"), contains("cards"), goalkeeper_saves) %>% 
 #     select(fixture_date, game_score,goals_for_total_home)
      reactable(
        defaultColDef = colDef(
          align = "center",
          minWidth = 150,
          headerStyle = list(background = "darkblue")
        ),
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        borderless = TRUE,
        pagination = FALSE,
        defaultPageSize = 15,
        showPageInfo = FALSE,
        showPagination = FALSE,
        # set the theme for the table
        theme = reactableTheme(
          borderColor = "#000000",
          color = "#000000",
          backgroundColor = "#004157",
          highlightColor = "#2f829e",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%",
                                  color = "black")
        ) #       columns = list(
          #fixture_date = colDef(name = "date",
          #                        align = "left"),
         #game_score = colDef(name = "results",
          #                        align = "center"),
          #passing_accuracy = colDef(name = "passing accuracy",
          #                         align = "center"),
          #ball_possession = colDef(name = "ball possession",
          #                      align = "left"),
          #goalkeeper_saves = colDef(name = "galkeeper saves",
          #                    align = "center"),
          #passing_accuracy = colDef(name = "passing accuracy",
          #                          align = "center")
                  
      )
    
    #reactable(stats_all,
    #          highlight = TRUE,
    #          borderless = TRUE,
    #          pagination = FALSE,
    #          defaultPageSize = 15,
    #          showPageInfo = FALSE,
    #          showPagination = FALSE,
              # set the theme for the table
    #          theme = reactableTheme(
    #            borderColor = "#000000",
    #            color = "#000000",
    #            backgroundColor = "#004157",
    #            highlightColor = "#2f829e",
    #            cellPadding = "8px 12px",
    #            style = list(color = "white")
    #          ),
    #          columns = list(
    #            home = colDef(name = "HOME",
    #                          align = "center"),
    #            away = colDef(name = "AWAY",
    #                          align = "center"),
    #            total = colDef(name = "TOTAL",
    #                           align = "center")
    #          )
              
    #)
    
  })
  
  output$info_team_summary <- renderPrint({
   # data <-  buli_fixture_stats %>% filter(team_name == "FC Bayern Munich" & season == 2017)
   data <- buli_fixture_stats %>%
     filter(team_name == input$info_team_club_selection &
              season == as.numeric(str_split(
                input$info_team_season_selection,pattern = "/")[[1]][1]))
   
   summary(data[, c(10:25)])
   #mean_shots_total <-  buli_fixture_stats %>%
   #   filter(team_name == input$info_team_club_selection &
   #            season == as.numeric(str_split(
   #              input$info_team_season_selection,pattern = "/")[[1]][1]))%>% 
   #  summarise(mean(shots_total, na.rm = TRUE)) %>% unname() %>% unlist()
   
   #total_yellow <-  buli_fixture_stats %>%
   #  filter(team_name == input$info_team_club_selection &
   #           season == as.numeric(str_split(
   #             input$info_team_season_selection,
   #             pattern = "/"
   #           )[[1]][1]))  %>% summarise(sum(cards_yellow, na.rm = TRUE))%>%
   #  unname() %>% unlist()
   
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
      all_leagues_tm_squads %>%
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
                borderColor = "#000000",
                color = "#000000",
                backgroundColor = "#004157",
                highlightColor = "#2f829e",
                cellPadding = "8px 12px",
                style = list(color = "white"),
                searchInputStyle = list(width = "100%",
                                        color = "black")
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
                borderColor = "#000000",
                color = "#000000",
                backgroundColor = "#004157",
                highlightColor = "#2f829e",
                cellPadding = "8px 12px",
                style = list(color = "white"),
                searchInputStyle = list(width = "100%",
                                        color = "black")
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
                borderColor = "#000000",
                color = "#000000",
                backgroundColor = "#004157",
                highlightColor = "#2f829e",
                cellPadding = "8px 12px",
                style = list(color = "white"),
                searchInputStyle = list(width = "100%",
                                        color = "black")
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
                borderColor = "#000000",
                color = "#000000",
                backgroundColor = "#004157",
                highlightColor = "#2f829e",
                cellPadding = "8px 12px",
                style = list(color = "white"),
                searchInputStyle = list(width = "100%",
                                        color = "black")
                )
              )
      
  })
  
  
 ############### over time ################# 
  # create a plot for the market value over time
  
  market_value_reactive <- reactive({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)

    if(input$info_team_league_selection=="Bundesliga 1"){
      league <- "Bundesliga"
    } else {league <- input$info_team_league_selection}
    
    market_value_filtered <-
      all_leagues_market_values_over_time %>%
      filter(
        club == input$info_team_club_selection &
          league == league
      )
    
  })
  
  output$info_team_market_value_over_time <- renderPlotly({
    market_values <- market_value_reactive() %>%
      # transform the market value at the given time to a numeric
      # mutate(value_then = as.numeric(str_remove_all(value_then,"[\u20AC|m]")) * 1000000) %>%
      # create actual plot for the market value over time by club
      plot_ly(x = ~date, y = ~value_then_mil_euro) %>%
      add_lines() %>%
      layout(title = list(text = "Market value over time", y = 0.95, x = 0.5),
             yaxis = list(title = "Market Value (million euro)"),
             xaxis = list(title = "Year"),
             font = list(color = "white"),
             plot_bgcolor = "rgba(0, 65, 87, 10)",
             paper_bgcolor = "rgba(0, 65, 87, 10)",
             fig_bg_color = "rgba(0, 65, 87, 10)")
    
    market_values
  })
  
  
  ############# fifa team rating plot #####################
  fifa_team_reactive <- reactive({
    req(input$info_team_league_selection)
    req(input$info_team_club_selection)
    
    if(input$info_team_league_selection=="Bundesliga 1"){
      league <- "Bundesliga"
    } else {league <- input$info_team_league_selection}
    
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
        title = "Fifa team rating over time",
        yaxis = list(title = "Rating"),
        xaxis = list(title = "Year"),
        font = list(color = "white"),
        plot_bgcolor = "rgba(0, 65, 87, 10)",
        paper_bgcolor = "rgba(0, 65, 87, 10)",
        fig_bg_color = "rgba(0, 65, 87, 10)"
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
     
    major_five_league_transfers %>%
      filter(from_team_name == input$info_team_club_selection |
               to_team_name == input$info_team_club_selection) %>%
      select(date, player_name, type, from_team_name,
             to_team_name) %>%
      arrange(desc(date)) %>%
      reactable(
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
          searchInputStyle = list(width = "100%",
                                  color = "black")
        ), 
        # modify the layout and names of the columns
        columns = list(
          date = colDef(name = "Date",
                        align = "left"),
          player_name = colDef(name = "Player",
                               align = "center"),
          type = colDef(name = "Type",
                        align = "center"),
          from_team_name = colDef(name = "From Team",
                                  align = "center"),
          to_team_name = colDef(name = "To Team",
                                align = "center")
        )
      )
  })
}