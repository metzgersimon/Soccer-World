information_player_server <- function(input, output, session){
  
  sapply(player_tab_data, function(x)
    sum(is.na(x)))

   observeEvent(input$information_player_league_selection, {
     updateSelectizeInput(session, 
                       inputId = "information_player_team_selection",
                       choices = unique(player_tab_data %>%
                                          filter(league_name == input$information_player_league_selection) %>%
                                          select(team_name) %>%
                                          unlist() %>%
                                          unname())
                     )
   })
  

  # create an observer to display for the club selection to display
  # only those players that are present in the selected club
  observeEvent(input$information_player_team_selection, {
    updateSelectizeInput(session, 
                      inputId = "information_player_season_selection",
                      choices = c(
                        "",
                        paste0(unique(player_tab_data %>%
                                        filter(team_name == input$information_player_team_selection &
                                                 league_name == input$information_player_league_selection) %>%
                                        select(league_season) %>%
                                        unlist() %>%
                                        unname())
                        ,"/",unique(
                          player_tab_data %>% filter(
                            team_name == input$information_player_team_selection &
                              league_name == input$information_player_league_selection
                          ) %>%
                            select(league_season) %>%
                            unlist() %>%
                            unname()
                        )+1)),
                      selected = ""
    )
    
  })
  
  observeEvent(input$information_player_season_selection, {
    updateSelectizeInput(session, 
                      inputId = "information_player_player_selection",
                      choices = unique(player_tab_data %>%
                                         filter(team_name == input$information_player_team_selection &
                                                  league_name == input$information_player_league_selection &
                                                  league_season ==  as.numeric(
                                                    str_split(input$information_player_season_selection,
                                                              pattern = "/")[[1]][1])) %>%
                                         select(player_name) %>%
                                         unlist() %>%
                                         unname()),
                      selected = ""
    )
    
  })
  
  
  # filter the data for the selected club and the selected player
  player_infos <- reactive({
    player_tab_data %>%
    filter(league_name == input$information_player_league_selection,
           team_name == input$information_player_team_selection,
           player_name == input$information_player_player_selection,
           league_season ==  as.numeric(
             str_split(input$information_player_season_selection,
                       pattern = "/")[[1]][1])) %>%
    data.frame() %>%
    unique()
  })
  # create the output for the table on the overview page
  output$info_player_overview <- renderUI({
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)
    
    HTML(paste(
      paste0("<b>", "Name: ", "</b>", unique(player_infos()$player_name)),
      paste0(
        "<b>",
        "Country: ",
        "</b>",
        unique(player_infos()$player_nationality)
      ),
      paste0("<b>", "Club: ", "</b>",  unique(player_infos()$team_name)),
      paste0(
        "<b>",
        "Position: ",
        "</b>",
        unique(player_infos()$player_position)
      ),
      paste0("<b>", "Foot: ", "</b>" , unique(player_infos()$player_foot)),
      paste0(
        "<b>",
        "Market value: ",
        "</b>",
        unique(player_infos()$player_market_value_in_million_euro), " Million Euro"
      ),      
      paste0("<b>", "Previous club: ", "</b>" , unique(player_infos()$player_previous_club)),
      sep = "<br/>"
    ))
    
    # 
    # # extract the league of the player
    # league <- unique(player_infos$league) 
    # 
    # # extract the nationality of the player
    # nationality <- unique(player_infos$player_nationality)
    # 
    # # extract the position the player plays on
    # position <- unique(player_infos$position)
    # 
    # # extract the market value of the player
    # market_value <- unique(player_infos$player_market_value_in_million_euro)
    # contract_date <- unique(player_infos$player_contract_date)
    # 
    # # create the texts for the table
    # country_text <- paste0("Country: ", nationality)
    # position_text <- paste0("Position: ", position)
    # birthday_text <- paste0("Birthday: ", birth_date)
    # age_text <- paste0("Age: ", age)
    # height_text <- paste0("Height: ", height, " m")
    # joining_date_text <- paste0("Joined: ", joining_date)
    # market_value_text <- paste0("Market value: ", market_value, " million")
    # contract_text <- paste0("Contract to: ", contract_date)
    # # 
    # player_info_frame <-
    #   data.frame(matrix(
    #     c(
    #       country_text,
    #       position_text,
    #       birthday_text,
    #       age_text,
    #       height_text,
    #       market_value_text,
    #       joining_date_text,
    #       contract_text
    #     ),
    #     ncol = 2,
    #     nrow = 4,
    #     byrow = TRUE
    #   )
    # ) 
    # 
    # `colnames<-`(player_info_frame , NULL)
    # set the NA format in a kable table to an empty string 
    # options(knitr.kable.NA = "")
    
    # create a kable table with the data
    #player_info_frame %>%
    #  kableExtra::kable("html", row.names = FALSE, col.names = NULL
    #  ) %>%
    #  kable_minimal() %>%
    #  kable_styling(full_width = F)
    
  })
  
  # output for the player logo
  output$info_player_player_img <- renderUI({
    # we need the user to select a player first
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    print(input$information_player_player_selection)
    # extract image from the data
    player_image <- all_leagues_player_stats %>%
      filter(league_name == input$information_player_league_selection &
               player_name == input$information_player_player_selection) %>%
      select(player_photo) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    print(player_image)
    
    # set the img on the extracted image
    tags$img(src = player_image)
  })
  
  # output for the club logo
  output$info_player_club_img <- renderUI({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)    

        # extract image from the data
    club_image <- all_leagues_club_stats %>%
      filter(team_name == input$information_player_team_selection) %>%
      select(team_logo) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    print(club_image)
    
    # set the img on the extracted image
    tags$img(src = club_image)
  })
  
############ valuebox 
  filter_player_data <- reactive({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    # filter the huge data frame on the club selected
    player_tab_data %>%
      filter(league_name ==  input$information_player_league_selection,
             team_name == input$information_player_team_selection,
             league_season ==  as.numeric(str_split(input$information_player_season_selection,
                                                    pattern = "/")[[1]][1]),
             player_name == input$information_player_player_selection)
    
    
  })
  
  output$joining_date <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    valueBox(
      filter_player_data()  %>%
        select(player_joining_date) %>% pull(),
      "Joining date",
      icon = icon("arrow-left"),
      color = "orange",
      width = 3
    )
  })
  
  output$contract_date <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
  
  valueBox(
    filter_player_data() %>%
      select(player_contract_date) %>%
      pull() ,
    "Contract date",
    icon = icon("arrow-right"),
    color = "purple",
    width = 3
  )
})
  
  output$age <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  

    valueBox(
      filter_player_data() %>%
        select(player_age) %>%
        pull(),
      "Age",
      icon = icon("user"),
      color = "green",
      width = 3
    )
  })
  
  output$height <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    valueBox(
      filter_player_data() %>%
        select(player_height) %>%
        pull(),
      "Height (m)",
      icon = icon("flag"),
      color = "green",
      width = 3
    )
  })
  
  output$game_minute <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    valueBox(
      filter_player_data() %>%
        select(games_minutes) %>%
        pull() %>% round(2),
      "Mean Game Minutes",
      # icon = icon("clock"),
      color = "green",
      width = 3
    )
  })
  
  output$passes_accuracy <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    valueBox(
      filter_player_data() %>%
        select(passes_accuracy) %>%
        pull() %>%round(2), 
      "Mean Passes Accuracy",
      # icon = icon("flag"),
      color = "blue",
      width = 3
    )
  })
  
  
  output$card_yellow <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    valueBox(
      filter_player_data() %>%
        select(cards_yellow) %>%
        pull() , 
      "Total Yellow Cards",
      # icon = icon("cross"),
      color = "orange",
      width = 3
    )
  })
  
  output$card_red <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    valueBox(
      filter_player_data() %>%
        select(cards_red) %>%
        pull() , 
      "Total Red Cards",
      # icon = icon("cross"),
      color = "purple",
      width = 3
    )
  })
  
  output$game_substitute <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    valueBox(
      filter_player_data() %>%
        select(games_substitute) %>%
        pull() , 
      "Total Games Substitute",
      # icon = icon("flag"),
      color = "purple",
      width = 3
    )
  })
  
  output$goals_assist <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    valueBox(
      filter_player_data() %>%
        select(goals_assists) %>%
        pull() , 
      "Total Goals Assists",
      # icon = icon("arrow"),
      color = "orange",
      width = 3
    )
  })
  
  output$goals_assist <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    valueBox(
      filter_player_data() %>%
        select(goals_assists) %>%
        pull() , 
      "Total Goals Assists",
      # icon = icon("arrow"),
      color = "orange",
      width = 3
    )
  })
  
  
  output$penalty_scored <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    valueBox(
      filter_player_data() %>%
        select(penalty_scored) %>%
        pull() , 
      "Total Scored Penalty",
      # icon = icon("arrow"),
      color = "green",
      width = 3
    )
  })
  
  output$penalty_saved <- renderValueBox({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    valueBox(
      filter_player_data() %>%
        select(penalty_saved) %>%
        pull() , 
      "Total Saved Penalty",
      icon = icon("arrow"),
      color = "blue",
      width = 3
    )
  })
  
########### radar plot
  output$info_player_stats_radarplot <- renderPlot({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    data <- filter_player_data() %>% select(goals_total,dribbles_success, shots_total,tackles_total, fouls_committed , passes_total  ,duels_total )
    
    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    data <- rbind(rep(max(data, na.rm = TRUE),10) , rep(0,10) , data)
    
    # Custom the radarChart !
    radarchart( data  , axistype=1 , 
                
                #custom polygon
                pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                
                #custom labels
                vlcex=0.8 
    )
    # radarchart(
    #   data   ,
    #   axistype = 1 )
      # #custom polygon
      # pcol = colors_border ,
      # pfcol = colors_in ,
      # plwd = 8 ,
      # plty = 1
      #custom the grid
      # cglcol = "grey",
      # cglty = 1,
      # axislabcol = "grey",
      # caxislabels = seq(0, 20, 5),
      # cglwd = 0.8,
      # #custom labels
      # vlcex = 0.8
    # )
    # legend(
    #   x = 0.7,
    #   y = 1,
    #   legend = rownames(data[-c(1, 2), ]),
    #   bty = "n",
    #   pch = 20 ,
    #   col = colors_in ,
    #   text.col = "grey",
    #   cex = 1.2,
    #   pt.cex = 3
    # )
  })

 
  
  
############## stats
  
  player_stats <- reactive({
    all_leagues_player_stats %>%
      filter(league_name == input$information_player_league_selection &
               player_name == input$information_player_player_selection &
               league_season == as.numeric(
                 str_split(input$information_player_season_selection,
                           pattern = "/")[[1]][1])) 
  })
  # create the table output for the stats
  output$info_player_stats_general_games <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)      
    # last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    player_stats()%>%
      select(team_name, fixture_date,
             contains("games")) %>%
           #  contains("goals")) %>%
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
      ,
    # modify the layout and names of the columns
               columns = list(
                 fixture_date = colDef(name = "Date",
                               align = "left"),
                 team_name = colDef(name = "Club",
                                      align = "center"),
                 games_minutes = colDef(name = "Game minutes",
                               align = "center"),
                 games_number = colDef(name = "Game number",
                                         align = "center"),
                 games_position = colDef(name = "Position",
                                       align = "center"),
                 games_rating = colDef(name = "Game rating",
                                       align = "center"),
                 games_captain = colDef(name = "Game captain",
                                       align = "center"),
                 games_substitute = colDef(name = "Game substitute",
                                       align = "center")
                
               )
      )
  })
  
  
  # create the table output for the stats
  output$info_player_stats_general_goals <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)      
    # last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    player_stats()%>%
      select(fixture_date,
             contains("goals")) %>%
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
        ,
        # modify the layout and names of the columns
        columns = list(
          fixture_date = colDef(name = "Date",
                                align = "left"),  
          goals_total = colDef(name = "Total goals",
                               align = "center"),
          goals_conceded = colDef(name = "Conceded goals",
                                  align = "center"),
          goals_assists = colDef(name = "Assist goals",
                                 align = "center"),
          goals_saves = colDef(name = "Saved goals",
                               align = "center")
          
        )
      )
  })
 
  
  # create the table output for the stats
  output$info_player_stats_general_shots <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)      
    # last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    player_stats() %>%
      select(contains("shots")) %>%
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
        ,
        # modify the layout and names of the columns
        columns = list(
          shots_total = colDef(name = "Total shots",
                               align = "center"),
          shots_on = colDef(name = "Shots on goal",
                                  align = "center")
          
        )
      )
  })
  
  
  # create the table output for the stats
  output$info_player_stats_general_passes <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)      
    # last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    player_stats() %>%
      select(fixture_date, contains("passes")) %>%
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
        ,
        # modify the layout and names of the columns
        columns = list(
          fixture_date = colDef(name = "Date",
                                align = "left"),
          passes_total = colDef(name = "Total passes",
                               align = "center"),
          passes_key = colDef(name = "Key passes",
                            align = "center"),
          passes_accuracy = colDef(name = "Passes accuracy",
                              align = "center")
          
        )
      )
  })
  
  output$info_player_stats_general_duel <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)      
    # last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    
    player_stats() %>%
      select(contains("duels")) %>%
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
        ,
        # modify the layout and names of the columns
        columns = list(
          duels_total = colDef(name = "Total duels",
                                align = "center"),
          duels_won = colDef(name = "Duels won",
                              align = "center")
          
        )
      )
  })
  
  output$info_player_stats_general_dribbles <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)      
    # last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    
    player_stats() %>%
      select(fixture_date, tackles_total,tackles_blocks,tackles_interception,contains("dribbles")) %>%
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
        ,
        # modify the layout and names of the columns
        columns = list(
          fixture_date = colDef(name = "Date",
                                align = "left"),
          dribbles_attempts = colDef(name = "Attempt dribbles",
                               align = "center"),
          dribbles_past = colDef(name = "Past dribbles",
                             align = "center"),
          dribbles_success = colDef(name = "Success dribbles",
                                 align = "center"),
          tackles_total = colDef(name = "Total tackles",
                                    align = "center"),
          tackles_blocks = colDef(name = "Blocks tackles",
                                 align = "center"),
          tackles_interceptions = colDef(name = "Interceptions",
                                 align = "center")
          
        )
      )
  })
  
  
  output$info_player_stats_general_cards <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)      
    # last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    
    player_stats() %>%
      select(fixture_date, contains("cards"), contains("penalty")) %>%
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
        ,
        # modify the layout and names of the columns
        columns = list(
          fixture_date = colDef(name = "Date",
                                align = "left"),
          penalty_won = colDef(name = "Penality won",
                                     align = "center"),
          penalty_commited = colDef(name = "Commited penality",
                                 align = "center"),
          penalty_missed = colDef(name = "Missed penality",
                                    align = "center"),
          penalty_saved = colDef(name = "Saved penality",
                                  align = "center"),
          penalty_scored = colDef(name = "Scored penality",
                                 align = "center"),
          cards_yellow = colDef(name = "Yellow cards",
                                  align = "center"),
          cards_red = colDef(name = "Red cards",
                                align = "center")
          
        )
      )
  })
  
  output$info_player_stats_general_offsides <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)      
    # last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    
    player_stats() %>%
      select(fixture_date, offsides, contains("fouls")) %>%
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
        ,
        # modify the layout and names of the columns
        columns = list(
          fixture_date = colDef(name = "Date",
                                align = "left"),
          offsides = colDef(name = "Offsides",
                                     align = "center"),
          fouls_drawn = colDef(name = "Drawn fouls",
                                 align = "center"),
          fouls_committed = colDef(name = "Committed fouls",
                                    align = "center")
          
        )
      )
  })
  
 
  # create the table output for the stats rating
  output$info_player_stats_rating <- renderPlotly({
    req(input$information_player_player_selection)
    req(input$information_player_league_selection)

    # last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    
    data <- all_leagues_fifa_squads %>%
      # currently we have to filter out these dates
      filter(league== input$information_player_league_selection&
               fifa_player_name == input$information_player_player_selection) %>%
      pivot_longer(
        fifa_player_overall_rating:fifa_player_market_value_mil_euro,
        names_to = "variable",
        values_to = "value"
      )
    
    fifa_player_rating <- data %>%
      # create actual plot for the market value over time by club
      plot_ly(
        x = ~ date,
        y = ~ value,
        color =  ~ variable,
        type = "scatter"#,
       # visible = "legendonly"
      ) %>%
      layout(
        title = "Fifa player rating and market value over time",
        yaxis = list(title = c("Rating","Market value")),
        xaxis = list(title = "Year"),
        font = list(color = "white"),
        plot_bgcolor = "rgba(0, 65, 87, 10)",
        paper_bgcolor = "rgba(0, 65, 87, 10)",
        fig_bg_color = "rgba(0, 65, 87, 10)"
      )
    
    fifa_player_rating
  })
  
  
  # create a table for all transfers the player had so far during his career
  output$info_player_transfers <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    
    # some player name has only one word, we should distinguish the name length to avoid the warning
    if (str_count(input$information_player_player_selection, ' ') >= 1) {
      last_name <-
        str_split(input$information_player_player_selection, " ")[[1]][-1]
    } else if (str_count(input$information_player_player_selection, ' ') == 0){ # name with only one word 
      last_name <-
        input$information_player_player_selection
    }
    
    # get the transfer infos for the selected player and team
    all_leagues_team_transfers  %>%
      filter(
        player_name %like% last_name,
        from_team_name == input$information_player_team_selection |
          to_team_name == input$information_player_team_selection
      ) %>%
      select(
        transfer_date,
        player_name,
        transfer_type,
        from_team_name,
        to_team_name,
        transfer_sum_mil_euro
      ) %>%
      distinct() %>%
      arrange(desc(transfer_date)) %>%
      reactable(
        defaultColDef = colDef(
          align = "center",
          minWidth = 150,
          headerStyle = list(background = "darkblue")
        ),
        striped = TRUE,
        highlight = TRUE,
        borderless = TRUE,
        # set the theme for the table
        theme = reactableTheme(
          borderColor = "#000000",
          color = "#000000",
          backgroundColor = "#004157",
          highlightColor = "#2f829e",
          cellPadding = "8px 12px",
          style = list(color = "white")
        ),
        # modify the layout and names of the columns
        columns = list(
          transfer_date = colDef(name = "Date",
                                 align = "left"),
          player_name = colDef(name = "Player",
                               align = "center"),
          transfer_type = colDef(name = "Type",
                                 align = "center"),
          transfer_sum_mil_euro = colDef(name = "Money (million)",
                                         align = "center"),
          from_team_name = colDef(name = "From Team",
                                  align = "center"),
          to_team_name = colDef(name = "To Team",
                                align = "center")
        )
      )
  })
  
}
