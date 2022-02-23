information_player_server <- function(input, output, session){
  
  # check all the missing values in player_tab_data
  sapply(player_tab_data, function(x)
    sum(is.na(x)))

  # update the select input team according to the league selection
  observeEvent(input$information_player_league_selection, {
    updateSelectizeInput(
      session,
      inputId = "information_player_team_selection",
      choices = unique(
        player_tab_data %>%
          filter(league_name == input$information_player_league_selection) %>%
          select(team_name) %>%
          unlist() %>%
          unname()
      )
    )
  })
  

   # create an observer to display for the club selection to display
   # only those seasons that are present in the selected club
   observeEvent(input$information_player_team_selection, {
     updateSelectizeInput(
       session,
       inputId = "information_player_season_selection",
       # want to have the season format like this XXXX/XXXX
       choices = c("",
                   paste0(
                     unique(
                       player_tab_data %>%
                         filter(
                           team_name == input$information_player_team_selection &
                             league_name == input$information_player_league_selection
                         ) %>%
                         select(league_season) %>%
                         unlist() %>%
                         unname()
                     )
                     ,
                     "/",
                     unique(
                       player_tab_data %>% filter(
                         team_name == input$information_player_team_selection &
                           league_name == input$information_player_league_selection
                       ) %>%
                         select(league_season) %>%
                         unlist() %>%
                         unname()
                     ) + 1
                   )),
       selected = ""
     )
     
   })
  
   # create an observer to display for the season selection to display
   # only those players that are present in the selected club
   observeEvent(input$information_player_season_selection, {
     updateSelectizeInput(
       session,
       inputId = "information_player_player_selection",
       choices = unique(
         player_tab_data %>%
           filter(
             team_name == input$information_player_team_selection &
               league_name == input$information_player_league_selection &
               league_season ==  as.numeric(
                 str_split(input$information_player_season_selection,
                           pattern = "/")[[1]][1]
               )
           ) %>%
           select(player_name) %>%
           unlist() %>%
           unname()
       ),
       selected = ""
     )
     
   })
  
   ############# Overview tab begins #####################
  # filter the data for the selected league, club, season and the selected player
  # reactive is more efficient
  player_infos <- reactive({
    player_tab_data %>%
      filter(
        league_name == input$information_player_league_selection,
        team_name == input$information_player_team_selection,
        player_name == input$information_player_player_selection,
        league_season ==  as.numeric(
          str_split(input$information_player_season_selection,
                    pattern = "/")[[1]][1]
        )
      ) %>%
      data.frame() %>%
      unique()
  })
  
  
  # create the output for the table on the overview page 
  output$info_player_overview <- renderUI({
    # there has to be a player, team, league, season selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)
    
    # create a text output for the basic infos of selected player
    HTML(paste(
      paste0("<b>", "Name: ", "</b>", unique(player_infos()$player_name)),
      paste0(
        "<b>",
        "Country: ",
        "</b>",
        unique(player_infos()$player_nationality) # extract player nationality 
      ),
      paste0("<b>", "Club: ", "</b>",  unique(player_infos()$team_name)), # extract player club 
      paste0(
        "<b>",
        "Position: ",
        "</b>",
        unique(player_infos()$player_position) # extract player position 
      ),
      paste0("<b>", "Foot: ", "</b>" , unique(player_infos()$player_foot)), # extract whether player use right or left foot 
      paste0(
        "<b>",
        "Market value: ", # extract the market value of player
        "</b>",
        unique(player_infos()$player_market_value_in_million_euro),
        " Million Euro"
      ),
      paste0(
        "<b>",
        "Previous club: ",  # extract the previous club of player
        "</b>" ,
        unique(player_infos()$player_previous_club) 
      ),
      sep = "<br/>"
    ))
    
  })
  
  # output for the player image
  output$info_player_player_img <- renderUI({
    # we need the user to select a player first
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)
    
    print(input$information_player_player_selection)
    
    # extract image from the data
    player_image <- all_leagues_player_stats %>%
      filter(
        league_name == input$information_player_league_selection &
          player_name == input$information_player_player_selection
      ) %>%
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
  
# valuebox for the fast facts of players
  filter_player_data <- reactive({
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)  
    
    # filter the huge data frame on the selected inputs
    player_tab_data %>%
      filter(league_name ==  input$information_player_league_selection,
             team_name == input$information_player_team_selection,
             league_season ==  as.numeric(str_split(input$information_player_season_selection,
                                                    pattern = "/")[[1]][1]),
             player_name == input$information_player_player_selection)
  })
  
  # get the joining date info
  output$joining_date <- renderValueBox({
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
      color = "light-blue",
      width = 3
    )
  })
  
  # get the contract date info
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
      color = "light-blue",
      width = 3
    )
  })
  
  # get the age info
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
      color = "purple",
      width = 3
    )
  })
  
  # get the height info
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
      color = "blue",
      width = 3
    )
  })
  
  # get the mean game minute of selected player in selected season
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
      color = "light-blue",
      width = 3
    )
  })
  
  # get the mean passes accuracy of selected player in selected season
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
  
  # get the total yellow card of selected player in selected season
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
      color = "navy",
      width = 3
    )
  })
  
  # get the total red card of selected player in selected season
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
  
  # get the total game substitute of selected player in selected season
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
      color = "navy",
      width = 3
    )
  })
  
  # get the total goals assist of selected player in selected season
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
      color = "light-blue",
      width = 3
    )
  })
  
  # get the total scored penalty of selected player in selected season
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
      color = "blue",
      width = 3
    )
  })
  
  # get the total saved penalty of selected player in selected season
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
      color = "purple",
      width = 3
    )
  })
  
################### statistics tab of player tab ###################
  # radarplot for the summary of selected player stats in selected season
  output$info_player_stats_radarplot <- renderPlot({
    # we need the user to select a player first
    # there has to be a player selected
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)
    
    # select the infos to present in the radarplot
    data <-
      filter_player_data() %>% select(
        goals_total,
        dribbles_success,
        shots_total,
        tackles_total,
        fouls_committed ,
        passes_total  ,
        duels_total
      ) 
    
    colnames(data) <- c("total goals" , "success dribbles" , "total shots" , "total tackles" , "committed fouls", "total passes" , "total duels")
    
    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    data <-
      rbind(rep(max(data, na.rm = TRUE), 10) , rep(0, 10) , data)
    
    # Custom the radarChart 
    radarchart(
      data  ,
      axistype = 1 ,
      
      #custom polygon
      pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5),
      plwd = 5 ,
      
      #custom the grid
      cglcol = "orange",
      cglty = 1,
      axislabcol = "peru",
      caxislabels = seq(0, 20, 5),
      cglwd = 0.8,
      
      #custom labels
      vlcex = 0.8
    )
  }, height = 300, bg = "#567895") # set the height and background color

  
# create stats tables for different type of stats 
# firstly filter the data of seleted league, season and player
  player_stats <- reactive({
    all_leagues_player_stats %>%
      filter(
        league_name == input$information_player_league_selection &
          player_name == input$information_player_player_selection &
          league_season == as.numeric(
            str_split(input$information_player_season_selection,
                      pattern = "/")[[1]][1]
          )
      )
  })
  
  # create the table output for the stats
  output$info_player_stats_general_games <- renderReactable({
    # require selected team, league, season and player 
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)     
    
    # collect the games infos like games minutes, substitute...
    player_stats() %>%
      select(team_name, fixture_date,
             contains("games")) %>%
      reactable(
        # create a reactable table
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
  
  
  # similarly, create the table output for the goals stats
  output$info_player_stats_general_goals <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)      

    player_stats() %>%
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
          borderColor = "#BFC9D1",
          color = "#BFC9D1",
          backgroundColor = "#567895",
          highlightColor = "#CE8B65",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%")
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
 
  
  # similarly, create the table output for the shots stats
  output$info_player_stats_general_shots <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)
    
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
          borderColor = "#BFC9D1",
          color = "#BFC9D1",
          backgroundColor = "#567895",
          highlightColor = "#CE8B65",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%")
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
  
  
  # similarly, create the table output for the passes stats
  output$info_player_stats_general_passes <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)
    
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
          borderColor = "#BFC9D1",
          color = "#BFC9D1",
          backgroundColor = "#567895",
          highlightColor = "#CE8B65",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%")
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
  
  # similarly, create the table output for the duels stats
  output$info_player_stats_general_duel <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)
    
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
          borderColor = "#BFC9D1",
          color = "#BFC9D1",
          backgroundColor = "#567895",
          highlightColor = "#CE8B65",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%")
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
  
  # similarly, create the table output for the dribbles stats
  output$info_player_stats_general_dribbles <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)      
    
    player_stats() %>%
      select(
        fixture_date,
        tackles_total,
        tackles_blocks,
        tackles_interception,
        contains("dribbles")
      ) %>%
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
          borderColor = "#BFC9D1",
          color = "#BFC9D1",
          backgroundColor = "#567895",
          highlightColor = "#CE8B65",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%")
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
          tackles_interception = colDef(name = "Interceptions",
                                         align = "center")
          
        )
      )
  })
  
  # similarly, create the table output for the cards and penalty stats
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
          borderColor = "#BFC9D1",
          color = "#BFC9D1",
          backgroundColor = "#567895",
          highlightColor = "#CE8B65",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%")
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
  
  # similarly, create the table output for the offsides stats
  output$info_player_stats_general_offsides <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    req(input$information_player_league_selection)
    req(input$information_player_season_selection)      
    
    
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
          borderColor = "#BFC9D1",
          color = "#BFC9D1",
          backgroundColor = "#567895",
          highlightColor = "#CE8B65",
          cellPadding = "8px 12px",
          style = list(color = "white"),
          searchInputStyle = list(width = "100%")
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
  
  # create the plotly output for the stats rating
  output$info_player_stats_rating <- renderPlotly({
    # require selected player
    req(input$information_player_player_selection)
    req(input$information_player_league_selection)
    
    # filter the selected data and transfer the data to the longer form
    # so that we can plot in the plotly with different colors
    data <- all_leagues_fifa_squads %>%
      filter(
        league == input$information_player_league_selection &
          fifa_player_name == input$information_player_player_selection
      ) %>%
      pivot_longer(
        fifa_player_overall_rating:fifa_player_market_value_mil_euro,
        names_to = "variable",
        values_to = "value"
      )
    
    fifa_player_rating <- data %>%
      # create actual plot for the fifa player rating
      plot_ly(
        x = ~ date,
        y = ~ value,
        color =  ~ variable,
        type = "scatter" # ,
        # visible = "legendonly"
      ) %>%
      layout(
        title = list(text = "Fifa player rating and market value over time", y = 0.95, x = 0.5),
        yaxis = list(title = c("Rating", "Market value")),
        xaxis = list(title = "Year"),
        font = list(color = "white"),
        plot_bgcolor = "#567895",
        paper_bgcolor = "#567895",
        fig_bg_color = "#567895"
      )
    
    fifa_player_rating
  })
  
##################### transfer infos of players ####################
  # create a table for all transfers the player had so far during his career
  # output$info_player_transfers <- renderReactable({
  #   # require the selected player
  #   req(input$information_player_player_selection)
  #   req(input$information_player_team_selection)
  #   
  #   # some player name has only one word, we should distinguish the name length to avoid the warning
  #   if (str_count(input$information_player_player_selection, ' ') >= 1) {
  #     last_name <-
  #       str_split(input$information_player_player_selection, " ")[[1]][-1]
  #   } else if (str_count(input$information_player_player_selection, ' ') == 0) {
  #     # name with only one word
  #     last_name <-
  #       input$information_player_player_selection
  #   }
  #   
  #   # get the transfer infos for the selected player and team
  #   all_leagues_team_transfers  %>%
  #     filter(  # filter the selected player and the selected team so that we have correct data
  #       player_name %like% last_name,
  #       from_team_name == input$information_player_team_selection |
  #         to_team_name == input$information_player_team_selection
  #     ) %>%
  #     select(
  #       transfer_date,
  #       player_name,
  #       transfer_type,
  #       from_team_name,
  #       to_team_name,
  #       transfer_sum_mil_euro
  #     ) %>%
  #     distinct() %>%
  #     arrange(desc(transfer_date)) %>%
  #     reactable(
  #       defaultColDef = colDef(
  #         align = "center",
  #         minWidth = 150,
  #         headerStyle = list(background = "darkblue")
  #       ),
  #       striped = TRUE,
  #       highlight = TRUE,
  #       borderless = TRUE,
  #       # set the theme for the table
  #       theme = reactableTheme(
  #         borderColor = "#000000",
  #         color = "#000000",
  #         backgroundColor = "#004157",
  #         highlightColor = "#2f829e",
  #         cellPadding = "8px 12px",
  #         style = list(color = "white")
  #       ),
  #       # modify the layout and names of the columns
  #       columns = list(
  #         transfer_date = colDef(name = "Date",
  #                                align = "left"),
  #         player_name = colDef(name = "Player",
  #                              align = "center"),
  #         transfer_type = colDef(name = "Type",
  #                                align = "center"),
  #         transfer_sum_mil_euro = colDef(name = "Money (million)",
  #                                        align = "center"),
  #         from_team_name = colDef(name = "From Team",
  #                                 align = "center"),
  #         to_team_name = colDef(name = "To Team",
  #                               align = "center")
  #       )
  #     )
  # })
  
}
