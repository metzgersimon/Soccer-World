information_player_server <- function(input, output, session){
  # observeEvent(input$information_player_league_selection, {
  #   updateSelectInput(session, 
  #                     inputId = "information_player_team_selection",
  #                     choices = unique(fifa_team_stats_over_time_clean %>%
  #                                        filter(league == input$information_player_league_selection) %>%
  #                                        select(club) %>%
  #                                        unlist() %>%
  #                                        unname())
  #                     )
  # })
  
  
  
  # create an observer to display for the club selection to display
  # only those players that are present in the selected club
  observeEvent(input$information_player_team_selection, {
    updateSelectInput(session, 
                      inputId = "information_player_player_selection",
                      choices = unique(season_players_joined %>%
                                         filter(club == input$information_player_team_selection,
                                                season_start_year == as.numeric(
                                                  str_split(input$information_player_season_selection,
                                                            pattern = "/")[[1]][1])) %>%
                                         select(player_name) %>%
                                         unlist() %>%
                                         unname()),
                      selected = ""
    )

  })
  
  
  
  # create the output for the table on the overview page
  output$info_player_player_name <- renderTable({
    # there has to be a player selected
    req(input$information_player_player_selection)

    # filter the data for the selected club and the selected player
    player_infos <- season_players_joined %>%
      filter(club == input$information_player_team_selection,
             player_name == input$information_player_player_selection,
             season_start_year == max(season_start_year)) %>%
      select(player_name, country.x, position, birth_date,
             age, height, joining_date, contract_date,
             market_value_in_million_euro, club,
             league) %>%
      data.frame() %>%
      unique()
    
    player_infos <- season_players_joined %>%
      filter(club == "FC Bayern Munich",
             player_name == "Manuel Neuer",
             season_start_year == max(season_start_year)) %>%
      select(player_name, country.x, position, birth_date,
             age, height, joining_date, contract_date,
             market_value_in_million_euro, club,
             league) %>%
      data.frame() %>%
      unique()

    
    # extract the name of the player
    name <- unique(player_infos$player_name)
    
    # extract the league of the player
    league <- unique(player_infos$league) 
    
    # extract the nationality of the player
    country <- unique(player_infos$`country.x`)
    
    # extract the position the player plays on
    position <- unique(player_infos$position)
    
    # extract the age of the player
    age <- unique(player_infos$age)
    
    # extract the birth date of the player
    birth_date <- unique(player_infos$birth_date)
    
    # extract the height of the player
    height <- unique(player_infos$height)
    
    # extract the date the player joined its current club
    joining_date <- unique(player_infos$joining_date)
    
    # extract the market value of the player
    market_value <- unique(player_infos$market_value_in_million_euro)
    
   
    # create the texts for the table
    country_text <- paste0("Country: ", country)
    position_text <- paste0("Position: ", position)
    age_text <- paste0("Age: ", age)
    height_text <- paste0("Height: ", height)
    joining_date_text <- paste0("Joined: ", joining_date)
    market_value_text <- paste0("Market value: ", market_value)
    
    # put all the information together into a data frame
    # with 2 columns and 3 rows
    player_info_frame <- data.frame(matrix(c(country_text, position_text, 
                                           age_text, height_text, 
                                           joining_date_text,
                                           market_value_text), ncol = 2,
                                           nrow = 3,
                                           byrow = TRUE)
                                    ) 
   
    # set the NA format in a kable table to an empty string 
    options(knitr.kable.NA = "")
    
    # create a kable table with the data
    player_info_frame %>%
      kableExtra::kable("html", row.names = FALSE, col.names = NULL
      ) %>%
      kable_minimal() %>%
      kable_styling(full_width = F)
    
    
  })
  
  
  # output for the club logo
  output$info_player_player_img <- renderUI({
    # we need the user to select a club first
    req(input$information_player_player_selection)
    
    print(input$information_player_player_selection)
    # extract image from the data
    player_image <- player_stats_2021_buli %>%
      filter(name %like% input$information_player_player_selection) %>%
      select(photo) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    print(player_image)
    
    # set the img on the extracted image
    tags$img(src = player_image)
  })
  
  
  # create the table output for the stats
  output$info_player_stats_general <- renderReactable({
    req(input$information_player_player_selection)
    
    last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    # last_name <- "Baumann"
    
    
    player_stats_2021_buli %>%
      # filter(name == input$information_player_player_selection) %>%
      filter(lastname %like% last_name) %>%
      select(team_name, 
             contains("games"),
             contains("substitutes")) %>%
      reactable(sortable = TRUE,
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
                )#, 
                # modify the layout and names of the columns
      #           columns = list(
      #             date = colDef(name = "Date",
      #                           align = "left"),
      #             player_name = colDef(name = "Player",
      #                                  align = "center"),
      #             type = colDef(name = "Type",
      #                           align = "center"),
      #             from_team_name = colDef(name = "From Team",
      #                                     align = "center"),
      #             to_team_name = colDef(name = "To Team",
      #                                   align = "center")
      #           )
      # )
  })
  
  
  # create the table output for the stats
  output$info_player_stats_matches <- renderReactable({
    req(input$information_player_player_selection)
    
    last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    # last_name <- "Baumann"
    
    
    player_stats_2021_buli %>%
      # filter(name == input$information_player_player_selection) %>%
      filter(lastname %like% last_name) %>%
      select(team_name, shots_total:penalty_saved) %>%
      reactable(sortable = TRUE,
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
      )#, 
    # modify the layout and names of the columns
    #           columns = list(
    #             date = colDef(name = "Date",
    #                           align = "left"),
    #             player_name = colDef(name = "Player",
    #                                  align = "center"),
    #             type = colDef(name = "Type",
    #                           align = "center"),
    #             from_team_name = colDef(name = "From Team",
    #                                     align = "center"),
    #             to_team_name = colDef(name = "To Team",
    #                                   align = "center")
    #           )
    # )
  })
  
  
  # create a table for all transfers the player had so far during his career
  output$info_player_transfers <- renderReactable({
    req(input$information_player_player_selection)
    req(input$information_player_team_selection)
    
    last_name <- str_split(input$information_player_player_selection, " ")[[1]][-1]
    
    all_transfers %>%
      filter(player_name %like% last_name,
             from_team_name == input$information_player_team_selection |
               to_team_name == input$information_player_team_selection) %>%
      select(date, player_name, type, from_team_name,
             to_team_name) %>%
      distinct() %>%
      arrange(desc(date)) %>%
      reactable(sortable = TRUE,
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