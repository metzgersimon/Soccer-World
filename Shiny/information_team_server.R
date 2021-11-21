# subserver for the team-tab in the information menu item
information_team_server <- function(input, output, session){
  # create the output for the table on the overview page
  output$info_team_team_name <- function(){
    # there has to be a club selected
    req(input$info_team_club_selection)
    
    # filter the huge data frame on the club selected
    team_infos <- season_players_joined %>%
      filter(club == input$info_team_club_selection)
    
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
      select(`country.y`) %>%
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
      select(age) %>%
      summarize(avg_age = mean(age, na.rm = TRUE)) %>%
      # extract the value (it is a data frame)
      pull() %>%
      # round the value
      round(digits = 2)
    
    # extract the information for the stadium
    venue_info <- team_infos %>%
      select(venue_name, venue_city,
             venue_capacity) %>%
      unique()
        
    # combine the selected data (only name, league and country of the club) 
    # to a data frame
    club_info_frame <- data.frame(name, league, country) 
    # create additional rows to be able to merge the frames later
    club_info_frame[, (ncol(club_info_frame)+1):(ncol(club_info_frame)+2)] <- NA
    # transpose the frame such that the columns are now rows
    club_info_frame <- club_info_frame %>% 
      t() 
    
    # create the texts for the table
    squad_size_text <- paste0("Squad size: ", squad_size)
    avg_age_text <- paste0("Average squad age: ", avg_age)
    venue_name_text <- paste0("Venue: ", venue_info$venue_name)
    venue_city_text <- paste0("\t", venue_info$venue_city)
    venue_capa_text <- paste0("\t", venue_info$venue_capacity, " seats")
    
    # put the texts together into a data frame
    add_info_frame <- data.frame(squad_size_text,
                                 avg_age_text,
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
    # we need the user to select a club first
    req(input$info_team_club_selection)
    
    # extract the logo from the frame
    team_image <- bundesliga_2020_infos %>%
      filter(team_name == input$info_team_club_selection) %>%
      select(logo) %>%
      unlist() %>%
      unname()
      
    # set the img on the extracted logo
    tags$img(src = team_image)
  })
  
  # create the table for the squad
  output$info_team_squad <- renderReactable({
    # we need the user to select a club first 
    req(input$info_team_club_selection)
    
    # create the table
    season_players_joined %>%
      # filter the data for the selected club
      filter(club == input$info_team_club_selection) %>%
      # selected only columns of interest
      select(c(player_name, country, position, age, 
               joining_date, contract_date, market_value_in_million_euro)) %>%
      # create the actual table
      reactable(
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
          player_name = colDef(name = "Name",
                               align = "left"),
          country = colDef(name = "Nationality",
                           align = "center"),
          position = colDef(name = "Position",
                            align = "center"),
          age = colDef(name = "Age",
                       align = "center"),
          joining_date = colDef(name = "Joining Date",
                                align = "center"),
          contract_date = colDef(name = "Contract till",
                                 align = "center"),
          market_value_in_million_euro = colDef(name = "Market value",
                                                align = "right")
        ))
  })
}