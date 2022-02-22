# subserver for the home menu item
home_server <- function(input, output, session) {
  
  # create the map of home
  output$map <- renderLeaflet({
    
    # create the columns which contains league and popup text info
    venues_with_coordinates_home <-
      all_leagues_venue_information %>% mutate(
        league = if_else(
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
        ),
        popup_text = paste0(
          "<center>",
          #Setting up poopup info
          "</br><b>Team Name</b>: ",
          team_name,
          "</br><b>Country</b>: ",
          country,
          "</br><b>Founded Year</b>: ",
          founded,
          "</br><b>Venue Name</b>: ",
          venue_name
        )
      ) %>%  # filter the selected leagues and the latest clubs info  
      filter(league == input$leagues, season==max(season,na.rm = TRUE))
    
    # make the icon to be the team flag
    flagIcon <- makeIcon(
      iconUrl =  venues_with_coordinates_home$logo,
      iconWidth = 30,
      iconHeight = 30,
      shadowWidth = 10,
      shadowHeight = 10
    )
    
    # leaflet map 
    leaflet(venues_with_coordinates_home) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>% clearMarkerClusters() %>%
      addMarkers(
        ~ longitude, # add the location 
        ~ latitude,
        label = ~ team_name,      
        icon = flagIcon,          # add the icon 
        labelOptions = labelOptions(textsize = "12px"),
        popup = ~ popup_text    # add popup text
      )
    
  })
  
  # output for the club logo
  output$home_league_flag <- renderUI({
    # we need the user to select a club first
    req(input$leagues)
    
    # extract the logo from the frame
    leagues_image <- all_leagues_club_stats %>%
      filter(league_name == input$leagues) %>%
      select(league_logo) %>%
      unlist() %>%
      unname() %>%
      unique()
    
    # set the img on the extracted logo
    tags$img(src = leagues_image)
  })
}