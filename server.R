# main server function
server <- function(input, output, session){
  # call the sub-servers for the different pages
  home_server(input, output, session)
  
  wiki_server(input, output, session)

  information_league_general_server(input, output, session)
  
  information_league_match_server(input, output, session)
  
  information_team_server(input, output, session)
  
  information_player_server(input, output, session)
  
  prediction_model_server(input, output, session)
  
  about_server(input, output, session)
  
  
}