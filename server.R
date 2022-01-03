# main server function
server <- function(input, output, session){
  # call the sub-servers for the different pages
  home_server(input, output, session)
  
  information_league_general_server(input, output, session)
  
  information_league_match_server(input, output, session)
  
  information_team_server(input, output, session)
  
  information_player_server(input, output, session)
  
  prediction_investment_server(input, output, session)
  
}