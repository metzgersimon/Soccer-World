# main server function
server <- function(input, output, session){
  # call the sub-servers for the different pages
  home_server(input, output, session)
  
  information_league_server(input, output, session)
  
  information_team_server(input, output, session)
  
  information_player_server(input, output, session)
  

}