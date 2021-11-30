############## get_club_by_player_name #################
# inputs: player, season_start
# outputs: should return the club (or clubs) the player played/plays for
# in a given season
# example: player <- "Manuel Neuer",
#          season_start <- 2021
# -> The function returns "FC Bayern Munich"
get_club_by_player_name <- function(player, season_start){
  # filter the squads from 2010 to 2021
  club_name <- squads_by_season_2010_2021 %>%
    # for the player name and the season
    filter(player_name == player,
           season == season_start) %>%
    # extract only the club name and convert it into a vector
    select(club_name) %>%
    unique() %>%
    unlist() %>%
    unname() 
  
  # if the club_name is empty, the player was not found
  # we have to give this information to the user
  if(length(club_name) == 0){
    print(paste("Player", player, "not found in league", league,
                "in season", season_start, ".",
                "Verify that your input is correct."))
    return(NULL)
  } else {
    # otherwise return the club name
    return(club_name)
  }
}