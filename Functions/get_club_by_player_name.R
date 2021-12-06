############## get_club_by_player_name #################
# inputs: player, season_start
# outputs: should return the club (or clubs) the player played/plays for
# in a given season
# example: player <- "Manuel Neuer",
#          season_start <- 2021
# -> The function returns "FC Bayern Munich"
get_club_by_player_name <- function(player, season_start){
  # filter the squads from 2010 to 2021
  club_name <- all_seasons_from_2010_squads %>%
    # for the player name and the season
    filter(player_name == player,
           season == season_start) 
  
  # it is possible that the player left the club during the season
  # transfermarkt does not cover this problem. This is why
  # we have to cover it ourselves.
  # If there is no match for the current season
  if(nrow(club_name) == 0){
    club_name <- all_seasons_from_2010_squads %>%
      # for the player name and the season
      filter(player_name == player,
             # we search for the player in the last season
             season == (season_start - 1))
  }
  
  club_name <- club_name %>%
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