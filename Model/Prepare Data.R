# function should prepare the venue data frame to be able to be used in the model.
prepare_venue_data <- function(){
  venues <- venues_with_coordinates %>%
    # drop unnecessary variables
    select(-c(team_name, country, national, logo, venue_name, 
              venue_address, venue_city, venue_surface, venue_image))
  return(venues)
}

############# pseudo code cronjob lineup ############
# check for the latest available matchday
latest_matchday <- buli_matches %>%
  filter(league_season == current_season,
         !is.na(fulltime_score_home) &
           !is.na(fulltime_score_away)) %>%
  filter(league_round == max(league_round)) %>%
  select(league_round) %>%
  pull() %>%
  unique()

# get the newest matchday
newest_matchday <- latest_matchday + 1

next_matches <- buli_matches_2010_2021 %>%
  filter(fixture_date >= Sys.Date(),
         league_round == newest_matchday)




# function should map the (historical) transfermarkt lineups data with
# the lineup information of a soon to begin match from the API
prepare_lineup_data <- function(match_id, max_season = 2021){
  # match_id <- 719494
  match_information <- buli_matches_2010_2021 %>%
    filter(fixture_id == match_id) %>%
    mutate(fixture_date = ymd(fixture_date)) %>%
    data.frame()
  
  # get the lineups from the API
  # live call to API to get the lineups for the soon to begin lineup
  # lineups_API <- get_fixture_lineups(match_id) %>%
  lineups_API2 <- lineups_API %>%
    # prepare the name of the players to map the API and the TM data
    mutate(player_lastname = str_remove(player_name, pattern = ".*\\. |.*\\s"),
           # map all special characters to plain ones
           player_lastname2 = stri_trans_general(player_lastname, id = "Latin-ASCII"))
  

  # check if the current matchday is the first matchday of the season
  # If it is the first matchday, we want to impute the data somehow
  if(match_information$league_round == 1){
      # in solchen fällen den durchschnittswert der letzten mannschaft (absteiger)
    lineups_tm <- buli_fixture_lineups_2015_2021_tm %>%
      filter(season == match_information$league_season - 1)
  # if it is not the first matchday, we get the current season
  } else {
    lineups_tm <- buli_fixture_lineups_2015_2021_tm %>%
      filter(season == match_information$league_season)
  }
  
  # search in the current season lineup data from transfermarkt for the
  # current teams (historical data)
  lineups_tm <- lineups_tm %>%
    filter(team %in% c(match_information$club_name_home,
                       match_information$club_name_away),
           matchday < match_information$league_round) %>%
    mutate(player_lastname = str_to_title(sub(".*?\\s", "", player_name)),
           # map all special characters to plain ones
           player_lastname2 = stri_trans_general(player_lastname, id = "Latin-ASCII")) %>%
    mutate(player_lastname2 = ifelse(player_name == "Fabian Kunze", "Affe", player_lastname2))
    
    
    ######## if a player is missing then we want to impute the values for this player
    # based on the averages for this particular position and team
  
  # first check if there are any players that did not match from the API (newest data)
  # to the transfermarkt
  not_matched_players <- lineups_API2 %>%
    anti_join(lineups_tm, by = c("team_name" = "team", "player_number",
                                 "player_lastname2")) 
  
  # we also want to join the all the other players
  matched_players <- lineups_API2 %>%
    # join by the team name, the player number and the (cleaned) lastname
    inner_join(lineups_tm, by = c("team_name" = "team", "player_number",
                                 "player_lastname2")) %>%
    # create team, number and lastname pairs to get the latest information
    # for every pair from transfermarkt
    group_by(team_name, player_number, player_lastname2) %>%
    filter(match_date == max(match_date)) %>%
    ungroup() %>%
    # do the final variable transformations and ordering
    rename(player_name_API = `player_name.x`,
           player_pos_API = `player_pos.x`,
           player_lastname_to_map = player_lastname2,
           player_name_TM = `player_name.y`,
           player_pos_TM = `player_pos.y`) %>%
    # create a new variable for the match id
    mutate(fixture_id = match_id)
  
  
  # find the player match stats that are relevant for the current match
  player_stats <- buli_player_fixture_stats %>%
    mutate(player_lastname = str_to_title(sub(".*?\\s", "", player_name)),
           # map all special characters to plain ones
           player_lastname_map = stri_trans_general(player_lastname, id = "Latin-ASCII")) %>%
    filter(league_season == match_information$league_season,
           league_round < match_information$league_round,
           team_name %in% c(match_information$club_name_home,
                            match_information$club_name_away))
  
  
  # if not_matched_players is not empty, i.e., there is at least one player
  # that is now, i.e., not in the historical data of transfermarkt
  if(nrow(not_matched_players) != 0){
    # search for the player in the player_fixture_stats of the league
    missing_player_stats <- buli_player_fixture_stats %>%
      filter(league_season == match_information$league_season,
             player_id == not_matched_players$player_id) %>%
      filter(fixture_date == max(fixture_date))
    
    # if the missing_player_stats is empty that means that the player was not previously
    # playing in one of the leagues we consider. Therefore we impute its values 
    # with information about its current position and team
    if(nrow(missing_player_stats) == 0){
      # get the average player stats of the current team for every position
      missing_player_stats_imputed <- player_stats %>%
        group_by(team_id, games_position) %>%
        summarize(across(c(games_minutes, games_rating, goals_total:cards_red),
                         ~ median(.x, na.rm = TRUE))) %>% 
        ungroup()
      
      # join the data of players that did not match with the statistics we
      # imputed for those players from the team averages
      missing_player_stats <- not_matched_players %>%
        inner_join(missing_player_stats_imputed, 
                   by = c("team_id", 
                          "player_pos" = "games_position"))
        
    }
  }
  
  # now get the player stats for all players that are in the current lineup
  
  
  ####################### TO DO ###########################
  # lineup_tm daten ebenfalls imputen für spieler, die nicht vorhanden sind
  # und diese an die player_stats joinen
  # Stats für die lineups berechnen (missing_players) vor aggregierung einfügen
  player_stats_lineup <- player_stats %>%
    filter(player_id %in% matched_players$player_id) %>%
    group_by(team_id, games_position) %>%
    summarize(across(c(games_minutes, games_rating, goals_total:cards_red),
                     ~median(.x, na.rm = TRUE)))

  
  
  player_data_with_stats <- players_list2 %>%
    left_join(., player_agg_data_up_to_match,
              by = c("team_id",  "player_id")) %>%
    select(-c(#player_lastname, 
      player_lastname.x, player_lastname.y))
  
}