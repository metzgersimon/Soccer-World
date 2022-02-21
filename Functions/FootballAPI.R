############## get_api_calls_left #################
# function should return the number of calls that are still available 
# for the current day
get_api_calls_left <- function(){
  # set the endpoint for the status
  endpoint <- "https://v3.football.api-sports.io/status"
  
  # create a get request to that API with the API key
  # and the selected parameters (league via league_id and season)
  response <- GET(endpoint, 
                  add_headers('x-apisports-key' = football_api_key))
  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(response) >= 200 & status_code(response) < 300){
    # extract the content from the response
    content <- content(response)$response
    
    # get the requests list of the content
    requests_info <- content$requests
    
    # compute the requests that are still available today
    calls_left <- as.numeric(requests_info$limit_day) - 
      as.numeric(requests_info$current)
    
    # get the calls we already used
    # if the request was not successful print an error 
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(response)))
  }
  
  return(calls_left)
}



############## get_team_information_in_league #################
# inputs: league_id, season
# outputs: should return a data frame containing information about all
# teams that are in the given league for the selected season
# example: founding year of the club, stadium information of the club, etc.
get_team_information_in_league <- function(league_id, season){
  # to be able to avoid problems, we convert the season parameter into a numeric
  # in case it is given as a string
  season <- as.numeric(season)
  
  # set the endpoint of the API
  endpoint <- "https://v3.football.api-sports.io/teams"
  
  # create a get request to that API with the API key
  # and the selected parameters (league via league_id and season)
  response <- GET(endpoint, 
                  add_headers('x-apisports-key' = football_api_key),
                  query = list(league = league_id,
                               season = season))
  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(response) >= 200 & status_code(response) < 300){
    # extract the content from the response
    content <- content(response)$response
    
    # compute the number of teams to pre-allocate the data frame
    number_of_teams <- length(content)
    
    # pre-allocate the data frame
    team_info_frame <- data.frame(team_id = integer(number_of_teams),
                                  team_name = character(number_of_teams),
                                  federal_state = character(number_of_teams),
                                  country = character(number_of_teams),
                                  founded = integer(number_of_teams),
                                  national = logical(number_of_teams),
                                  logo = character(number_of_teams),
                                  venue_id = integer(number_of_teams),
                                  venue_name = character(number_of_teams),
                                  venue_address = character(number_of_teams),
                                  venue_city = character(number_of_teams),
                                  venue_capacity = integer(number_of_teams),
                                  venue_surface = character(number_of_teams),
                                  venue_image = character(number_of_teams))
    
    # iterate through all teams
    for(i in 1:number_of_teams){
      # get the team infos for the current team
      curr_team_infos <- unname(unlist(content[[i]]))
      # and insert it into the i-th row of the data frame
      team_info_frame[i,] <- curr_team_infos
    }
    
    # after the loop integrate variables for the league (given by league_id)
    # and the season
    team_info_frame <- team_info_frame %>%
      mutate(league_id = league_id,
             season = season) %>%
      # reorder the data frame
      select(league_id, season, everything())
    
    # map the club names
    team_info_frame$team_name <- sapply(team_info_frame$team_name,
                                        club_name_mapping)
    
    # if the request was not successful print an error 
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(response)))
  }
  
  # return the data frame containing information about all teams
  # in the league for the given season
  return(team_info_frame)
}



############## get_team_stats_in_league_by_season #################
# inputs: league_id, team_id, season
# outputs: should return a list where each element represents a statistic about
# a given team in a given league and season
# example: form of the team, average goals, etc.
get_team_stats_in_league_by_season <- function(league_id, team_id, season, match_dates){
  
  # take only those match_dates that are in the past because otherwise there is no
  # data available
  match_dates <- ymd(match_dates)
  match_dates <- match_dates[match_dates < Sys.Date()]
  
  # set the endpoint of the API
  endpoint <- "https://v3.football.api-sports.io/teams/statistics"
  
  # create an empty variable to store the team stats over the whole season
  all_season_team_stats <- NULL
  
  # iterate over all (remaining) match dates to get the statistics
  for(i in 1:length(match_dates)){
    # create a get request to that API with the API key
    # and the selected parameters (league via league_id and season)
    # and the current date
    response <- GET(endpoint, 
                    add_headers('x-apisports-key' = football_api_key),
                    query = list(league = league_id,
                                 team = team_id,
                                 season = season,
                                 date = match_dates[i]))
    
    
    # check if the request was successful and only then go on with the 
    # transformation of the data
    if(status_code(response) >= 200 & status_code(response) < 300){
      # extract the content from the response
      content <- content(response)$response
      
      
      if(is.null(content)){
        return(NULL)
      }
      
      # extract the league information by using the helper function
      # get_content_for_list_element
      league_info <- get_content_for_list_element(content, "league") %>%
        api_football_fixtures_general_complete_check(., "team_stats_league_info") %>%
        fill_empty_with_na(.) %>%
        mutate(league_id = as.numeric(league_id),
               league_season = as.numeric(league_season),
               league_name = as.character(league_name),
               league_country = as.character(league_country),
               league_logo = as.character(league_logo),
               league_flag = as.character(league_flag)) %>%
        # for the bundesliga we have to remap the name
        mutate(league_name = ifelse(league_name == "Bundesliga 1",
                                    "Bundesliga",
                                    league_name))
        
      
      # extract the team information by using the helper function
      # get_content_for_list_element
      team_info <- get_content_for_list_element(content, "team") %>%
        api_football_fixtures_general_complete_check(., "team_stats_team_info") %>%
        fill_empty_with_na(.) %>%
        mutate(team_id = as.numeric(team_id),
               team_name = as.character(team_name),
               team_logo = as.character(team_logo))
      
      # extract the form of the team
      form_info <- content$form %>%
        data.frame("current_form" = .)
      
      # extract the fixture information by using the helper function
      # get_content_for_list_element
      fixtures_info <- get_content_for_list_element(content, "fixtures") %>%
        api_football_fixtures_general_complete_check(., "team_stats_fixture_info") %>%
        fill_empty_with_na(.)
      
      # extract the goal information by using the helper function
      # get_content_for_list_element
      goal_info <- get_content_for_list_element(content, "goals") %>%
        api_football_fixtures_general_complete_check(., "team_stats_goal_info") %>%
        fill_empty_with_na(.) %>%
        # for those who are in % we remove the percentage-sign to be able
        # to convert the values into numeric
        mutate(across(contains("percentage"), ~ str_remove(.x, "%"))) %>%
        mutate(across(.cols = everything(), as.numeric))
        
      
      # extract the penalty information by using the helper function
      # get_content_for_list_element
      penalty_info <- get_content_for_list_element(content, "penalty") %>%
        api_football_fixtures_general_complete_check(., "team_stats_penalty_info") %>%
        fill_empty_with_na(.) %>%
        # for those who are in % we remove the percentage-sign to be able
        # to convert the values into numeric
        mutate(across(contains("percentage"), ~ str_remove(.x, "%"))) %>%
        mutate(across(.cols = everything(), as.numeric))
      
      # extract the biggest information by using the helper function
      # get_content_for_list_element
      biggest_info <- get_content_for_list_element(content, "biggest") %>%
        api_football_fixtures_general_complete_check(., "team_stats_biggest_info") %>%
        fill_empty_with_na(.) %>%
        # for those who are actual numeric values we want to convert into
        # numeric
        mutate(across(c(contains("streak"), contains("goals")), as.numeric),
               across(c(biggest_wins_home:biggest_loses_away), as.character))
        
      
      # extract the clean_sheet information by using the helper function
      # get_content_for_list_element
      clean_sheet_info <- get_content_for_list_element(content, "clean_sheet") %>%
        api_football_fixtures_general_complete_check(., "team_stats_clean_sheet_info") %>%
        fill_empty_with_na(.)
      
      # extract the failed_to_score information by using the helper function
      # get_content_for_list_element
      failed_to_score_info <- get_content_for_list_element(content,
                                                           "failed_to_score") %>%
        api_football_fixtures_general_complete_check(., "team_stats_failed_to_score_info") %>%
        fill_empty_with_na(.)
        
    
      # extract the cards information by using the helper function
      # get_content_for_list_element
      cards_info <- get_content_for_list_element(content, "cards") %>%
        api_football_fixtures_general_complete_check(., "team_stats_cards_info") %>%
        fill_empty_with_na(.) %>%
        # for those who are in % we remove the percentage-sign to be able
        # to convert the values into numeric
        mutate(across(contains("percentage"), ~ str_remove(.x, "%"))) %>%
        mutate(across(.cols = everything(), as.numeric))
        
      # add a variable for the matchday
      matchday_frame <- data.frame("matchday" = fixtures_info$fixtures_played_total)
      
      # bind together all the columns into one big data frame
      team_stats_complete <- bind_cols(league_info, matchday_frame, team_info, 
                                      fixtures_info, form_info, goal_info,
                                      penalty_info, biggest_info, clean_sheet_info, 
                                      failed_to_score_info, cards_info)
      
      # lastly map the club names
      team_stats_complete$team_name <- sapply(team_stats_complete$team_name,
                                              club_name_mapping)

      # bind the data of the current date to the overall team stats
      all_season_team_stats <- bind_rows(all_season_team_stats,
                                         team_stats_complete)
      
      Sys.sleep(2)
      
      # if the request was not successful print an error 
    } else {
      print(paste0("Error: The request was not successful. \nStatus code: ",
                   status_code(response)))
    }
    
  }
  
  # do some final mutations such as splitting the biggest_wins/loses columns
  # into a goal difference such that we can use it numerically
  all_season_team_stats <- all_season_team_stats %>%
  separate(col = biggest_wins_home, sep = "-",
           into = c("biggest_wins_home_goals_home",
                    "biggest_wins_home_goals_away"),
           convert = TRUE) %>%
    separate(col = biggest_wins_away, sep = "-",
             into = c("biggest_wins_away_goals_home",
                      "biggest_wins_away_goals_away"),
             convert = TRUE) %>%
    separate(col = biggest_loses_home, sep = "-",
             into = c("biggest_loses_home_goals_home",
                      "biggest_loses_home_goals_away"),
             convert = TRUE) %>%
    separate(col = biggest_loses_away, sep = "-",
             into = c("biggest_loses_away_goals_home",
                      "biggest_loses_away_goals_away"),
             convert = TRUE) %>%
    # create variables for the biggest goal differences for wins and loses
    # at home and away
    mutate(biggest_wins_home_diff = as.numeric(biggest_wins_home_goals_home) -
             as.numeric(biggest_wins_home_goals_away),
           biggest_wins_away_diff = as.numeric(biggest_wins_away_goals_away) -
             as.numeric(biggest_wins_away_goals_home),
           biggest_loses_home_diff = as.numeric(biggest_loses_home_goals_home) -
             as.numeric(biggest_loses_home_goals_away),
           biggest_loses_away_diff = as.numeric(biggest_loses_away_goals_away) -
             as.numeric(biggest_loses_away_goals_home)) %>%
    # drop the biggest variables we do not need anymore
    select(-c(biggest_wins_home_goals_home,
              biggest_wins_home_goals_away,
              biggest_wins_away_goals_away,
              biggest_wins_away_goals_home,
              biggest_loses_home_goals_home,
              biggest_loses_home_goals_away,
              biggest_loses_away_goals_away,
              biggest_loses_away_goals_home)) %>%
    # reoder variables
    select(contains("league"), matchday, contains("team"),
           contains("fixtures"), current_form, contains("goals"),
           contains("penalty"), contains("biggest"), contains("clean"),
           contains("failed"), contains("cards")) %>%
    # if available we want to drop the cards total columns
    select(-any_of(c("cards_yellow_total", "cards_yellow_percentage", 
                     "cards_red_total", "cards_red_percentage")))

  # return the list containing statistics about the given team
  # in the league for the given season
  return(all_season_team_stats)
  
}



############## get_fixtures_in_league_by_season #################
# inputs: league_id, season
# outputs: should return a data frame which contains all available
# fixtures in a league for a given season
# example: 
get_fixtures_in_league_by_season <- function(league_id, season, matchday = NULL){
  # set the endpoint of the API
  endpoint <- "https://v3.football.api-sports.io/fixtures"
  
  # create a get request to that API with the API key
  # and the selected parameter (league given by its id and the season)
  if(is.null(matchday)){
    response <- GET(endpoint, 
                    add_headers('x-apisports-key' = football_api_key),
                    query = list(league = league_id,
                                 season = season))
  } else {
    matchday <- paste0("Regular Season - ", matchday)
    response <- GET(endpoint, 
                    add_headers('x-apisports-key' = football_api_key),
                    query = list(league = league_id,
                                 season = season,
                                 round = matchday))
    
  }
  
  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(response) >= 200 & status_code(response) < 300){
    
    # extract the content from the response
    content <- content(response)$response
    
    # we break if the content is empty
    if(length(content) == 0){
      return(NULL)
    }
    
    # create empty variable to store the information
    all_fixture_information <- NULL
    
    # iterate through all fixtures
    for(i in 1:length(content)){
      print(i)
      # extract fixture information with the enframe function
      # to convert the fixture list into a tibble
      fixture_general_info <- enframe(unlist(content[[i]]$fixture)) 
      
      # extract the matchday of the current fixture
      matchday <- content[[i]]$league$round
      
      # only do this if the matchday is not the relegation
      if(matchday != "Relegation Round"){
        if("venue.name" %in% fixture_general_info$name){
          # if the venue.city column is empty and 
          # the name column contains the venue city in the venue.name in parantheses
          if(!("venue.city" %in% unique(fixture_general_info$name)) &
             str_detect(
               fixture_general_info$value[fixture_general_info$name == "venue.name"],
               pattern = "\\(|\\)"
             )
          ){
            # extract the row position where it contains the venue.name 
            row_position <- which(fixture_general_info$name == "venue.name")
            
            # split this row by the opening paranthesis into a new variable
            venue_city_split <- str_split(
              fixture_general_info[row_position, "value"],
              pattern = "\\(") 
            
            # extract the venue name
            fixture_venue_name <- venue_city_split %>%
              .[[1]] %>%
              .[1] %>%
              trimws()
            
            # and the venue city from this variable
            # and remove the parantheses
            fixture_venue_city <- venue_city_split %>%
              .[[1]] %>%
              .[-1] %>%
              .[1] %>%
              str_remove_all("\\(|\\)") %>%
              trimws()
            
            # set the value of the row to the venue name
            fixture_general_info[row_position, "value"] <- fixture_venue_name
            
            # and add a new row after the specific row extracted earlier with
            # the venue city
            fixture_general_info <- fixture_general_info %>%
              add_row(
                name = "venue.city",
                value = fixture_venue_city,
                .after = row_position
              )

          }
        }
      }
      
      
      # extract the general info of the fixture (id, date, etc.)
      fixture_general_info <- fixture_general_info %>%
        # use pivot_wider to transform the tibble from long to wide
        # so that we only got 1 row with multiple columns
        pivot_wider(names_from = name, values_from = value,
                    names_glue = "{name}") %>%
        unnest(cols = everything()) %>%
        data.frame() %>%
        # drop uncessessary columns
        select(-any_of(c("timestamp", "periods.first", "periods.second")))
      
      # check if the data frame contains all columns it should contain
      # with a helper function called "api_football_fixtures_general_complete_check"
      fixture_general_info <- 
        api_football_fixtures_general_complete_check(fixture_general_info,
                                                     content_type = "general") %>%
        data.frame() %>%
        # rename all variables appropriately
        rename(fixture_id = id,
               fixture_date = date,
               venue_id = `venue.id`,
               venue_name = `venue.name`,
               venue_city = `venue.city`,
               status_long = `status.long`,
               status_short = `status.short`,
               status_elapsed = `status.elapsed`) %>%
        # convert the date into an appropriate format (ymd_hms)
        # with the correct timezone
        mutate(fixture_date = ymd_hms(str_remove_all(fixture_date, pattern = "\\+.*"),
               tz = timezone),
               # and remove the country from the referee column
               referee = str_remove_all(referee, pattern = ",.*")) %>%
        # convert the date into the correct timezone
        mutate(fixture_date = with_tz(fixture_date, tzone = "Europe/Berlin")) %>%
        # separate the date column into date and time
        separate(col = fixture_date,
                 into = c("fixture_date", "fixture_time"),
                 sep = " ") %>%
        # remove the milliseconds 
        mutate(fixture_time = str_remove_all(fixture_time, 
                                             pattern = ":[0-9]+$")) %>%
        select(-timezone)
      
      # in case the frame is empty
      fixture_general_info <- check_for_value_column(fixture_general_info)
      
      # map the city names 
      fixture_general_info$venue_city <- sapply(fixture_general_info$venue_city,
                                                city_name_mapping)
        
      
      # extract league information helper function and convert it into
      # a data frame of one row and x columns
      fixture_league_info <- get_content_for_list_element(content[[i]], "league",
                                                          piv_wider = TRUE,
                                                          name_elem = "league_")
      
      # extract the information about the league
      fixture_league_info <- 
        # check if the data frame contains all columns it should contain
        # with a helper function called "api_football_fixtures_general_complete_check"
        api_football_fixtures_general_complete_check(fixture_league_info,
                                                     content_type = "league") %>%
        # extract only the number of the round from the league_round column
        mutate(league_round = str_extract(league_round, pattern = "[0-9]+"),
               league_name = ifelse(league_name == "Bundesliga 1",
                                    "Bundesliga",
                                    league_name))
      
      # in case the frame is empty
      fixture_league_info <- check_for_value_column(fixture_league_info)
      
      
      # extract team information helper function and convert it into
      # a data frame of one row and x columns
      fixture_team_info <- get_content_for_list_element(content[[i]], "teams",
                                                        piv_wider = TRUE,
                                                        name_elem = "")
        

      # extract the information about the team
      fixture_team_info <- 
        # check if the data frame contains all columns it should contain
        # with a helper function called "api_football_fixtures_general_complete_check"
        api_football_fixtures_general_complete_check(fixture_team_info,
                                                     content_type = "team") 
      
      # map the team names with the club_name_mapping function
      fixture_team_info$home_name <- sapply(fixture_team_info$home_name,
                                            club_name_mapping)
      
      fixture_team_info$away_name <- sapply(fixture_team_info$away_name,
                                            club_name_mapping)
      
      # rename all variables in the frame
      fixture_team_info <- fixture_team_info %>%
        # drop, if available these columns
        select(-any_of(c("home_winner", "away_winner"))) %>%
        rename(club_id_home = home_id,
               club_name_home = home_name,
               club_logo_home = home_logo,
               club_id_away = away_id,
               club_name_away = away_name,
               club_logo_away = away_logo) %>%
        # transform the numeric ones into numeric
        mutate(club_id_home = as.numeric(club_id_home),
               club_id_away = as.numeric(club_id_away))

      # in case the frame is empty
      fixture_team_info <- check_for_value_column(fixture_team_info)
      
      # extract score information helper function and convert it into
      # a data frame of one row and x columns
      fixture_score_info <- get_content_for_list_element(content[[i]], "score",
                                                         piv_wider = TRUE,
                                                         name_elem = "")
      
      # extract score specific information and clean it
      fixture_score_info <- 
        # check if the data frame contains all columns it should contain
        # with a helper function called "api_football_fixtures_general_complete_check"
        api_football_fixtures_general_complete_check(fixture_score_info,
                                                     content_type = "score") %>%
        # rename the variables
        rename(halftime_score_home = halftime_home,
               halftime_score_away = halftime_away,
               fulltime_score_home = fulltime_home,
               fulltime_score_away = fulltime_away)
      
      # check if there is no score available and if so fill the columns with NAs
      if(nrow(fixture_score_info) == 0){
        fixture_score_info[1,] <- NA
        fixture_score_info <- fixture_score_info %>%
          mutate(across(.cols = everything(), as.numeric))
      }
      
      # in case the frame is empty
      fixture_score_info <- check_for_value_column(fixture_score_info)
      
      # bind all information for the current fixture together via bind_cols
      all_fixture_information <- bind_rows(
        all_fixture_information,
        bind_cols(fixture_general_info,
                  fixture_league_info,
                  fixture_team_info,
                  fixture_score_info)
      )
    }
 
    # if the request was not successful print an error 
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(response)))
  }
  
  # at the end we want to mutate all variables to give them the
  # right data type
  all_fixture_information <- all_fixture_information %>%
    mutate(fixture_id = as.numeric(fixture_id),
           fixture_date = ymd(fixture_date),
           venue_id = as.numeric(venue_id),
           status_elapsed = as.numeric(status_elapsed),
           league_id = as.numeric(league_id),
           league_season = as.numeric(league_season),
           league_round = as.numeric(league_round),
           club_id_home = as.numeric(club_id_home),
           club_id_away = as.numeric(club_id_away)) 
  
  # include additional information about the points each team got for a match
  # therefore we split the data into past matches and future matches
  past_matches <- all_fixture_information %>%
    filter(fixture_date < Sys.Date()) %>%
    # add variables for the points the teams got for a played match
    mutate(home_points = ifelse(fulltime_score_home > fulltime_score_away,
                                3,
                                ifelse(fulltime_score_home == fulltime_score_away,
                                       1,
                                       0)),
           away_points = ifelse(fulltime_score_away > fulltime_score_home,
                                3,
                                ifelse(fulltime_score_home == fulltime_score_away,
                                       1,
                                       0))) 
  
  # for the future matches we do not need to create these variables because
  # they will be added during the bind_rows operation
  future_matches <- all_fixture_information %>%
    filter(fixture_date >= Sys.Date())
  
  # bind the past matches with the points with the future matches
  all_matches <- bind_rows(past_matches,
                           future_matches) %>%
    # and reorder the variables in a way that makes sense
    select(contains("league"), fixture_date, fixture_time, 
           fixture_id, venue_name, venue_city, venue_id, referee, everything()) %>%
    # order the rows
    arrange(league_id, league_season, league_round, fixture_date, fixture_time)
  
  # return the data frame containing information about all fixtures
  # for the given league and season
  return(all_matches)

}



############## get_fixture_stats #################
# inputs: fixture_id
# outputs: should return a data frame which contains all available
# stats for a given fixture
# example: shots, fouls, cards, etc.
get_fixture_stats <- function(fixture_id){
  # set the endpoint of the API for the actual match statistics
  endpoint <- "https://v3.football.api-sports.io/fixtures/statistics"
  
  # create a get request to that API with the API key
  # and the selected parameter (the selected fixture via its id)
  response <- GET(endpoint, 
                  add_headers('x-apisports-key' = football_api_key),
                  query = list(fixture = fixture_id))
  
  # create empty variable to store the data
  fixture_stats <- NULL
  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(response) >= 200 & status_code(response) < 300){
    
    # extract the content from the response
    content <- content(response)$response
    
    # if there is no content we return NULL
    if(length(content) == 0){
      return(NULL)
    }
    
    # iterate over both team statistics
    for(i in 1:length(content)){
      # extracting the information about the team by extracting the elements
      # unlist to open the content from the list into plain values
      # and convert it with enframe into a data frame
      team_info <- enframe(unlist(content[[i]]$team)) %>%
        # transform the frame by separating the name column with the
        # values from the value column
        pivot_wider(names_from = name, values_from = value,
                    names_glue = "team_{name}")
      
      
      
      team_stats <- enframe(content[[i]]$statistics) %>%
        # extract the list in the value column into separate columns
        # with "_" as separator
        unnest_wider(value, names_sep = "_")
      
      test1 <- team_stats$value_value
      save(test1, file = "test1.RData")
      print(fixture_id)
      
      team_stats$value_value[sapply(team_stats$value_value, is.null)] <- as.character(NA)
      
      # unlist the elements (enframe converts the data into list elements)
      team_stats <- team_stats %>%
        mutate(value_value = unlist(value_value))
      
      save(team_stats, file = "team.RData")
      
      team_stats <- team_stats %>%
        select(-name) %>%
        
      # create the team_stats frame by converting the statistics list
      # element into a data frame with enframe
      # team_stats <- enframe(content[[i]]$statistics) %>%
      #   # extract the list in the value column into separate columns
      #   # with "_" as separator
      #   unnest_wider(value, names_sep = "_") %>%
      #   # unlist the elements (enframe converts the data into list elements)
      #   mutate(value_value = unlist(value_value)) %>%
      #   select(-name) %>%
        # transform the frame by separating the value_type column with the
        # values from the value_value column
        pivot_wider(names_from = value_type, values_from = value_value,
                    names_glue = "{value_type}") %>%
        # rename all variables appropriately
        rename(shots_on_goal = `Shots on Goal`,
               shots_off_goal = `Shots off Goal`,
               shots_total = `Total Shots`,
               shots_blocked = `Blocked Shots`,
               shots_inside_box = `Shots insidebox`,
               shots_outside_box = `Shots outsidebox`,
               fouls = `Fouls`,
               corners = `Corner Kicks`,
               offsides = `Offsides`,
               ball_possession = `Ball Possession`,
               cards_yellow = `Yellow Cards`,
               cards_red = `Red Cards`,
               goalkeeper_saves = `Goalkeeper Saves`,
               passes_total = `Total passes`,
               passes_accurate= `Passes accurate`,
               passing_accuracy = `Passes %`) %>%
        api_football_fixtures_general_complete_check(., content_type = "fixture_stats") %>%
        # add a column for the fixture id
        # and a variable to indicate whether the team is the home or the away team
        # remove the % sign for the possession and the passing accuracy
        mutate(fixture_id = fixture_id,
               ball_possession = str_remove(ball_possession, "%"),
               passing_accuracy = str_remove(passing_accuracy, "%"),
               # replace all NAs with 0
               across(c(shots_on_goal:passing_accuracy), ~replace_na(.x, "0"))) %>%
        # convert all numeric variables into numerics
        mutate(across(c(shots_on_goal:passing_accuracy), as.numeric)) %>%
        # change the order such that the frame begins with the fixture_id
        select(fixture_id, everything())
      
      # bind all data into one frame with multiple columns
      team_data <- bind_cols(team_info, 
                             team_stats) %>%
        mutate(team_id = as.numeric(team_id))
      
      
      # add the current fixture into the data frame
      # saving all fixtures by adding it as a row
      fixture_stats <- bind_rows(
        fixture_stats,
        team_data
      )
    }
    
    # now we want to map the team names
    fixture_stats$team_name <- sapply(fixture_stats$team_name,
                                      club_name_mapping)
    
    # reorder the variables in the data frame
    fixture_stats <- fixture_stats %>%
      select(fixture_id, team_id, team_name, team_logo,
             everything())
    
    # map the fixture information to the data
    matches_information <- all_leagues_matches %>%
      select(league_id, season = league_season, matchday = league_round,
             fixture_date, fixture_time, fixture_id, fulltime_score_home,
             fulltime_score_away)
    
    # join the both data sets to also get the season and league information
    fixture_stats <- matches_information %>%
      inner_join(fixture_stats,
                 by = c("fixture_id")) %>%
      # reorder the data frame
      select(league_id, season, matchday, fixture_date, fixture_time, fixture_id,
             team_id:passing_accuracy, fulltime_score_home, fulltime_score_away)
    
    # if the request was not successful print an error 
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(response)))
  }
  
  return(fixture_stats)
}




############## get_fixture_events #################
# inputs: fixture_id
# outputs: should return a data frame which contains all available
# information about the events happening in a selected fixture
# example: goals, cards, etc.
get_fixture_events <- function(fixture_id){
  # set the endpoint of the API
  endpoint <- "https://v3.football.api-sports.io/fixtures/events"
  
  # create a get request to that API with the API key
  # and the selected parameter (the selected fixture via its id)
  response <- GET(endpoint, 
                  add_headers('x-apisports-key' = football_api_key),
                  query = list(fixture = fixture_id))

  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(response) >= 200 & status_code(response) < 300){
    
    # extract the content from the response
    content <- content(response)$response
    
    # if there is no content we return NULL
    if(length(content) == 0){
      return(NULL)
    }
    
    # pre-allocate a data frame with all necessary variables
    fixture_events <- data.frame(time_elapsed = rep(NA, length(content)),
                                 time_extra = rep(NA, length(content)),
                                 team_id = rep(NA, length(content)),
                                 team_name = rep(NA, length(content)),
                                 team_logo = rep(NA, length(content)),
                                 player_id = rep(NA, length(content)),
                                 player_name = rep(NA, length(content)),
                                 assist_id = rep(NA, length(content)),
                                 assist_name = rep(NA, length(content)),
                                 event_type = rep(NA, length(content)),
                                 event_detail = rep(NA, length(content)),
                                 event_comments = rep(NA, length(content)))
    
    # iterate over all the events
    for(i in 1:length(content)){
      # use the helper function get_content_for_list_element
      # to extract the needed content 
      curr_events <- get_content_for_list_element(content,
                                                  i,
                                                  name_elem = "") 
      
      # use the helper function to add columns that are not currently 
      # present in the data 
      curr_events <- api_football_fixtures_general_complete_check(curr_events,
                                                                  "fixture_events") %>%
        # rename the event variables
        rename(event_comments = comments,
               event_type = type, 
               event_detail = detail) %>%
        # convert the variables into proper type
        mutate(time_elapsed = as.numeric(time_elapsed),
               time_extra = as.numeric(time_extra),
               team_id = as.numeric(team_id),
               player_id = as.numeric(player_id),
               assist_id = as.numeric(assist_id),
               assist_name = as.character(assist_name),
               event_comments = as.character(event_comments)) %>%
        # rename the abbreviation subst into Substitution
        # and replace the NAs for time extra to 0s
        mutate(event_type = ifelse(event_type == "subst",
                                   "Substitution",
                                   event_type),
               time_extra = replace_na(time_extra, 0))
      
      # map the club names
      curr_events$team_name <- sapply(curr_events$team_name,
                                      club_name_mapping)
      
      
      
      # insert the current data in the data frame pre-allocated above
      fixture_events[i, ] <- curr_events

    }
    
    #add an additional variable for the fixture_id
    # as the first variable
    fixture_events <- fixture_events %>%
      add_column(fixture_id, .before = 1)
    
    # join these fixture infos with basic match information such as fixture date
    fixture_events_data <- all_leagues_matches %>%
      # join by fixture_id
      inner_join(fixture_events, by = "fixture_id")
    
    # if the request was not successful print an error 
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(response)))
  }
  
  # return a data frame containing all events of a fixture
  return(fixture_events_data)

}



############## get_fixture_lineups #################
# inputs: fixture_id
# outputs: should return a data frame which contains all available
# information about the lineups of a selected fixture
# example: formation, starting grid, etc.
get_fixture_lineups <- function(fixture_id){
  # set the endpoint of the API
  endpoint <- "https://v3.football.api-sports.io/fixtures/lineups"
  
  # create a get request to that API with the API key
  # and the selected parameter (the selected fixture via its id)
  response <- GET(endpoint, 
                  add_headers('x-apisports-key' = football_api_key),
                  query = list(fixture = fixture_id))
  
  # create an empty variable to the lineup info of both teams
  fixture_lineups <- NULL
  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(response) >= 200 & status_code(response) < 300){
    
    # extract the content from the response
    content <- content(response)$response
    
    # if there is no content we return NULL
    if(length(content) == 0){
      return(NULL)
    }
    
    # iterate over both team statistics
    for(i in 1:length(content)){
      # create an empty variable to store the lineup info of the current team
      curr_team_lineup_info <- NULL
      
      # extract the content from the team list-element
      team_content <- content[[i]]
      
      # now transform this content from a list element
      # into a data frame of one row by extracting the elements
      # with the helper function get_content_for_list_element
      team_info <- get_content_for_list_element(team_content, "team",
                                                piv_wider = TRUE,
                                                name_elem = "team_") %>%
        mutate(team_id = as.numeric(team_id))
      
      # map the team name with the mapping function club_name_mapping
      team_info$team_name <- sapply(team_info$team_name,
                                    club_name_mapping) %>%
        unname()
      
      # drop all colnames that contain the word colors (for the shirts)
      # because we do not want this information
      if(sum(colnames(team_info) %like% "colors") > 0){
        team_info <- select(team_info, -c(contains("colors")))
      }
      
      # extracting the start formation and if null set it to NA and
      # convert it into a character
      starting_formation <- ifelse(is.null(content[[i]]$formation),
                                   NA, content[[i]]$formation) %>%
        as.character()
      
      # extracting the players of the start formation
      # get the plain data into a named character vector
      player_data <- unlist_no_drop_nulls(content[[i]]$startXI)
      
      # compute the number of features that are available for each player
      number_features <- length(unique(names(player_data)))
      
      # create the column names for the data frame
      colnames_player_data <- unique(names(player_data)) %>%
        # all columns are named like player.id, we want them to be 
        # named like player_id, so we replace the "." with an underscore
        str_replace_all(., pattern = "\\.", replacement = "_")
      
      # transform the named character vector into a data frame
      player_data <- player_data %>%
        # create a matrix with 11 rows (1 for each player; every team
        # starts with 11 players) and number_features columns (5 in this case)
        matrix(data = .,
               nrow = 11,
               ncol = number_features,
               byrow = TRUE) %>%
        data.frame()
      
      # in case the player_grid variable is not in the data (for early matches, e.g. 2015)
      if(!("player_grid" %in% colnames_player_data)){
        # create a new variable for player_grid and set it to NA
        player_data <- player_data %>%
          mutate(player_grid = NA) %>%
          # convert it then into character because in case there is data
          # it looks like this: "2:4"
          mutate(player_grid = as.character(player_grid))
        
        # add the player_grid to the colnames
        colnames_player_data <- c(colnames_player_data, "player_grid")
      }
      
      # set the column names accordingly
      colnames(player_data) <- colnames_player_data
      
      # add a new variable indicating that these players are in the starting 
      # lineup and convert the player_id and number into numeric values
      player_data <- player_data %>%
        mutate(is_starting_grid = TRUE,
               player_id = as.numeric(player_id),
               player_number = as.numeric(player_number))
      
      # starting_formation_info <- player_data %>%
      #   # add a new column counting the defenders, midfielders and strikers
      #   transmute(number_defenders = sum(player_pos == "D"),
      #             number_midfielders = sum(player_pos == "M"),
      #             number_attackers = sum(player_pos %in% c("S", "F"))) %>%
      #   unique()
      
      # extracting the substitute players
      substitutes_data <- content[[i]]$substitutes
      
      # create an empty variable to store the substitute information
      all_substitutes <- NULL
      
      # iterate over the substitutes
      for(j in 1:length(substitutes_data)){
        # extract the information of the current substitute player
        curr_sub_player <- substitutes_data[[j]] %>%
          list.stack()
        
        # clean the colnames of the data frame
        colnames_sub <- colnames(curr_sub_player) %>%
          paste("player", ., sep = "_")
        
        # set the colnames accordingly
        colnames(curr_sub_player) <- colnames_sub
        
        # bind all the information together
        all_substitutes <- bind_rows(all_substitutes,
                                     curr_sub_player)
    
      }
      
      # add a new variable to indicate that these players do not stand in the
      # starting grid
      all_substitutes <- all_substitutes %>%
        mutate(is_starting_grid = FALSE)
     
      
      # combine all the lineup info of the current team into one
      # temporary data frame
      curr_team_lineup_info <- curr_team_lineup_info %>%
        bind_rows(player_data, all_substitutes) %>%
        bind_cols(team_info, .) %>%
        mutate(formation = starting_formation)
      
      # add the current team lineup information to the overall lineup frame
      fixture_lineups <- bind_rows(fixture_lineups,
                                   curr_team_lineup_info)
      
    }
    
    # add the fixture id as new variable to the lineup information
    # to be able to join the lineup information with the match data
    fixture_lineups <- fixture_lineups %>%
      mutate("fixture_id" = fixture_id)
    
    # finally, combine the lineup information with the match information
    fixture_lineups_data <- all_leagues_matches %>%
      inner_join(fixture_lineups, by = "fixture_id")
  
    # if the request was not successful print an error 
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(response)))
  }
  
  # return the lineups for the fixture
  return(fixture_lineups_data)
  
}




############## get_player_stats_fixture #################
# inputs: fixture_id
# outputs: should return a data frame which contains all available
# statistics about the the players of a selected fixture
get_player_stats_fixture <- function(fixture_id){
  # set the endpoint of the API
  endpoint <- "https://v3.football.api-sports.io/fixtures/players"

  # create a get request to that API with the API key
  # and the selected parameter (the selected fixture via its id)
  response <- GET(endpoint, 
                  add_headers('x-apisports-key' = football_api_key),
                  query = list(fixture = fixture_id))
  
  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(response) >= 200 & status_code(response) < 300){
    
    # extract the content from the response
    content <- content(response)$response
    
    # create an empty variable to store all player stats
    players_all <- NULL
    
    # iterate over both team statistics
    for(i in 1:length(content)){
      # extract information about the current team with the helper function
      team_info <- get_content_for_list_element(content[[i]], "team",
                                                piv_wider = TRUE,
                                                name_elem = "team_") %>%
        mutate(team_id = as.numeric(team_id)) %>%
        select(-team_update)
      
      # map the team name
      team_info$team_name <- sapply(team_info$team_name,
                                    club_name_mapping)
      
      # extract the players data
      player_list <- content[[i]]$players
      
      # create an empty variable to store all player stats
      # of the current team
      players_current_team <- NULL
      
      # iterate through all players in the current team
      for(player in 1:length(player_list)){
        # extract the player info of the current player by using the
        # helper function
        player_info <- get_content_for_list_element(player_list[[player]], "player",
                                                 piv_wider = TRUE,
                                                 name_elem = "player_") %>%
          mutate(player_id = as.numeric(player_id))
        
        # extract its statistics
        player_stats <- player_list[[player]]$statistics[[1]]
        
        # get game stats
        game_stats <- get_content_for_list_element(player_stats, "games")
        
        # get offside stats
        offside_stats <- get_content_for_list_element(player_stats, "offsides")
        
        # get shot stats
        shot_stats <- get_content_for_list_element(player_stats, "shots")
        
        # get goal stats
        goal_stats <- get_content_for_list_element(player_stats, "goals")
        
        # get pass stats
        pass_stats <- get_content_for_list_element(player_stats, "passes")
        
        # get tackle stats
        tackle_stats <- get_content_for_list_element(player_stats, "tackles")
        
        # get duel stats
        duel_stats <- get_content_for_list_element(player_stats, "duels")
        
        # get dribbles stats
        dribble_stats <- get_content_for_list_element(player_stats, "dribbles")
        
        # get foul stats
        foul_stats <- get_content_for_list_element(player_stats, "fouls")
        
        # get card stats
        card_stats <- get_content_for_list_element(player_stats, "cards")
        
        # get penalty stats
        penalty_stats <- get_content_for_list_element(player_stats, "penalties")
        
        # bind all information together into a data frame
        current_player <- c(game_stats, offside_stats, shot_stats, goal_stats,
                            pass_stats, tackle_stats, duel_stats, dribble_stats,
                            foul_stats, card_stats, penalty_stats) %>%
          # the binding gives a list of the single stats and we need to unlist them
          unlist() %>%
          # and then convert it into a data frame (every stat is one row)
          data.frame()
        
        # add the row names as a new variable called stats
        current_player <- rownames_to_column(current_player,
                                             var = "stat")
        
        # convert the data frame into one row with stats as variables
        current_player <- current_player %>%
          pivot_wider(names_from = "stat",
                      values_from = ".")
        
        # use the helper function to clean the data frame of the current player
        # by adding the missing columns and filling them with NA values
        current_player_compl <- api_football_fixtures_general_complete_check(
          current_player, "player_stats_fixture") %>%
          # fill all NA values with 0
          mutate(across(.cols = everything(), ~replace_na(.x, 0)),
                 # convert most of the variables dynamically into numeric variables
                 across(c(contains("offsides"), contains("shots"),
                          contains("goals"), contains("tackles"),
                          contains("duels"), contains("dribbles"),
                          contains("fouls"), contains("cards"),
                          contains("penalty")), as.numeric),
                 # convert the variables left into a proper format
                 games_minutes = as.numeric(games_minutes),
                 games_number = as.numeric(games_number),
                 games_rating = as.numeric(games_rating),
                 games_captain = as.logical(games_captain),
                 games_substitute = as.logical(games_substitute),
                 passes_total = as.numeric(passes_total),
                 passes_key = as.numeric(passes_key),
                 passes_accuracy = as.numeric(str_remove(passes_accuracy, 
                                                         pattern = "%"))) %>%
          data.frame() %>%
          # add the player data as new columns at the beginning of the data frame
          cbind(player_info, .)
        
        # check if there is a variable offsides_1
        if("offsides_1" %in% colnames(current_player_compl)){
          current_player_compl <- current_player_compl %>%
            mutate(offsides = offsides_1) %>%
            select(-offsides_1)
        }
        
        if("tackles_interceptions" %in% colnames(current_player_compl)){
          current_player_compl <- current_player_compl %>%
            mutate(tackles_interception = tackles_interceptions,
                   tackles_interception = replace_na(tackles_interception,
                                                     0)) %>%
            select(-tackles_interceptions)
        }
        
        current_player_compl <- current_player_compl %>%
          # reorder the whole data frame
          select(contains("team"), contains("player"), contains("games"), 
                 contains("goals"), contains("shots"), contains("offsides"),
                 contains("passes"), contains("tackles"), contains("duels"), 
                 contains("dribbles"), contains("fouls"), contains("penalty"),
                 contains("cards"))
        
        
        
        # add the current player to the players_current_team frame
        players_current_team <- bind_rows(players_current_team,
                                          current_player_compl)
      }
      
      # combine the player stats with the team information we extracted
      # at the beginning
      players_current_team <- bind_cols(team_info, 
                                        players_current_team)
      
      # add the player stats of the current team to the frame
      # players_all
      players_all <- bind_rows(
        players_all,
        players_current_team
      )
      
    }
    
    # add the fixture id to the player stats
    players_all$fixture_id <- fixture_id
    
    # join the both data sets to also get the season and league information
    players_all <- all_leagues_matches %>%
      inner_join(players_all,
                 by = c("fixture_id")) %>%
      # drop the points
      select(-c(home_points, away_points))
    
    
    # if the request was not successful print an error 
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(response)))
  }
  
  # return the stats of all players for the given fixture
  return(players_all)
  
}



add_fixture_infos_to_player_fixture_stats <- function(league_id, match_id){
  # get the data from the data base
  match_information <- NULL
  
  if(league_id == 78){
    # match_information <- tbl(con, "buli_matches") %>%
    #   filter(fixture_id == match_id) %>%
    #   data.frame()
    match_information <- buli_matches %>%
      filter(fixture_id == match_id)
  } else if(league_id == 79){
    match_information <- tbl(con, "buli2_matches") %>%
      filter(fixture_id == match_id) %>%
      data.frame()
  } else if(league_id == 39){
    # match_information <- tbl(con, "pl_matches") %>%
    #   filter(fixture_id == match_id) %>%
    #   data.frame()
    match_information <- pl_matches_2010_to_2021 %>%
      filter(fixture_id == match_id)
  } else if(league_id == 135){
    match_information <- tbl(con, "serie_a_matches") %>%
      filter(fixture_id == match_id) %>%
      data.frame()
  } else if(league_id == 140){
    # match_information <- tbl(con, "la_liga_matches") %>%
    #   filter(fixture_id == match_id) %>%
    #   data.frame()
    match_information <- la_liga_matches %>%
      filter(fixture_id == match_id) %>%
      data.frame()
  } else if(league_id == 61){
    # match_information <- tbl(con, "ligue1_matches") %>%
    #   filter(fixture_id == match_id) %>%
    #   data.frame()
    match_information <- ligue1_matches %>%
      filter(fixture_id == match_id) %>%
      data.frame()
  } else {
    print("League ID not available")
    return(NULL)
  }
  
  player_fixture_stats <- get_player_stats_fixture(match_id)
  
  fixture_data <- bind_cols(match_information, player_fixture_stats)
  
  return(fixture_data)
}


############## get_player_stats_season #################
# inputs: league_id, season
# outputs: should return a data frame which contains stats about all players
# in the league for a season
get_player_stats_season <- function(league_id, season){
  # set the endpoint of the API
  endpoint <- "https://v3.football.api-sports.io/players"
  
  # create a get request to that API with the API key
  # and the selected parameters (the league via its id and the season)
  response <- GET(endpoint, 
                  add_headers('x-apisports-key' = football_api_key),
                  query = list(league = league_id,
                               season = season))
  
  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(response) >= 200 & status_code(response) < 300){
    
    # this endpoint is paginated. Therefore we have to extract the number of pages
    # to iterate over all pages
    number_pages <- content(response)$paging$total
    
    # create an empty variable to store all player stats
    all_players <- NULL
    
    # iterate over all pages
    for(i in 1:number_pages){
      # print for debugging
      print(paste0("Current page: ", i))
      
      # create a get request for the current page with the given parameters
      # and the current page
      response_current_page <- GET(endpoint, 
                      add_headers('x-apisports-key' = football_api_key),
                      query = list(league = league_id,
                                   season = season,
                                   page = i))
      
      
      # check if the request was successful and only then go on with the 
      # transformation of the data
      if(status_code(response_current_page) >= 200 & 
         status_code(response_current_page) < 300){
      
        # extract the content of the current page
        content_current_page <- content(response_current_page)$response
  
        # iterate over all players on the current page
        for(player in 1:length(content_current_page)){
          # print for debugging
          print(paste0("Current player: ", player))
          
          current_player <- content_current_page[[player]]
          
          # extract player specific information such as name, age and nationality
          # we use the helper function get_content_for_list_element
          player_infos <- get_content_for_list_element(current_player, "player",
                                                       piv_wider = TRUE,
                                                       name_elem = "player_") %>%
            # make the name column correct, i.e., do not shorten the first name
            mutate(player_name = paste0(player_firstname, " ", player_lastname))
            
          # extract player statistics
          # use the helper function get_content_for_list_element
          player_stats <- get_content_for_list_element(current_player$statistics, 1,
                                                       piv_wider = TRUE,
                                                       name_elem = "")
          
          # drop unwanted variables
          if("games_number" %in% colnames(player_stats)){
            player_stats <- select(player_stats, -games_number)
          } 
          if("games_captain" %in% colnames(player_stats)){
            player_stats <- select(player_stats, -games_captain)
          }
          
          # map the team name with our mapping function
          player_stats$team_name <- sapply(player_stats$team_name,
                                           club_name_mapping)
          
          
          
          
          # combine both data frames into one by binding the columns together
          player_data <- bind_cols(player_infos, 
                                   player_stats)
          
          # transform the data frame
          player_data <- player_data %>%
            # order the data frame
            select(contains("league"), contains("team"), contains("player"),
                   everything()) %>%
            # check if all variables are present and if not add them into the
            # correct spot
            api_football_fixtures_general_complete_check(., "player_stats_season") %>%
            # create new variables for the height and weight and make them numeric
            mutate(player_weight_kg = as.numeric(str_remove(player_weight, pattern = " kg")),
                   player_height_cm = as.numeric(str_remove(player_height, pattern = " cm")),
                   player_lastname_to_map = str_to_title(stri_trans_general(player_lastname, id = "Latin-ASCII"))) %>%
            # drop the previous variables which are no longer needed
            select(-c(player_weight, player_height)) %>%
            # convert all variables that are numeric values into actual numbers
            mutate(across(c(contains("id"), contains("season"), contains("age"),
                            contains("substitutes"), contains("shots"), contains("goals"),
                            contains("passes"), contains("tackles"), contains("duels"),
                            contains("dribbles"), contains("fouls"), 
                            contains("cards"), contains("penalty")), as.numeric),
                   # transform dates and booleans
                   player_birth_date = ymd(player_birth_date),
                   player_injured = as.logical(player_injured),
                   across(c(games_appearences, games_lineups, games_minutes,
                            games_rating), as.numeric),
                   # transform all character variables to actual characters to prevent
                   # R from saving missing data as a logical variable
                   across(c(league_name, league_country, league_flag,
                            team_name, team_logo,
                            player_name, player_firstname, player_lastname,
                            player_birth_place, player_birth_country,
                            player_nationality, player_photo,
                            games_position), as.character),
                   # replace all NAs if correct, with 0)
                   across(c(games_appearences:games_minutes,
                            games_rating:penalty_saved), ~replace_na(.x, 0))) %>%
            # order the data frame
            select(contains("league"), contains("team"), contains("player"),
                   everything())
          
          
          # add the current player to the all_players frame
          all_players <- bind_rows(all_players, player_data)
            
        }
        # if the inner request was not successful print an error 
      } else {
        print(paste0("Error: The request was not successful. \nStatus code: ",
                     status_code(response_current_page)))
      }
      
      # sleep for 20 seconds to not overwhelm the API and because the API
      # does not return a value if we request too many data in a short time
      Sys.sleep(20)
      
    }
    
    # if the outer request was not successful print an error 
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(response)))
  }
  
  
  return(all_players)
}



 
############## get_team_transfers #################
# inputs: team_id
# outputs: data frame with all transfers of a given team
get_team_transfers <- function(team_id){
  # set the endpoint of the API
  endpoint <- "https://v3.football.api-sports.io/transfers"
  
  # create a get request to that API with the API key
  # and the selected parameter (team_id)
  response <- GET(endpoint, 
                  add_headers('x-apisports-key' = football_api_key),
                  query = list(team = team_id))
  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(response) >= 200 & status_code(response) < 300){
    
    # extract the content from the response
    content <- content(response)$response
    
    # get the content of all transfers of the club
    transfer_infos <- content
    
    # create a variable to store all the transfers
    transfer_frame <- NULL
    
    # iterate over all transfers the player had
    for(transfer in 1:length(transfer_infos)){
      print(paste0("Curr. Transfer: ", transfer))
      # extract the player information of the transfer
      player_infos <- get_content_for_list_element(transfer_infos[[transfer]],
                                                   "player",
                                                   piv_wider = TRUE,
                                                   name_elem = "player_") %>%
        data.frame()
      
      if(!("player_name" %in% colnames(player_infos))){
        next
      }
    
    
      # extract the date of the transfer and convert it into a data frame
      date <- transfer_infos[[transfer]]$transfers[[1]]$date %>%
        data.frame("date" = .)
      
      # extract the type of the transfer and convert it into a data frame
      type <- transfer_infos[[transfer]]$transfers[[1]]$type 
      
      # deal with the case that there is no "type" data available
      # if there is no type information, we create a data frame and fill it with NA
      # otherwise, we create a data frame and fill it with the type data
      if(is.null(type)){
        type <- data.frame("type" = as.character(NA)) %>%
          mutate(transfer_sum_mil_euro = as.numeric(NA))
        
      } else {
        # create a data frame with the type
        type <- data.frame("type" = type) %>%
          # transform the transfer sum (extract it from the type variable)
          mutate(transfer_sum_mil_euro = type,
                 # create the type variable in a clean format that is easy to understand
                 type = ifelse(str_detect(type, pattern = ".*[0-9].*"),
                               "Paid",
                               ifelse(type == "N/A",
                                      as.character(NA),
                                      type)),
                 # transform the transfer sum properly
                 transfer_sum_mil_euro = str_extract(transfer_sum_mil_euro, pattern = "[0-9]+(\\.?[0-9]+)?[MK]?"),
                 # if it ends with M (millions) leave it as it is (because the variable
                 # is in millions)
                 transfer_sum_mil_euro = ifelse(endsWith(transfer_sum_mil_euro, "M"),
                                                         as.numeric(str_remove(transfer_sum_mil_euro,
                                                                               "M")),
                                                # if the its ends with K (1000) we
                                                # divide by 1000
                                                as.numeric(str_remove(transfer_sum_mil_euro,
                                                                      "K"))/1000)) %>%
          # if the type is Free or Loan we want to set the transfer sum to 0
          mutate(transfer_sum_mil_euro = ifelse(type %in% c("Free", "Loan"),
                                                0, 
                                                transfer_sum_mil_euro))
                                                 
                 
      }
        
      # extract the information about the teams involved in the transfer
      # the information about the team the player transfered from
      # using the helper function get_content_for_list_element to extract the
      # content and return it in a proper format
      team_infos_from <- 
        get_content_for_list_element(transfer_infos[[transfer]]$transfers[[1]]$teams,
                                     "out",
                                     piv_wider = TRUE,
                                     name_elem = "from_team_") %>%
        data.frame()
      
      # deal with the case that there is no club the player came from, i.e.,
      # the player is a young player who was scouted. Then we create a data frame 
      # and fill it with NA 
      if(nrow(team_infos_from) == 0){
        team_infos_from <- data.frame("from_team_id" = as.numeric(NA),
                                      "from_team_name" = as.character(NA),
                                      "from_team_logo" = as.character(NA))
      } 
      
      
      # the information about the team the player transfered to
      # using the helper function get_content_for_list_element to extract the
      # content and return it in a proper format
      team_infos_to <-  
        get_content_for_list_element(transfer_infos[[transfer]]$transfers[[1]]$teams,
                                     "in",
                                     piv_wider = TRUE,
                                     name_elem = "to_team_") %>%
        data.frame()
      
      # deal with the case that there is no club the player goes to, i.e.,
      # the player ends his career. Then we create a data frame 
      # and fill it with NA 
      if(nrow(team_infos_to) == 0){
        team_infos_to <- data.frame("to_team_id" = as.numeric(NA),
                                    "to_team_name" = as.character(NA),
                                    "to_team_logo" = as.character(NA))
      }
      
      # map the team names (from_team_name and to_team_name) with the
      # mapping function club_name_mapping
      team_infos_from$from_team_name <- sapply(team_infos_from$from_team_name,
                                               club_name_mapping)
      
      team_infos_to$to_team_name <- sapply(team_infos_to$to_team_name,
                                           club_name_mapping)
      
      
      # combine all information into one data frame
      curr_transfer_infos <- bind_cols(player_infos, date, type, team_infos_from,
                                   team_infos_to)
      
      # for the last cleaning step, we have to make sure that all columns are
      # present (this could not be the case if the player ends his career).
      # To do this, we use the cleaning function api_football_fixtures_general_complete_check
      curr_transfer_infos <- api_football_fixtures_general_complete_check(curr_transfer_infos,
                                                                          "team_transfer")
      
      # do some final variable mutations
      # convert the team information
      curr_transfer_infos <- curr_transfer_infos %>%
        mutate(from_team_id = as.numeric(from_team_id),
               from_team_name = as.character(from_team_name),
               from_team_logo = as.character(from_team_logo),
               to_team_id = as.numeric(to_team_id),
               to_team_name = as.character(to_team_name),
               to_team_logo = as.character(to_team_logo),
               # convert the player id into numeric
               player_id = as.numeric(player_id),
               # convert the date into an actual date
               transfer_date = ymd(date),
               # rename some variables
               transfer_type = type,
               transfer_sum_mil_euro,
               # add a variable that indicates which team we currently observe
               "team_id" = team_id) %>%
        # reorder the variables
        select(team_id, player_id, player_name, transfer_date, transfer_type,
               transfer_sum_mil_euro, from_team_id, from_team_name, from_team_logo,
               to_team_id, to_team_name, to_team_logo) %>%
        # lastly, filter for only those transfers that actually are finished
        filter(transfer_date < Sys.Date())
        
      
      # now combine the data extracted for the current transfer with the
      # data collected earlier into one frame by binding rows
      transfer_frame <- bind_rows(
        transfer_frame,
        curr_transfer_infos
      )
    }
    
    # if the request was not successful print an error 
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(response)))
  }
  
  # return all the transfers of the club
  return(transfer_frame)
} 



###################### HELPERS ###################### 
############## get_content_for_list_element #################
# inputs: fixture_id
# outputs: should return a data frame which contains all available
# information about the lineups of a selected fixture
# example: coach, formation, starting grid, etc.

get_content_for_list_element <- function(content_list, list_element,
                                         piv_wider = TRUE,
                                         name_elem = NULL){
  # convert the clean_sheet-list in content into a named string
  # and then with enframe into a tibble with 2 columns (name and value)
  content_for_list_element <- 
    enframe(unlist(content_list[[list_element]])) %>%
    data.frame() %>%
    # rename all values (later the variables) of the name column
    # to have an underscore instead of a point to separate
    mutate(name = str_replace_all(name, pattern = "\\.",
                                  replacement = "_"))
  
  if(piv_wider){
    if(is.null(name_elem)){
      content_for_list_element <- content_for_list_element %>%
        # use pivot_wider to spread the first column (name) with the second
        # column (value) as values. use names_glue to convert the names
        # of the variables into "clean_sheet_{name_of_variable}"
        pivot_wider(names_from = name,
                    values_from = value,
                    names_glue = paste0(list_element, "_{name}"))
    } else {
      content_for_list_element <- content_for_list_element %>%
        # use pivot_wider to spread the first column (name) with the second
        # column (value) as values. use names_glue to convert the names
        # of the variables into "clean_sheet_{name_of_variable}"
        pivot_wider(names_from = name,
                    values_from = value,
                    names_glue = paste0(name_elem, "{name}"))
    }
    
  }
  
  content_for_list_element <- check_for_value_column(content_for_list_element)
  content_for_list_element <- as.data.frame(content_for_list_element)
    
  
  return(content_for_list_element)
}


check_for_value_column <- function(frame_to_observe){
  if(nrow(frame_to_observe) == 0 & "value" %in% colnames(frame_to_observe)){
    frame_to_observe <- frame_to_observe %>%
      select(-value)
  }
  
  return(frame_to_observe)
}





city_name_mapping <- function(city_name){
  if(is.na(city_name)){
    return(city_name)
  }
  
  city_name <- str_to_title(city_name)
  
  if(str_detect(city_name, pattern = "München")){
    return("Munich")
  } else if(str_detect(city_name, pattern = "Düsseldorf")){
    return("Dusseldorf")
  } else if(str_detect(city_name, pattern = "Mönchengladbach")){
    return("Monchengladbach")
  } else if(str_detect(city_name, pattern = "Nürnberg")){
    return("Nuremberg")
  } else if(str_detect(city_name, pattern = "Köln")){
    return("Cologne")
  } else if(str_detect(city_name, pattern = "Frankfurt")){
    return("Frankfurt am Main")
  } else if(str_detect(city_name, pattern = "Fürth")){
    return("Furth")
  }
  
  return(city_name)
}
