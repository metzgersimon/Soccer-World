# ############## get_seasons_by_league_id #################
# inputs: league_id
# outputs: should return a data frame for a given league ID in all seasons
# which is passed on as a parameter through an ID

get_seasons_by_league_id <- function(league_id) {
  # For this API one first have to get a token which is valid for one hour
  # (this is done with a POST).
  # This token then must be used instead of the API key when retrieving data
  access_token_elena <- get_elena_access_token()
  
  # create the url endpoint for the seasons
  url_season <- paste0("https://football.elenasport.io/v2/leagues/",
                       league_id,
                       "/seasons")
  
  # retrieve the data from the API
  season_response <- GET(url_season,
                         add_headers('Authorization' =
                                       paste0("Bearer ", access_token_elena)))
  
  # extract the content
  season_content <- content(season_response)$data
  
  # create a data frame by parsing the list of lists
  season_frame <- list.stack(season_content)
  
  return(season_frame)
}



# ############## get_fixtures_by_season_id #################
# inputs: season_id
# outputs: should return a data frame which contains the information about
# all fixtures of a season 

get_fixtures_by_season_id <- function(season_id) {
  # For this API one first have to get a token which is valid for one hour
  # (this is done with a POST).
  # This token then must be used instead of the API key when retrieving data
  access_token_elena <- get_elena_access_token()
  
  # create a variable to control the paginated endpoint
  page <- 1
  
  # create the url endpoint for the paginated fixtures
  url_fixtures <- paste0(
    "https://football.elenasport.io/v2/",
    "seasons/",
    season_id,
    "/fixtures?page=",
    page
  )
  
  # retrieve the data from the API
  fixtures_response <- GET(url_fixtures,
                           add_headers('Authorization' =
                                         paste0("Bearer ", access_token_elena)))
  
  fixtures_frame <- NULL
  
  # check if the request was successful and only then go on with the 
  # transformation of the data
  if(status_code(fixtures_response) >= 200 & status_code(fixtures_response) < 300){
    
    # extract the content of the response
    fixtures_content <- content(fixtures_response)
    
    # extract the content of the first page and store it as a data frame
    current_fixtures_frame <- list.stack(fixtures_content$data)
    
    # use the helper function clean_fixtures_frame to extract the list of referees
    # from the fixtures_frame and store them as new variables in the frame
    fixtures_frame <- clean_fixtures_frame(current_fixtures_frame)
    
    # extract information about whether there is a next page
    has_next <- fixtures_content$pagination$hasNextPage
    
    # iterate through the pages as long as there is a next page
    while (has_next) {
      print(paste0("page=", page))
      # create the url endpoint for the current page
      url_fixtures <- paste0(
        "https://football.elenasport.io/v2/",
        "seasons/",
        season_id,
        "/fixtures?page=",
        page
      )
      
      # retrieve the data from the API
      fixtures_response <- GET(url_fixtures,
                               add_headers('Authorization' =
                                             paste0("Bearer ", access_token_elena)))
      
      # extract the content of the response
      fixtures_content <- content(fixtures_response)
      
      # reset the has_next variable
      has_next <- fixtures_content$pagination$hasNextPage
      
      # extract the content of the current page and store it as a data frame
      current_fixtures_frame <- list.stack(fixtures_content$data)
      
      # use the helper function clean_fixtures_frame to extract the list of referees
      # from the current_fixtures_frame and store them as new variables in the frame
      current_fixtures <-
        clean_fixtures_frame(current_fixtures_frame)
      
      
      # append the fixtures of the current page to the frame of the previous pages
      fixtures_frame <- bind_rows(fixtures_frame, current_fixtures)
      
      # increase the page counter
      page <- page + 1
      
    }
  } else {
    print(paste0("Error: The request was not successful. \nStatus code: ",
                 status_code(fixtures_response)))
  }
  
  # extract the content of the first page and store it as a data frame
  current_fixtures_frame <- list.stack(fixtures_content$data)
  
  # use the helper function clean_fixtures_frame to extract the list of referees
  # from the fixtures_frame and store them as new variables in the frame
  fixtures_frame <- clean_fixtures_frame(current_fixtures_frame)# return the fixtures_frame 
  return(fixtures_frame)
  
}
  


############### clean_fixtures_frame #################
# inputs: fixtures_frame
# outputs: should return a data frame which extracts the list of referee information 
# from a column in the fixtures_frame and merge all information together

clean_fixtures_frame <- function(fixtures_frame_test) {
  # extract the number of referees (should be always 4 but maybe there is some
  # mistake in the data)
  number_of_refs <- fixtures_frame_test %>%
    group_by(id) %>%
    count() %>%
    ungroup()
  
  # extract the referee column which is a list and store it as a data frame
  # after that correct a typo, "assitant referee" to "assistant_referee"
  fixtures_with_refs <- unnest_wider(
    fixtures_frame_test,
    col = referees) %>%
    mutate(type = ifelse(type == "assitant referee", "assistant_referee",
                         type))
  
  row_positions <- which.max(fixtures_with_refs$id == 
                         number_of_refs$id[number_of_refs$n != 4])
  
  if(length(row_positions) > 0){
    for(i in 1:length(row_positions)){
      # get the position of the current row
      curr_row_pos <- row_positions[i]
      
      row_to_insert <- fixtures_with_refs[curr_row_pos, ] %>%
        mutate(idReferee = NA,
               refereeName = NA)
      
      # insert missing column with dynamic name 
      # and insert it at the correct position
      fixtures_with_refs <- fixtures_with_refs %>%
        add_row(row_to_insert, .after = (curr_row_pos - 1))
    }
  }
  
  
  # drop all unnecessary columns from the fixtures frame and only use unique
  # rows (after dropping the referees column there is only one row per fixture
  # instead of 4 available)
  fixtures_with_refs <- fixtures_with_refs %>%
    select(-c(
      elapsed,
      elapsedPlus,
      eventsHash,
      lineupsHash,
      statsHash
    )) %>%
    unique()
  
  # iterate through the referees frame starting with 0 (instead of 1)
  # in order to use this method
  for (i in 0:(nrow(fixtures_with_refs) - 1)) {
    # we want to check if the current string in the type column contains
    # the word assistant
    if (str_detect(fixtures_with_refs$type[(i + 1)], "assistant")) {
      # if so, we want to append a number
      # such that out of "assistant_referee" we make "assistant_referee_1",
      # _2 and _3 depending on i with the modulo operator
      fixtures_with_refs$type[(i + 1)] <-
        paste0(fixtures_with_refs$type[(i + 1)], "_", (i %% max(number_of_refs$n)))
    }
  }
  
  # clean the referees frame
  fixtures_with_refs_clean <- fixtures_with_refs %>%
    # group by every n-th row depending on the number of referees (number_of_refs)
    group_by(grp = rep(row_number(), length.out = n(), each = max(number_of_refs$n))) %>%
    # transform the type column and create new columns for each referees id
    # and name
    pivot_wider(names_from = type,
                values_from = c(idReferee, refereeName)) %>%
    ungroup() %>%
    select(-grp)
  
  # bind the fixtures_frame and the cleaned referee frame together
  #fixtures_with_refs_clean <- cbind(fixtures_frame, referees_clean)
  
  # return the cleaned fixtures frame
  return(fixtures_with_refs_clean)
  
}



############### get_lineups_by_fixture_id #################
# inputs: fixture_id
# outputs: should return a data frame which contains the lineup for a given fixture_id

get_lineups_by_fixture_id <- function(fixture_id) {
  # For this API one first have to get a token which is valid for one hour
  # (this is done with a POST).
  # This token then must be used instead of the API key when retrieving data
  access_token_elena <- get_elena_access_token()
  
  # create a variable to control the paginated endpoint
  page <- 1
  
  # create the url endpoint for the seasons
  url_lineup <-
    paste0(
      "https://football.elenasport.io/v2/fixtures/",
      fixture_id,
      "/lineups?page=",
      page
    )
  
  # retrieve the data from the API
  lineup_response <- GET(url_lineup,
                         add_headers('Authorization' =
                                       paste0("Bearer ", access_token_elena)))
  
  lineup_content <- content(lineup_response)
  
  # extract the content of the first page and store it as a data frame
  lineup_frame <- list.stack(lineup_content$data)
  
  # extract information about whether there is a next page
  has_next <- lineup_content$pagination$hasNextPage
  
  # iterate through the pages as long as there is a next page
  while (has_next) {
    # create the url endpoint for the current page
    url_lineup <-
      paste0(
        "https://football.elenasport.io/v2/fixtures/",
        fixture_id,
        "/lineups?page=",
        page
      )
    
    # retrieve the data from the API
    lineup_response <- GET(url_lineup,
                           add_headers('Authorization' =
                                         paste0("Bearer ", access_token_elena)))
    
    # extract the content of the response
    lineup_content <- content(lineup_response)
    
    # reset the has_next variable
    has_next <- lineup_content$pagination$hasNextPage
    
    # extract the content of the current page and store it as a data frame
    current_lineup_frame <- list.stack(lineup_content$data)
    
    # append the players of the current page to the frame of the previous pages
    lineup_frame <- bind_rows(lineup_frame, current_lineup_frame)
    
    # increase the page counter
    page <- page + 1
    
  }
  
  # return data frame which contains the lineup and the rotation substitute players
  return(lineup_frame)
  
}