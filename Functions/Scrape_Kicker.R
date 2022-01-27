############## get_fixture_stats_kicker #################
# inputs: league_name, season
# outputs: returns all stats available for the games played in a given season
# for a given league
get_fixture_stats_kicker <- function(league, season, port = 4321L, matchday = NULL){
  # we want to extract the last two digits of the season because the date in the
  # kicker url is given as "season-season+1" where the second one only has two digits
  # e.g., "2013-14"
  last_two_digits <- substr(season, start = nchar(season)-2+1, stop = nchar(season)) %>%
    as.numeric()
  
  # convert the season into a number
  season <- as.numeric(season)
  
  # paste together the url we want to scrape
  url <- paste0("https://www.kicker.de/", league, "/spieltag/",
                season, "-", last_two_digits+1, "/")
  
  
  # create a driver from Rselenium
  rD <- rsDriver(browser = "firefox", port = port)
  
  # get the client
  remDr <- rD$client
  
  # set time outs to give the page the change to first fully load before
  # we try to get information form it
  remDr$setTimeout(type = "implicit", milliseconds = 10000)
  remDr$setTimeout(type = "page load", milliseconds = 10000)
  
  # navigate to the created url
  remDr$navigate(url)

  # if we want to scrape the whole season
  if(is.null(matchday)){
    # extract the number of matchdays dynamically
    number_matchdays <- read_html(remDr$getPageSource()[[1]]) %>%
      html_nodes(xpath = paste0("//select[@class='kick__head-dropdown__select']",
                                "//option[@value][contains(text(), 'Spieltag')]")) %>%
      # get the attributes
      html_attrs() %>%
      # we need to subtract 1 because the last element is "ALL" which we do not want
      length() - 1
    
    # set the starting matchday to 1
    starting_matchday <- 1
    # set the ending matchday to the number of matchdays available
    ending_matchday <- number_matchdays
    
    # if we only want to scrape a specific matchday
  } else {
    # set the starting and the ending matchday to the selected matchday
    starting_matchday <- matchday
    ending_matchday <- matchday
  }
  
  
  # variable to store the match stats over the whole season
  all_season_stats <- NULL
  
  # iterate over all matchdays in between the selected range
  for(matchday in starting_matchday:ending_matchday){
    print(paste0("Matchday: ", matchday))
    
    final_url <- paste0(url, matchday)
    
    # navigate to the final url
    remDr$navigate(final_url)
    
    # extract all the links to the analysis of the matches to be able
    # to iterate over them
    match_refs <- read_html(remDr$getPageSource()[[1]]) %>%
      html_nodes(xpath = "//div[@class='kick__v100-gameList__gameRow__stateCell']/a") %>%
      html_attr("href")
    
    # variable to store all stats for the current matchday
    matchday_stats <- NULL
    
    # iterate over all matches
    for(match in 1:length(match_refs)){
      print(paste0("Match: ", match))
      
      # set the tail of the url from "analyse" to spielinfo to get to the
      # url that contain information about the fixture date and time
      date_endpoint <- str_replace(match_refs[match], pattern = "analyse",
                              replacement = "spielinfo")
      
      # paste the date url together
      match_date_url <- paste0("https://www.kicker.de", date_endpoint)
      
      # navigate to the date url of the current match
      remDr$navigate(match_date_url)
      
      Sys.sleep(2)
      
      # extract the fixture date and time information
      fixture_date_time <- read_html(remDr$getPageSource()[[1]]) %>%
        html_nodes(xpath = "//div[@class='kick__gameinfo-block']//p[span[@class='kick__weekday_box']]") %>%
        html_text(trim = TRUE) %>%
        # extract the date and the time
        str_extract_all(pattern = "[0-9]+.*") %>%
        .[[1]] %>%
        # split the string at the "," so that the first element represents the date
        # and the second one the time
        str_split(pattern = ",")
      
      # convert the date and the time in separate variables
      fixture_date <- fixture_date_time[[1]][1] %>%
        # convert the date in a proper format
        dmy()
      
      # break the loop if the date is in the fuxture
      if(fixture_date >= Sys.Date()){
        break
      }
      
      fixture_time <- fixture_date_time[[1]][-1] %>%
        trimws()
      
      Sys.sleep(1)
      
      # set the tail of the url from "analyse" to "spieldaten" to get to the
      # url that contain information about the stats
      stats_endpoint <- str_replace(match_refs[match], pattern = "analyse",
                                    replacement = "spieldaten")
      
      # paste the stats url together
      stats_url <- paste0("https://www.kicker.de", stats_endpoint)
      
      # navigate to the stats url of the current match
      remDr$navigate(stats_url)
      
      # extract the html of the page
      page_html <- read_html(remDr$getPageSource()[[1]])
      
      # extract team names
      team_names <- page_html %>%
        html_nodes(xpath = "//div[@class='kick__compare-select__input kick__compare-select__title']/a") %>%
        html_text()
      
      # extract stats of the game
      match_stats <- page_html %>%
        # get all relevant xpath elements
        html_nodes(xpath = paste0("//div[@class='kick__stats-bar__value kick__stats-bar__value--opponent1']",
                                  "|", "//div[@class='kick__stats-bar__value kick__stats-bar__value--opponent2']",
                                  "|", "//div[@class='kick__stats-bar__title']")) %>%
        html_text() %>%
        # convert the data into a matrix (and then data frame)
        # with 3 columns
        matrix(ncol = 3, byrow = TRUE) %>%
        data.frame() %>%
        # remove those where the two values contain a "-" which indicates
        # that these are values from the lower part of the page that are not
        # relevant for us
        filter(X1 != "-",
               X3 != "-") %>%
        # reorder and rename the variables accordingly
        select(statistic = X2, 
               value_home_team = X1, 
               value_away_team = X3)
      
      # we want each game to be a data frame of 2 rows and 13 columns of stats
      # and additionally information about the teams and the fixture
      # therefore we need to transpose the data frame to convert it to wide format
      match_stats <- t(match_stats) %>%
        data.frame()
      
      # set new variable names
      colnames(match_stats) <- c("goals", "shots_on_goal", "distance_ran",
                                 "passes", "passes_accurate", "passes_failed",
                                 "passing_accuracy", "possession", "duel_quota",
                                 "fouls_committed", "fouls_against", "offside",
                                 "corners")
      
      # remove the first row of the data frame
      match_stats <- match_stats[-1, ]
      
      # add the fixture date, time and the team names as variables
      match_stats <- match_stats %>%
        mutate(date = fixture_date,
               time = fixture_time) %>%
        cbind(., team_names) %>%
        # reorder the variables
        select(date, time,
               team_name = team_names, everything()) %>%
        # add a flag variable to keep track which team is the home team
        mutate(is_home_team = c(TRUE, FALSE))
      
      # bind the data of the current match to the frame for the current matchday
      # by adding the rows
      matchday_stats <- bind_rows(matchday_stats,
                                  match_stats)
        
      Sys.sleep(2)
    }
    
    
    if(fixture_date < Sys.Date()){
      # add the matchday as a variable
      matchday_stats <- matchday_stats %>%
        mutate("matchday" = matchday)
    }
    
    # append the current matchday stats to the variable which stores all stats
    # of the season
    all_season_stats <- bind_rows(all_season_stats,
                                  matchday_stats) 
  }
  
  all_season_stats <- all_season_stats %>%
    # add a column for the season and the league
    mutate(season_start_year = season,
           season_end_year = (season + 1),
           league = str_to_title(league),
           distance_ran_km = str_replace(str_remove(distance_ran, " km"), pattern = ",",
                                         replacement = "."),
           passing_accuracy = str_remove(passing_accuracy, "%"),
           possession = str_remove(possession, "%"),
           duel_quota = str_remove(duel_quota, "%")) %>%
    # reorder variables
    select(-distance_ran) %>%
    select(league, season_start_year,
           season_end_year,
           matchday,
           date, time,
           team_name, is_home_team, everything()) %>%
    # make all suited variables to numeric
    mutate(across(c(goals:distance_ran_km), as.numeric))
    
  
  # reset rownames
  rownames(all_season_stats) <- 1:nrow(all_season_stats)
  
  # close the driver (client) and the server
  remDr$close()
  rD$server$stop()
  rm(rD)
  gc()
  
  
  return(all_season_stats)
  
}




get_rest_days_kicker2 <- function(league, season, port = 7777L, matchday = NULL){
  # we want to extract the last two digits of the season because the date in the
  # kicker url is given as "season-season+1" where the second one only has two digits
  # e.g., "2013-14"
  last_two_digits <- substr(season, start = nchar(season)-2+1, stop = nchar(season)) %>%
    as.numeric()
  
  # convert the season into a number
  season <- as.numeric(season)
  
  # paste together the url we want to scrape
  url <- paste0("https://www.kicker.de/", league, "/vereine/",
                season, "-", last_two_digits + 1)
  
  all_matches_data <- NULL
  
  # create a driver from Rselenium
  rD <- rsDriver(browser = "firefox", port = port)
  
  # get the client
  remDr <- rD$client
  
  # set time outs to give the page the change to first fully load before
  # we try to get information form it
  remDr$setTimeout(type = "implicit", milliseconds = 50000)
  # remDr$setTimeout(type = "page load", milliseconds = 10000)
  
  # navigate to the created url
  remDr$navigate(url)
  
  # extract the html of the page
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  # get all the club links
  club_refs <- page_html %>%
    html_nodes(xpath = "//td[@class='kick__t__a__l kick__table--ranking__metainfo']/a") %>%
    html_attr("href") %>%
    # match just the links that contain the string "splielplan"
    str_match(., pattern = ".*spielplan.*") %>%
    # remove all that did not match that string
    .[!is.na(.)]
  
  Sys.sleep(1)
  
  for(i in 1:length(club_refs)){
    # paste together the final club url
    final_club_url <- paste0("https://www.kicker.de", club_refs[i])
    
    # navigate to the created url
    remDr$navigate(final_club_url)
    
    # extract the html of the page
    page_html <- read_html(remDr$getPageSource()[[1]])
    
    # get the name of the current name
    curr_team_name <- page_html %>%
      html_nodes(xpath = "//div[@class='kick__head-breadcrumb__items']//span[@class='kick__head-breadcrumb__item kick__head-breadcrumb__item--rest ']") %>%
      html_text(trim = TRUE)
    
    # get all matches of this team 
    all_matches <- page_html %>%
      html_nodes(xpath = paste0("//table[@class='kick__table kick__table--gamelist",
                                " kick__table--gamelist-timeline']"))
    
    all_matches_curr_team <- NULL 
    
    Sys.sleep(1)
    
    for(curr_month in 1:length(all_matches)){
      curr_month_information <- NULL
      
      # get the base data for the month
      curr_month_data <- all_matches[curr_month][[1]] %>%
        html_nodes(xpath = ".//tr")
      
      # extract the match date and convert it into an actual date
      curr_month_dates <- curr_month_data %>%
        html_nodes(xpath = ".//td[1]") %>%
        html_text(trim = TRUE) %>%
        str_remove_all(., pattern = ".*, ") %>%
        dmy()
      
      for(match in 1:length(curr_month_dates)){
        curr_match_data <- curr_month_data[[match]] 
        
        curr_date <- curr_match_data %>%
          html_nodes(xpath = ".//td[1]") %>%
          html_text() %>%
          str_split(pattern = ", ") %>%
          unlist() %>%
          .[2] %>%
          dmy()
        
        # break the loop if the date is in the future
        if(curr_date >= Sys.Date()){
          break
        }
        
        league_info <- curr_match_data %>%
          html_nodes(xpath = ".//td[3]") %>%
          html_text() %>%
          str_split(., pattern = ", ") %>%
          unlist() %>%
          str_remove_all(., pattern = "Gr.") %>%
          .[. != ""]
        
        # split the league infos into the matchdays and the actual league name
        # by extracting every 2second element to be the matchday starting at the 
        # second position and every other element to be the league
        league_name <- league_info %>%
          .[1]
        
        major_leagues <- c("Bundesliga", "Bundesliga 2", "Premier League",
                                "La Liga", "Serie A", "Ligue 1")
        
        league_name <- ifelse(league_name == "BL",
                              "Bundesliga",
                              ifelse(league_name == "2.BL",
                                     "Bundesliga 2",
                                     ifelse(league_name == "England",
                                            "Premier League",
                                            ifelse(league_name == "SPA",
                                                   "La Liga",
                                                   ifelse(league_name == "Italien",
                                                          "Serie A",
                                                          ifelse(league_name == "FRA",
                                                                 "Ligue 1",
                                                                 league_name
                                                          )
                                                   )
                                            )
                                     )
                              ))
        
        if(!(league_name %in% major_leagues)){
          next
        }
        
        curr_matchday <- league_info %>%
          .[2] %>%
          str_extract(pattern = "[0-9]+") %>%
          as.numeric()
        
        # curr_matchday <- ifelse(is.na(curr_matchday),
        #                         "NA",
        #                         curr_matchday)
        
        print(paste0("curr_matchday: ", curr_matchday))
        print(paste0("given matchday: ", matchday))
        
        if(curr_matchday == matchday){
          print(paste0("Current club: ", i))
          date_pos_in_vector <- which(ymd(curr_date) == curr_month_dates)
          number_matches <- date_pos_in_vector
          starting_iteration <- number_matches - 1
          
          if(starting_iteration == 0){
            prev_month_data <- all_matches[(curr_month - 1)][[1]] %>%
              html_nodes(xpath = ".//tr")
            
            # extract the match date and convert it into an actual date
            prev_month_dates <- prev_month_data %>%
              html_nodes(xpath = ".//td[1]") %>%
              html_text(trim = TRUE) %>%
              str_remove_all(., pattern = ".*, ") %>%
              dmy()
            
            previous_month_match <- prev_month_data[[length(prev_month_dates)]] %>%
              get_match_infos(., curr_team_name)
            
            starting_iteration <- 1
            
            curr_month_match <- get_match_infos(curr_month_data[[starting_iteration]],
                                                curr_team_name)
            
            curr_month_information <- bind_rows(curr_month_information,
                                                previous_month_match,
                                                curr_month_match)
            
            save(curr_month_information, file = "curr_month_with_prev_information.RData")
            
          } else {
            curr_month_match1 <- get_match_infos(curr_month_data[[starting_iteration]],
                                                 curr_team_name)
            
            Sys.sleep(1)
            curr_month_match2 <- get_match_infos(curr_month_data[[date_pos_in_vector]],
                                                 curr_team_name)
            
            Sys.sleep(1)
            
            curr_month_information <- bind_rows(curr_month_information,
                                                curr_month_match1,
                                                curr_month_match2)
            
            save(curr_month_information, file = "curr_month_with_prev_information.RData")
          }
          
        } else {
          next
        }
      }
      
      all_matches_curr_team <- bind_rows(all_matches_curr_team,
                                         curr_month_information)
      
      Sys.sleep(2)
     
    }
    
    # compute the rest days for every match for the current team
    all_matches_curr_team <- all_matches_curr_team %>%
      # by computing the difference between two matches and paste it to days
      mutate(rest_days = as.integer(difftime(match_date, lag(match_date, 1), 
                                             units = c("days"))))
      
    
    all_matches_data <- bind_rows(all_matches_data,
                                  all_matches_curr_team)
  }
  
  # do some final mutating of the data
  all_matches_data <- all_matches_data %>%
    # converting specific variables to numeric
    mutate(matchday = as.numeric(str_remove_all(matchday, pattern = ". Sp.*")),
           goals_home = as.numeric(goals_home),
           goals_away = as.numeric(goals_away)) %>%
    # filter only observations that contain matches of the leagues and no
    # other competitions. Also excluding test games
    filter(!is.na(matchday),
           league != "Testspiele") %>%
    # convert the league name into a more appropriate name
    mutate(league = ifelse(league == "BL",
                           "Bundesliga",
                           ifelse(league == "2.BL",
                                  "Bundesliga 2",
                                  ifelse(league == "England",
                                         "Premier League",
                                         ifelse(league == "SPA",
                                                "La Liga",
                                                ifelse(league == "Italien",
                                                       "Serie A",
                                                       ifelse(league == "FRA",
                                                              "Ligue 1",
                                                              league)))))))
  
  # split the data into a home data frame which contains only
  # those observations where the rest days correspond to the home team
  # and a away frame with the rest days that correspond to the away team
  all_matches_data_home <- all_matches_data %>%
    filter(team_name == home_team) %>%
    mutate(rest_days_home = rest_days) %>%
    select(-c(team_name, rest_days))
  # 
  all_matches_data_away <- all_matches_data %>%
    filter(team_name == away_team) %>%
    mutate(rest_days_away = rest_days) %>%
    select(-c(team_name, rest_days))
  # 
  # # join these two frames together into one frame and with this transforming
  # # it a data frame that just contains one row per match
  all_matches_data_complete <- inner_join(all_matches_data_home,
                                          all_matches_data_away,
                                          by = c("match_date", "league",
                                                 "matchday", "home_team",
                                                 "away_team", "goals_home",
                                                 "goals_away"))
  
  all_matches_data_complete <- all_matches_data_complete %>%
    filter(!is.na(rest_days_home),
           !is.na(rest_days_away)) %>%
    arrange(matchday, match_date) 
  
  # map the club names with the mapping function
  all_matches_data_complete$home_team <- sapply(all_matches_data_complete$home_team,
                                                club_name_mapping) %>%
    unname()
  
  all_matches_data_complete$away_team <- sapply(all_matches_data_complete$away_team,
                                                club_name_mapping) %>%
    unname()
  
  # close the driver (client) and the server
  remDr$close()
  rD$server$stop()
  rm(rD)
  gc()
  
  return(all_matches_data_complete)
}


get_rest_days_full_season_kicker <- function(league, season, ports, num_matchdays){
  all_seasons_rest_days <- NULL
  
  for(i in 1:num_matchdays){
    curr_matchday_rest_days <- get_rest_days_kicker2(league, season,
                                                     ports[i],
                                                     matchday = i)
    
    all_seasons_rest_days <- bind_rows(all_seasons_rest_days,
                                       curr_matchday_rest_days)
    
    Sys.sleep(10)
    
  }
  
  return(all_seasons_rest_days)
}


# helper function to extract data of the current match
get_match_infos <- function(match_html, curr_team_name){
  # extract the date of the current match and convert
  # it into an actuald ate
  curr_date <- match_html %>%
      html_nodes(xpath = ".//td[1]") %>%
      html_text() %>%
      str_split(pattern = ", ") %>%
      unlist() %>%
      .[2] %>%
      dmy()
    
    # break the loop if the date is in the future
    if(curr_date >= Sys.Date()){
      break
    }
    
    # extract the league information (matchday and league name)
    league_info <- match_html %>%
      html_nodes(xpath = ".//td[3]") %>%
      html_text() %>%
      str_split(., pattern = ", ") %>%
      unlist() %>%
      str_remove_all(., pattern = "Gr.") %>%
      .[. != ""]
    
    # split the league infos into the matchdays and the actual league name
    # by extracting every 2second element to be the matchday starting at the 
    # second position and every other element to be the league
    league_name <- league_info %>%
      .[1]
    
    matchday <- league_info %>%
      .[2]
    
    # extracting the base data for the match
    match_info <- match_html %>%
      html_nodes(xpath = ".//td[@class='kick__table--gamelist__gamecell']")
    
    # extract the name of the home team
    team_name_home <- match_info %>%
      html_nodes(xpath = ".//div[@class='kick__v100-gameCell__team__name']") %>%
      html_text() %>%
      .[1] %>%
      # .[seq(1, length(.), 2)] %>%
      trimws()
    
    # extract the name of the away team
    team_name_away <- match_info %>%
      html_nodes(xpath = ".//div[@class='kick__v100-gameCell__team__name']") %>%
      html_text() %>%
      # .[seq(2, length(.), 2)] %>%
      .[2] %>%
      trimws()
    
    # extract the goals scored by the home team
    goals_home <- match_info %>%
      html_nodes(xpath = ".//div[@class='kick__v100-scoreBoard__scoreHolder__score']") %>%
      html_text() %>%
      # .[seq(1, length(.), 2)] %>%
      .[1] %>%
      trimws()
    
    # extract the goals scored by the away team
    goals_away <- match_info %>%
      html_nodes(xpath = ".//div[@class='kick__v100-scoreBoard__scoreHolder__score']") %>%
      html_text() %>%
      # .[seq(2, length(.), 2)] %>%
      .[2] %>%
      trimws()
    
    
    # store the data in a temporary data frame
    curr_match_info <- data.frame("match_date" = curr_date, 
                                  "league" = league_name, 
                                  "matchday" = matchday,
                                  "team_name" = curr_team_name,
                                  "home_team" = team_name_home,
                                  "away_team" = team_name_away,
                                  "goals_home" = goals_home, 
                                  "goals_away" = goals_away)

  
  return(curr_match_info)
}

