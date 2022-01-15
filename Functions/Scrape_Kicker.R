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
  
  print(paste0("Liga:", league))
  
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
