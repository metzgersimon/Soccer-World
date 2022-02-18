############## get_market_values_over_time_all_leagues #################
# function should return the current current market values for all leagues
# given by a vector (as name and id)
get_market_values_over_time_all_leagues <- function(leagues, league_ids, date){
  # create a variable to store all the market values
  all_leagues_market_values <- NULL
  
  # iterate over all leagues given as a vector of strings
  for(i in 1:length(leagues)){
    # for every league, get the current market values
    curr_league_values <- get_market_values_over_time(leagues[i],
                                                      league_ids[i],
                                                      date)
    
    # check if the market values are null and if so return NULL
    if(is.null(curr_league_values)){
      return(NULL)
    }
    
    # append the curr league values to the overall data frame
    all_leagues_market_values <- bind_rows(all_leagues_market_values,
                                           curr_league_values)
    
    Sys.sleep(30)
  }
  
  return(all_leagues_market_values)
}




############## get_market_values_over_time #################
# inputs: league, league_id
# outputs: should return a data frame for a series of season in a certain league
# that contain information about a teams market value development over time
get_market_values_over_time <- function(league, league_id, date = NULL) {
  # create empty variable to store all market values
  market_values_over_time <- NULL
  
  # create base url to extract the market values
  url <-
    paste0(
      "https://www.transfermarkt.com/",
      league,
      "/marktwerteverein/",
      "wettbewerb/",
      league_id,
      "/plus/1",
      "?stichtag="
    )
  
  # extract all available cut off dates (back to the start_date)
  available_cut_off_dates <-
    read_html(paste0(url, "2010-11-01")) %>%
    html_nodes(xpath = "//option[@value]") %>%
    html_attrs()
  
  # clean the cut off dates by extracting the values from the list above
  # (take always the first element in the list (tail(1)), sort them ascending
  # and unname them)
  cut_off_dates <-
    rapply(available_cut_off_dates, function(x)
      tail(x, 1)) %>%
    sort() %>%
    unname()
  
  # converting the cut off dates into actual dates
  cut_off_dates <- ymd(cut_off_dates)
  
  # in case we have given a date we only take these cut off dates
  # that are greater that the given date
  if(!is.null(date)){
    cut_off_dates <- cut_off_dates[cut_off_dates > ymd(date)]
    
    # check if the cut_off_dates element now is empty
    # if so, return NULL because there is no new data to scrape
    if(length(cut_off_dates) == 0){
      return(NULL)
    }
  }
  
  # iterate through all possible cut off dates
  for (i in 1:length(cut_off_dates)) {
    print(paste0("Current Date: ", cut_off_dates[i]))
    print(paste0("Diff to end: ", length(cut_off_dates) - i))
    # create the final url based on the current selected
    # cut off date
    end_url <- paste0(url, cut_off_dates[i])
    
    # extract the html for the given url
    page_html <- read_html(end_url)
    
    # create a data frame with the important information for a given date
    market_value_then <- page_html %>%
      # extract only the body of the table
      html_nodes(xpath = "//table/tbody") %>%
      # convert the extracted nodes into a table
      html_table() %>%
      .[-1] %>%
      # convert it into a data frame
      data.frame() %>%
      # remove those columns which are empty (contain only NAs)
      remove_empty("cols") %>%
      # drop columns we do not want
      select(-any_of(c("X1", "X4"))) %>%
      # rename all columns
      rename(
        club = X3,
        value_then_euro = X5,
        squad_size_then = X6,
        current_market_value_euro = X7,
        current_squad_size = X8,
        abs_diff_market_value = X9,
        rel_diff_market_value = X10
      ) %>%
      # transform the date column from character to date in the YYYY-MM-DD format
      # additionally, convert the squad sizes to integers
      mutate(
        cut_off_day = ymd(cut_off_dates[i]),
        squad_size_then = as.numeric(squad_size_then),
        current_squad_size = as.numeric(current_squad_size),
        value_then_euro = str_remove(value_then_euro, pattern = "\u20ac"),
        # check if the market value ends with bn, m or Th.
        value_then_mil_euro = ifelse(endsWith(value_then_euro, "bn"),
                                     as.numeric(str_remove(value_then_euro, 
                                                           pattern = "bn"))*1000,
                                     ifelse(endsWith(value_then_euro, "m"),
                                            as.numeric(str_remove(value_then_euro,
                                                                  pattern = "m")),
                                            as.numeric(str_remove(value_then_euro, 
                                                                  pattern = "Th."))/1000)),
        current_market_value_euro = str_remove(current_market_value_euro, 
                                               pattern = "\u20ac"),
        # check if the market value ends with bn, m or Th.
        current_value_mil_euro = ifelse(endsWith(current_market_value_euro, "bn"),
                                        as.numeric(str_remove(current_market_value_euro, 
                                                              pattern = "bn"))*1000,
                                        ifelse(endsWith(current_market_value_euro, "m"),
                                               as.numeric(str_remove(current_market_value_euro,
                                                                     pattern = "m")),
                                               as.numeric(str_remove(current_market_value_euro, 
                                                                     pattern = "Th."))/1000)),
        abs_diff_market_value = str_remove(abs_diff_market_value, pattern = "\u20ac"),
        # check if the market value ends with bn, m or Th.
        abs_diff_value_mil_euro = ifelse(endsWith(abs_diff_market_value, "bn"),
                                         as.numeric(str_remove(abs_diff_market_value, 
                                                               pattern = "bn"))*1000,
                                         ifelse(endsWith(abs_diff_market_value, "m"),
                                                as.numeric(str_remove(abs_diff_market_value,
                                                                      pattern = "m")),
                                                as.numeric(str_remove(abs_diff_market_value, 
                                                                      pattern = "Th."))/1000)),
        rel_diff_value = as.numeric(trimws(str_remove(rel_diff_market_value, "%"))),
        league = str_to_title(league)) %>%
      # reorder (and rename) the variables
      select(league, date = cut_off_day, club,
             value_then_mil_euro, squad_size_then,
             current_value_mil_euro, current_squad_size, abs_diff_value_mil_euro,
             rel_diff_value)
    
    # remap the league name
    market_value_then <- market_value_then %>%
      mutate(league = ifelse(league == "2-Bundesliga",
                             "Bundesliga 2",
                             ifelse(league == "Premier-League",
                                    "Premier League",
                                    ifelse(league == "Premier-League",
                                           "Premier League",
                                           ifelse(league == "Ligue-1",
                                                  "Ligue 1",
                                                  ifelse(league == "Primera-Division",
                                                         "La Liga",
                                                         ifelse(league == "Serie-A",
                                                                "Serie A",
                                                                league)))))))
    
    
    
    # map the club names
    market_value_then$club <- sapply(market_value_then$club,
                                     club_name_mapping) %>% 
      unname()
    
    # append the data for the current cut_off_day to the data frame which stores
    # the market values for all cut off dates
    market_values_over_time <- bind_rows(market_values_over_time,
                                         market_value_then)
    
    # wait for 2 seconds before scraping the next page
    Sys.sleep(2)
  }
  
  # return the full data frame of market values over time
  return(market_values_over_time)
  
}



############## get_squads_by_season_all_leagues #################
# function should return the current squads of all leagues
get_squads_by_season_all_leagues <- function(leagues, league_ids, season){
  # create a variable to store all the squads
  all_leagues_squads <- NULL
  
  # iterate over all leagues given as a vector of strings
  for(i in 1:length(leagues)){
    # for every league, get the current squads of each team
    curr_league_squads <- get_squads_by_season(leagues[i],
                                               league_ids[i],
                                               season)
    
    # append the curr league squads to the overall data frame
    all_leagues_squads <- bind_rows(all_leagues_squads,
                                    curr_league_squads)
    
    Sys.sleep(30)
  }
  
  return(all_leagues_squads)
}




############## get_club_squad_urls #################
# inputs: league, league_id, season
# outputs: returns the url to the page of every club available
# for the given league and season on transfermarkt
get_club_squad_urls <- function(league, league_id, season){
  # paste whole url together
  final_url <- paste0("https://www.transfermarkt.com/", league,
                      "/startseite/wettbewerb/", league_id,
                      "/saison_id/", season)
  
  # extract the urls from the page
  league_clubs <- read_html(final_url) %>%
    html_nodes(xpath = "//table/tbody/tr/td/a") %>%
    # get the hyperlinks stored in the page
    html_attr("href") %>%
    # get only unique links
    unique() %>%
    # get only those where the string "kader" is contained
    .[grep(pattern = "kader", .)]
  
  # return all available club urls
  return(league_clubs)
}



############## get_squads_by_season #################
# inputs: league, league_id, season
# outputs: returns all squads for every club in a given league and season
get_squads_by_season <- function(league, league_id, season){
  # use the helper function get_club_squad_urls
  # to get all urls for the clubs to use them to iterate through
  club_names_url <- get_club_squad_urls(league, league_id, season)
  
  # create empty variable to store the information later
  all_squads_in_season <- NULL
  
  # compute the max season
  if(month(Sys.Date()) < 7){
    max_season <- year(Sys.Date()) - 1 
  } else {
    max_season <- year(Sys.Date())
  }
  
  league <- str_to_title(league)
  
  # map the league names 
  league <- ifelse(league == "Primera-Division",
                   "La Liga",
                   ifelse(league == "2-Bundesliga",
                          "Bundesliga 2",
                          ifelse(league == "Ligue-1",
                                 "Ligue 1",
                                 ifelse(league == "Premier-League",
                                        "Premier League",
                                        ifelse(league == "Serie-A",
                                               "Serie A",
                                               league)))))
  
  # iterate through all club urls
  for(i in 1:length(club_names_url)){
    # paste the final url together
    final_url <- paste0("https://www.transfermarkt.com", club_names_url[i],
                        "/plus/1")
    
    # store the page once to do not have to scrape it all the time
    page_html <- read_html(final_url)
    
    # print the current iteration for debugging
    print(i)
    
    # extract the name of the current club
    curr_club <- page_html %>%
      html_nodes(xpath = "//div[@class='dataName']//h1[@itemprop='name']//span") %>%
      html_text(trim = TRUE)
                    
    # extract the name and nationality of the players playing
    # in the current club
    name <- page_html %>%
      html_nodes(xpath = paste0("//table[@class='inline-table']//td[@class='hauptlink']/a")) %>%
      html_text(trim = TRUE)
    
    nationality <- page_html %>%
      html_nodes(xpath = "//table[@class='items']/tbody/tr/td[4]/img[1]") %>%
      html_attr("title")
    
    # extract position and number of the players that play in the current club
    position <- page_html %>%
      html_nodes(xpath = paste0("//table[@class='inline-table']//tr[2]/td")) %>%
      html_text(trim = TRUE)
    
    player_number <- page_html %>%
      html_nodes(xpath = paste0("//table[@class='items']//div[@class='rn_nummer']")) %>%
      html_text(trim = TRUE)
    
    # get a bunch of player information
    player_information <- page_html %>%
      html_nodes(xpath = "//table[@class='items']//tr") 
    
    # extract the birth date
    birth_date <- player_information %>% 
      html_nodes(xpath = paste0(".//td[3]")) %>%
      html_text(trim = TRUE)
    
    # based on the season the structure of the web page is different, i.e.,
    # if it is the current season (=max_season).
    # For the current season there is no column "current club", for obvious
    # reasons, and we have to take this case into account and get the correct variables
    # We get information such as height, contract date or market value
    if(season == max_season){
      height <- player_information %>% 
        html_nodes(xpath = paste0(".//td[5]")) %>%
        html_text(trim = TRUE)
      
      foot <- player_information %>% 
        html_nodes(xpath = paste0(".//td[6]")) %>%
        html_text(trim = TRUE)
      
      contract_date <- player_information %>% 
        html_nodes(xpath = paste0(".//td[9]")) %>%
        html_text(trim = TRUE)
      
      market_value <- player_information %>% 
        html_nodes(xpath = paste0(".//td[10]")) %>%
        html_text(trim = TRUE)
      
      joined_date <- player_information %>%
        html_nodes(xpath = ".//td[7]") %>%
        html_text()
      
      previous_clubs <- page_html %>%
        html_nodes(xpath = paste0("//div[@class='responsive-table']//table[@class='items']",
                                  "//td[8]/a/img")) %>%
        html_attr("alt")
      
    } else {
      height <- player_information %>% 
        html_nodes(xpath = paste0(".//td[6]")) %>%
        html_text(trim = TRUE)
      
      foot <- player_information %>% 
        html_nodes(xpath = paste0(".//td[7]")) %>%
        html_text(trim = TRUE)
      
      contract_date <- player_information %>% 
        html_nodes(xpath = paste0(".//td[10]")) %>%
        html_text(trim = TRUE)
      
      market_value <- player_information %>% 
        html_nodes(xpath = paste0(".//td[11]")) %>%
        html_text(trim = TRUE)
      
      joined_date <- player_information %>%
        html_nodes(xpath = ".//td[8]") %>%
        html_text()
      
      previous_clubs <- page_html %>%
        html_nodes(xpath = paste0("//div[@class='responsive-table']//table[@class='items']",
                                  "//td[9]/a/img")) %>%
        html_attr("alt")
    }
    
    # we have to deal with the case that the joined_date is NA
    # in this case we want to set the previous_club variable to NA
    for(date in 1:length(joined_date)){
      if(joined_date[date] == "-"){
        previous_clubs <- append(previous_clubs, 
                                       NA, after = (date - 1))
      }
    }
    
    # convert it from a vector into a data frame
    # with 11 columns
    curr_squad_information <- c(name, player_number, position, nationality,
                                birth_date, height, foot, contract_date,
                                market_value, previous_clubs, joined_date) %>%
      matrix(ncol = 11, byrow = FALSE) %>%
      data.frame() %>%
      # rename the variables properly
      rename(player_name = X1,
             player_number = X2,
             player_position = X3,
             player_nationality = X4,
             player_birth_date_age = X5,
             player_height = X6,
             player_foot = X7,
             player_contract_date = X8,
             player_market_value = X9,
             player_previous_club = X10,
             player_joining_date = X11) %>%
      # separate the birth_date_age column
      # where the age is in the birth_date column in parantheses
      # into two separate columns player_birth_date and player_age
      separate(col = player_birth_date_age,
               into = c("player_birth_date", "player_age"),
               sep = "\\(|\\)") %>%
      # transform various columns (dates to actual dates, numerics into a
      # actual numerics and missing values to NAs)
      mutate(player_birth_date = mdy(player_birth_date),
             player_age = as.integer(player_age),
             player_number = as.integer(player_number),
             player_height = as.numeric(str_replace(str_remove(player_height, pattern = " m"), 
                                             pattern = ",",
                                             replacement = ".")),
             player_foot = ifelse(player_foot == "",
                                  NA,
                                  player_foot),
             # dates in appropriate format
             player_joining_date = mdy(player_joining_date),
             player_contract_date = mdy(player_contract_date),
             # remove euro sign from market value
             player_market_value = str_extract(player_market_value, 
                                               pattern = "[0-9]+.*")) %>%
      # transform the market value by converting the numbers in a unified format
      mutate(player_market_value_in_million_euro = ifelse(endsWith(player_market_value, "m"),
                                                          as.numeric(str_remove(player_market_value, 
                                                                                pattern = "m")),
                                                          as.numeric(str_remove(player_market_value, 
                                                                                pattern = "Th."))/1000),
             club = curr_club,
             league_name = str_to_title(league),
             season_year = season) %>%
      # map the league names
      mutate(league_name = league) %>%
      # reorder (and rename) the variables
      select(league = league_name, season = season_year,
             club, player_name, player_number, player_position,
             player_nationality, player_birth_date, player_age,
             player_height, player_foot, player_contract_date,
             player_previous_club, player_joining_date,
             player_market_value_in_million_euro)
    
    # map the club names
    curr_squad_information$club <- sapply(curr_squad_information$club,
                                          club_name_mapping) %>%
      unname()
    
    curr_squad_information$player_previous_club <- 
      sapply(curr_squad_information$player_previous_club,
             club_name_mapping) %>%
      unname()
    
    
    # bind all together into the earlier created variable
    # by bind_rows (more flexible than rbind)
    all_squads_in_season <- bind_rows(all_squads_in_season,
                                      curr_squad_information)
    
 
    
    Sys.sleep(5)
  }
  
  # close all the connections
  closeAllConnections()
  gc()
  
  # return all squads for this season
  return(all_squads_in_season)
  
}



############## get_lineups_by_season_tm #################
# inputs: league, league_id, season
# outputs: returns all lineups for ended fixtures in a given season
get_lineups_by_season_tm <- function(league, league_id, season, matchday = NULL){
  # paste together the url we want to scrape
  url <- paste0("https://www.transfermarkt.com/", league, "/spieltagtabelle",
                      "/wettbewerb/", league_id, "?saison_id=",
                      season)
  
  # create an empty list to store our information
  lineup_information <- NULL
  
  
  league <- str_to_title(league)
  
  # map the league names 
  league <- ifelse(league == "Primera-Division",
                   "La Liga",
                   ifelse(league == "2-Bundesliga",
                          "Bundesliga 2",
                          ifelse(league == "Ligue-1",
                                 "Ligue 1",
                                 ifelse(league == "Premier-League",
                                        "Premier League",
                                        ifelse(league == "Serie-A",
                                               "Serie A",
                                               league)))))
  
  # create a flag variable to check whether the match is in the future
  is_future_match <- FALSE
  
  # get an random port 
  port <- randomPort()
  
  # create a driver from Rselenium without a browser windows
  rD <- rsDriver(browser = "firefox",# extraCapabilities = list(
   # "moz:firefoxOptions" = list(
    #  args = list('--headless'))),
    port = port)
  
  # get the client
  remDr <- rD$client
  
  # set time outs to give the page the change to first fully load before
  # we try to get information form it
  remDr$setTimeout(type = "implicit", milliseconds = 50000)
  remDr$setTimeout(type = "script", milliseconds = 50000)
  remDr$setTimeout(type = "page load", milliseconds = 500000)
  
  # use the external function set_implicit_wait
  set_implicit_wait(remDr, milliseconds = 50000)
  
  # navigate to the created url
  remDr$navigate(url)
  
  # wait for 2 seconds to be able to let the cookie window open up
  Sys.sleep(2)
  
  # find the cookie frame
  cookie_frame <- remDr$findElement(using = 'xpath', 
                                    "//iframe[@title='SP Consent Message']")
  # switch to the pop-up cookies frame
  remDr$switchToFrame(cookie_frame)
  
  
  Sys.sleep(2)
  
  # access the "ACCEPT ALL" button
  cookie_frame_accept_button <- remDr$findElement(using = 'xpath', 
                                                  "//button[@title='ACCEPT ALL']")
  # now click the accept all button
  cookie_frame_accept_button$clickElement()
  
  # after that we have to switch back to the default frame
  remDr$switchToFrame(NA)
  
  # extract the html for the given url
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  # check if a matchday is given via parameter
  # if not we just iterate over the whole season
  if(is.null(matchday)){
    # extract the number of matchdays by getting the length
    # of the elements
    number_matchdays <- page_html %>%
      html_nodes(xpath = "//select[@name='spieltag']/option[@value]") %>%
      html_attrs() %>%
      length()

    # set the starting matchday to 1 and the ending matchday
    # to the number of matchdays, i.e., we scrape the whole season
    starting_matchday <- 1
    ending_matchday <- number_matchdays
    
    # otherwise we just scrape the selected matchday and therefore
    # set the starting and ending matchday accordingly
  } else {
    starting_matchday <- matchday
    ending_matchday <- matchday
  }
  
  # iterate through all matchdays
  for(i in starting_matchday:ending_matchday){
    
    # if the flag variable for a future match is TRUE we break the loop
    if(is_future_match){
      break
    }

    # print for debugging
    print(paste0("Matchday ", i))
    
    # paste together the final url
    final_url <- paste0(url, "&spieltag=", i)

    # navigate to the final url
    remDr$navigate(final_url)
    
    # extract the html for the given url
    page_html <- read_html(remDr$getPageSource()[[1]])
    
    # get the urls for all matches taking place at the
    # given matchday
    matchday_refs <- page_html %>%
      html_nodes(xpath = "//a[@title='Match report']") %>%
      html_attr("href")
  
    # if there are no matchdays, we return the current lineup data
    if(length(matchday_refs) == 0){
      return(lineup_information)
    }

    # iterate through all matches
    for(j in 1:length(matchday_refs)){
      
      # repeat the process until we got all data we want
      repeat{
        # print for debugging
        print(paste0("Match_number ", j))
        
        # paste the url we want to scrape data from
        match_url <- paste0("https://www.transfermarkt.com",
                            matchday_refs[j])
        
        # navigate to the url of the specific match
        remDr$navigate(match_url)
        
        Sys.sleep(2)
        
        # store the webpage
        page_html <- read_html(remDr$getPageSource()[[1]])
        
        Sys.sleep(1)
        
        # extract the formations for each team
        formations <- page_html %>%
          html_nodes(xpath = paste0("//div[@class='large-7 aufstellung-vereinsseite",
                                    " columns small-12 unterueberschrift ",
                                    "aufstellung-unterueberschrift']")) %>%
          html_text(trim = TRUE)
        
        Sys.sleep(1)
        
        
        # find the button which leads to the lineup page  
        lineups_button <- remDr$findElement(using = 'xpath', 
                                            "//ul[@id='submenu']//li[@id='line-ups']/a")
      
        Sys.sleep(2)
        
        # hit the enter key on the line-ups button
        lineups_button$sendKeysToElement(list("line-ups", key = "enter"))
        
        Sys.sleep(2)
        
        # get the current url to be able to go to this url again and again
        # repeat this process until the url actually contains the string
        # "aufstellung" which means that we are on the line ups page
        repeat{
          # extract current url
          curr_url <- remDr$getCurrentUrl()
          
          # check if the url contains the string "aufstellung"
          if(str_detect(curr_url[[1]], pattern = "aufstellung")){
            break
          }
        }
        
        
        
        # repeat this process until the data we want to get first
        # i.e., date, teams and attendance are given
        repeat{
          
          # at the beginning we go to the extracted url
          remDr$navigate(curr_url[[1]])
          
          Sys.sleep(2)
        
          # extract the html for the given url
          page_html <- read_html(remDr$getPageSource()[[1]])
          
          Sys.sleep(1)
          
          # store the team names
          team_names <- page_html %>%
            html_nodes(xpath = paste0("//div[@class='sb-team sb-heim']/a[1]",
                                      "|", "//div[@class='sb-team sb-gast']/a[1]")) %>%
            html_attr("title") %>%
            # map the names with my own mapping function club_name_mapping
            sapply(., club_name_mapping) %>%
            unname()
          
          # store the date information
          date_information <- page_html %>%
            # extract nodes
            html_nodes(xpath = paste0("//div[@class='sb-spieldaten']",
                                      "/p[@class='sb-datum hide-for-small']/a[2]",
                                      "|", 
                                      "//div[@class='sb-spieldaten']",
                                      "/p[@class='sb-datum hide-for-small']/text()[3]")) %>%
            html_text(trim = TRUE) %>%
            # split the string by the "|"
            str_split(" \\|") %>%
            # remove all "|"
            str_remove_all(., pattern = "\\|") %>%
            # paste the string together
            paste(., collapse = "") %>%
            str_remove(pattern = ".*, ") %>%
            str_split("\\s")
  
          
          # convert the date into an actual date of the format YYYY-MM-DD
          date <- date_information[[1]][1] %>%
            mdy()
          
          
          # extract the attendance of this match
          attendance <- page_html %>%
            # extract nodes
            html_nodes(xpath = paste0("//div[@class='sb-spieldaten']",
                                      "/p[@class='sb-zusatzinfos']/",
                                      "span[@class='hide-for-small']/strong")) %>%
            html_text(trim = TRUE) %>%
            # remove the word attendance 
            str_remove_all(., pattern = "Attendance: ")
          
          # during the corona crisis there a matches where there is no audience
          if(length(attendance) == 0){
            attendance = "0"
          }
          
          # convert the attendance properly
          # if there is a . in the number we convert it into a numeric and
          # multiply it by 1000 (to get 25 555 from 25.555)
          attendance <- ifelse(str_detect(attendance, pattern = "\\."),
                               as.numeric(attendance) * 1000,
                               as.numeric(attendance))
          
          # referee information
          ref_info <- page_html %>%
            # extract nodes
            html_nodes(xpath = paste0("//div[@class='sb-spieldaten']",
                                      "/p[@class='sb-zusatzinfos']/a")) %>%
            html_text(trim = TRUE)
          
          
          # it is possible that the referee did not get updated and therefore
          # the ref_info element is empty, in that case we want it to be NA
          if(length(ref_info) == 0 & length(date) != 0 & length(team_names) != 0){
            ref_info <- as.character(NA)
          }
          
          
          # check if all values are non-na (or not empty)
          # if so, we can break the repeat-loop
          # if not, we go again into the repeat-loop
          if(length(date) != 0 & length(team_names) != 0 & length(ref_info) != 0){
            break
          }
        }
        
        # check if the extraced date is in the future, 
        # if so, there are no lineups, we set the flag variable to TRUE
        # and break the loop
        if(date >= Sys.Date()){
          is_future_match <- TRUE
          break
        }
        
        # extract the time 
        time_12h <- paste(date_information[[1]][3], date_information[[1]][4], collapse = "")
        print(time_12h)
        # convert it into a 24 hours format
        time <- format(strptime(time_12h, "%I:%M %p"), "%H:%M")
        
        Sys.sleep(1)
        
        # get the data for the starting grid
        
        # get the data for the name and age
        name_and_age_nodes <- page_html %>%
          html_nodes(xpath = "//div[@class='row sb-formation'][1]//table[@class='items']")
        
        # extract the name of the players
        name <- name_and_age_nodes %>%
          html_nodes(xpath = paste0(".//table[@class='inline-table']//tr[1]//td[2]/a")) %>%
          html_text(trim = TRUE)
        
        # extract the player numbers
        player_number <- page_html %>%
          html_nodes(xpath = paste0("//*[@class='row sb-formation'][1]//",
                                    "div[@class='rn_nummer']")) %>%
          html_text(trim = TRUE)
        
        # extract the age of the players
        age <- name_and_age_nodes %>%
          html_nodes(xpath = paste0(".//table[@class='inline-table']//tr[1]//td[2]/text()")) %>%
          html_text(trim = TRUE) %>%
          # remove the "years" and drop NAs
          str_extract(., pattern = "[0-9]+") %>%
          .[!is.na(.)]
        
        # extract the nationality
        nationality <- page_html %>%
          html_nodes(xpath = paste0("//div[@class='row sb-formation'][1]", "
                                  //table[@class='items']/tbody/tr/td[3]/img[1]")) %>%
          html_attr("alt")
        
        # extract the rating home (only available for the Bundesliga)
        rating_home <- page_html %>%
          html_nodes(xpath = paste0("//div[@class='row sb-formation'][1]",
                                    "//div[@class='large-6 columns'][1]",
                                    "//table[@class='items']/tbody/tr/td[4]/span")) %>%
          html_text(trim = TRUE)
        
        # and the rating away
        rating_away <- page_html %>%
          html_nodes(xpath = paste0("//div[@class='row sb-formation'][1]",
                                    "//div[@class='large-6 columns'][2]",
                                    "//table[@class='items']/tbody/tr/td[4]/span")) %>%
          html_text(trim = TRUE)
        
        # for other leagues then the bundesliga there is often no rating available
        # also, if there is a rating for the leagues it happens that only one team
        # gets rated so we have to deal with that case and fill all missing values
        # with NAs
        while(length(rating_home) < 11){
          rating_home <- c(rating_home, NA)
        }
        
        # iterate over the rating away
        while(length(rating_away) < 11){
          rating_away <- c(rating_away, NA)
        }
        
        # bind the rating together
        rating <- c(rating_home, rating_away)
        
        # extract the nodes for the position and market value
        pos_and_market_value <- page_html %>%
          html_nodes(xpath = paste0("//div[@class='row sb-formation'][1]",
                                    "//table[@class='inline-table']//tr[2]/td")) %>%
          html_text() %>%
          str_split(pattern = ",") 
        
        # get the position
        pos <- pos_and_market_value %>%
          sapply(magrittr::extract2, 1) %>%
          trimws()
        
        # get the market values 
        market_value <- pos_and_market_value %>%
          unlist() %>%
          # and again take only the numbers with the
          # m (millions) or k (thousand)
          str_extract(., pattern = "[0-9]+.*| -") %>%
          # drop NAs
          .[!is.na(.)]
        
        # iterate over the market values and transform the "-" to NAs
        for(value in 1:length(market_value)){
          market_value <- ifelse(market_value == " -",
                                 NA, market_value)
        }
        
        # extract the transfer status of the players
        transfer_status <- page_html %>%
          html_nodes(xpath = paste0("//div[@class='row sb-formation'][1]",
                                    "//table[@class='inline-table']")) %>%
          html_nodes(xpath = "..") %>%
          html_attr("class")
        
        # create new variables for the transfers and initialize them
        # with NA (22, 11*2)
        new_transfer <- rep(NA, 22)
        new_winter_transfer <- rep(NA, 22)
        returnee <- rep(NA, 22)
        
        # iterate over all players and check if one of these variables
        # can be set to TRUE
        for(k in 1:length(transfer_status)){
          # check if it is a new transfer
          new_transfer[k] <- ifelse(transfer_status[k] == "neuzugang",
                                    TRUE, FALSE)
          # check if it is a new winter transfer
          new_winter_transfer[k] <- ifelse(transfer_status[k] == "winter_neuzugang",
                                           TRUE, FALSE)
          # check if it is a returnee
          returnee[k] <- ifelse(transfer_status[k] == "rueckkehrer",
                                TRUE, FALSE)
        }
        
        # putting together the data frame for the starting grid
        starting_lineups <- c(name, player_number, pos, age, nationality, market_value,
                              rating, new_transfer, new_winter_transfer, returnee) %>%
          # create a data frame with 10 columns
          matrix(ncol = 10, byrow = FALSE) %>%
          data.frame() %>%
          # create groups to properly match the team names to the players
          mutate(match_group = rep(row_number(), length.out = n(), each = 11),
                 team = ifelse(match_group == 1,
                               team_names[1],
                               team_names[2]),
                 # convert the market value into an apropriate format by
                 # removing all additional signs and converts it into numeric
                 player_market_value_in_million_euro = ifelse(endsWith(X6, "m"),
                                                              as.numeric(str_remove(X6,
                                                                                    pattern = "m")),
                                                              as.numeric(str_remove(X6, 
                                                                                    pattern = "Th."))/1000),
                 # also converts some other variables into the proper format
                 # or add new variables
                 player_rating_tm = as.numeric(X7),
                 player_number = as.numeric(X2),
                 player_age = as.numeric(X4),
                 matchday = i,
                 match_date = date,
                 match_time = time,
                 match_attendance = attendance,
                 season,
                 "league" = league,
                 referee = ref_info,
                 player_is_new_transfer = as.logical(X8),
                 player_is_new_winter_transfer = as.logical(X9),
                 player_is_returnee = as.logical(X10),
                 player_is_starting_grid = TRUE) %>%
          # reorder the data frame
          select(league, season, matchday, match_date, match_time,
                 match_attendance, referee,
                 team, player_name = X1, player_number, player_pos = X3,
                 player_age, player_nationality = X5, 
                 player_market_value_in_million_euro,
                 player_rating_tm, 
                 player_is_new_transfer,
                 player_is_new_winter_transfer,
                 player_is_returnee,
                 player_is_starting_grid)
        
        
        # create an empty variable to store all information about the substitute
        # players of both teams
        substitute_players <- NULL
        
        # iterate over both teams to extract the substitute players
        for(team_subs in 1:length(team_names)){
          # get the data for the substitution players
          # extract the nodes for the name and ages
          name_and_age_nodes <- page_html %>%
            html_nodes(xpath = paste0("//div[@class='row sb-formation'][2]",
                                      "//div[@class='large-6 columns'][", team_subs, "]",
                                      "//table[@class='items']"))
        
          # extract the name from the element above
          name <- name_and_age_nodes %>%
            html_nodes(xpath = paste0(".//table[@class='inline-table']//tr[1]//td[2]/a")) %>%
            html_text(trim = TRUE)
          
          # extract the age from the element above
          age <- name_and_age_nodes %>%
            html_nodes(xpath = paste0(".//table[@class='inline-table']//tr[1]//td[2]/text()")) %>%
            html_text(trim = TRUE) %>%
            # get only the numbers, i.e., drop "years"
            str_extract(., pattern = "[0-9]+") %>%
            # remove NAs
            .[!is.na(.)]
          
          # extract the player numbers
          player_number <- page_html %>%
            html_nodes(xpath = paste0("//*[@class='row sb-formation'][2]",
                                      "//div[@class='large-6 columns'][", team_subs, "]",
                                      "//div[@class='rn_nummer']")) %>%
            html_text(trim = TRUE)
          
          # extract the player nationalities
          nationality <- page_html %>%
            html_nodes(xpath = paste0("//div[@class='row sb-formation'][2]",
                                      "//div[@class='large-6 columns'][", team_subs, "]",
                                      "//table[@class='items']/tbody/tr/td[3]/img[1]")) %>%
            html_attr("alt")
          
          # we create a vector for the rating which holds NAs
          rating <- rep(NA, length(name))
          
          # then we iterate over all players
          for(player_row in 1:length(name)){
            # check for the current rating and extract it
            curr_rating <- page_html %>%
              html_nodes(xpath = paste0("//div[@class='row sb-formation'][2]",
                                        "//div[@class='large-6 columns'][", team_subs, "]",
                                        "//table[@class='items']/tbody/tr[", player_row, "]/td[4]")) %>%
              html_text(trim = TRUE)
            
            # if the rating is empty, we set insert NA,
            # otherwise we leave insert the actual rating
            rating[player_row] <- ifelse(length(curr_rating) == 0,
                                         NA,
                                         curr_rating)
          }
          
          # extract the nodes for the player positions and market values
          pos_and_market_value <- page_html %>%
            html_nodes(xpath = paste0("//div[@class='row sb-formation'][2]",
                                      "//div[@class='large-6 columns'][", team_subs, "]",
                                      "//table[@class='inline-table']//tr[2]/td")) %>%
            html_text() %>%
            str_split(pattern = ",") 
          
          # extract the positions of the players
          pos <- pos_and_market_value %>%
            sapply(extract2, 1) %>%
            trimws()
          
          # extract the market values 
          market_value <- pos_and_market_value %>%
            unlist() %>%
            # get only the number and the ending (bn, m, Th.)
            str_extract(., pattern = "[0-9]+.*| -") %>%
            # drop NAs
            .[!is.na(.)]
          
          # iterate over the market values and transform the "-" to NAs
          for(value in 1:length(market_value)){
            market_value <- ifelse(market_value == " -",
                                   NA, market_value)
          }
          
          
          # get the transfer status
          transfer_status <- page_html %>%
            html_nodes(xpath = paste0("//div[@class='row sb-formation'][2]",
                                      "//div[@class='large-6 columns'][", team_subs, "]",
                                      "//table[@class='inline-table']")) %>%
            html_nodes(xpath = "..") %>%
            html_attr("class")
          
          # we create empty vectors and initialize them with NA
          new_transfer <- rep(NA, length(name))
          new_winter_transfer <- rep(NA, length(name))
          returnee <- rep(NA, length(name))
          
          # then we iterate over all substitute players of this team
          for(k in 1:length(transfer_status)){
            # set it to TRUE if it is a new transfer
            new_transfer[k] <- ifelse(transfer_status[k] == "neuzugang",
                                      TRUE, FALSE)
            # set it to TRUE if it is a new winter transfer
            new_winter_transfer[k] <- ifelse(transfer_status[k] == "winter_neuzugang",
                                             TRUE, FALSE)
            # set it to TRUE if it is a returnee
            returnee[k] <- ifelse(transfer_status[k] == "rueckkehrer",
                                  TRUE, FALSE)
          }
          
          # put together all the information scraped
          substitute_lineups <- c(name, player_number, pos, age, nationality, market_value,
                                  rating, new_transfer, new_winter_transfer, returnee) %>%
            # into a data frame of 10 columns
            matrix(ncol = 10, byrow = FALSE) %>%
            data.frame() %>%
            # create a variable for the team name
            mutate(team = ifelse(team_subs == 1,
                                 team_names[1],
                                 team_names[2]),
                   # convert the market value in a proper format 
                   player_market_value_in_million_euro = ifelse(endsWith(X6, "m"),
                                                                as.numeric(str_remove(X6,
                                                                                      pattern = "m")),
                                                                as.numeric(str_remove(X6, 
                                                                                      pattern = "Th."))/1000),
                   # rename and transform the ratings, number and age
                   # and add new variables for the matchday, date or league
                   player_rating_tm = as.numeric(X7),
                   player_number = as.numeric(X2),
                   player_age = as.numeric(X4),
                   matchday = i,
                   match_date = date,
                   match_time = time,
                   match_attendance = attendance,
                   season,
                   "league" = league,
                   referee = ref_info,
                   player_is_new_transfer = as.logical(X8),
                   player_is_new_winter_transfer = as.logical(X9),
                   player_is_returnee = as.logical(X10),
                   player_is_starting_grid = FALSE) %>%
            # reorder the data frame
            select(league, season, matchday, match_date, match_time,
                   match_attendance, referee,
                   team, player_name = X1, player_number, player_pos = X3,
                   player_age, player_nationality = X5, 
                   player_market_value_in_million_euro, 
                   player_rating_tm,
                   player_is_new_transfer,
                   player_is_new_winter_transfer,
                   player_is_returnee,
                   player_is_starting_grid)
          
          # bind together the substitute players of the current team
          # and the overall data frame of substitutes
          substitute_players <- bind_rows(substitute_players,
                                          substitute_lineups)
          
          Sys.sleep(2)
          
        }
        
        # bind together the starting and substitute lineups to the
        # total lineup information frame
        lineup_information <- bind_rows(lineup_information,
                                        starting_lineups,
                                        substitute_players)
        
        # wait for 2 seconds before continuing
        Sys.sleep(2)
        
        # we want to break the repeat loop if we have not just NAs
        # for the player age or the nationality which happens sometimes
        if(sum(is.na(lineup_information$player_age)) != length(lineup_information$player_age) |
           sum(is.na(lineup_information$player_nationality)) != 
           length(lineup_information$player_nationality)){
          break
        }
      }
    }
  }
  
  # close the driver (client) and the server
  remDr$close()
  rD$server$stop()
  rm(rD)
  gc()
  closeAllConnections()
    
  # returns the list containing all lineups for every match
  # on every matchday
  return(lineup_information)
  
}




# function should extract the nations that are currently in the top x in the 
# fifa world ranking list
get_fifa_world_ranking_top_nations <- function(newest_available_date, top_nations = 15){
  # base url
  base_url <- "https://www.transfermarkt.com/statistik/weltrangliste"
  
  # fifa ranking over time variable
  fifa_ranking_over_time <- NULL
  
  # extract the page content once
  page_html <- read_html(base_url)
  
  # extract all available dates (back to the start date)
  available_dates <- page_html %>%
    html_nodes(xpath = "//option[@value]") %>%
    html_attrs()
  
  # clean the dates by extracting the values from the list above
  # (take always the first element in the list (tail(1)), sort them ascending
  # and unname them)
  dates <- rapply(available_dates, function(x) tail(x, 1)) %>%
    sort() %>%
    unname() %>%
    ymd()
  
  # check if the newest available date (newest_available_date) is already in the data base
  # and if so, return NULL
  if(max(dates) <= ymd(newest_available_date)){
    return(NULL)
    # if there is new data to scrape we just want to scrape the data that 
    # is actually newer than the data we have stored in our data base
  } else {
    dates <- dates[dates > ymd(newest_available_date)]
  }
  
  # iterate over all available dates
  for(i in 1:length(dates)){
    # paste together the final url with the dates
    final_url <- paste0(base_url, "/statistik/stat/plus/0/galerie/0?",
                        "datum=", dates[i])
    
    # extract the content of the current page
    page_html <- read_html(final_url)
    
    # extract the information for the current date
    fifa_world_ranking_table <- page_html %>%
      html_nodes(xpath = "//table[@class='items']") %>%
      # convert it directly into a data frame
      html_table() %>%
      # get the first element of the extracted list which contains the data frame
      .[[1]] %>%
      # get only the top_nations (default 15)
      head(top_nations) %>%
      # add a variable for the date
      mutate(date = dates[i]) %>%
      # reorder the variables
      select(date, rank = "#", nation = Nation, points = Points) %>%
      data.frame()
  
    # store the table extracted for the current date in the overall frame
    # that stores all dates
    fifa_ranking_over_time <- bind_rows(fifa_ranking_over_time,
                                        fifa_world_ranking_table)
    
    Sys.sleep(2)
  }
  
  return(fifa_ranking_over_time)
}
