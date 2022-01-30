###################### transfermarkt ########################

############## get_scoring_ratio_per_season #################
# inputs: league, league_id, season
# outputs: should return a data frame for a given season in a certain league
# that contain information about the efficiency of the teams
# example: how many goals a team scored in relation to the shots the team did

get_scoring_ratio_per_season <-
  function(league, league_id, season) {
    # create the url endpoint for the given season
    url <- paste0(
      "https://www.transfermarkt.com/",
      league,
      "/",
      "chancenverwertung/wettbewerb/",
      league_id,
      "/sasion_id/",
      season,
      "/plus/1"
    )
    
    # extract the content of the table into a data frame
    chances_content <- read_html(url) %>%
      # we only want to extract the body of the table
      html_nodes(xpath = "//table/tbody") %>%
      # convert it into a data frame
      html_table() %>%
      # drop the first element of the list because we do not need it
      .[-1]
    
    # create the data frame with the second element of the list and drop
    # the first column which is the wappen variable which we do not have
    chances_frame <- as.data.frame(chances_content[[1]]) %>%
      .[,-1] %>%
      # rename the columns
      rename(
        club = X2,
        total_shots = X3,
        total_shots_on_goal = X4,
        goals = X5,
        conversion_rate = X6
      )
    
    # drop the unnecessary information about the table position of the team
    # by extracting only the club name and trim the whitespaces
    chances_frame$club <- lapply(chances_frame$club,
                                 function(x)
                                   (strsplit(x, "[0-9]+th position"))) %>%
      unlist() %>%
      trimws()
    
    # add an additional column for the season the information is for
    chances_frame$season <-
      rep(paste0(season, "/", (season + 1)), nrow(chances_frame))
    
    # return the data frame
    return(chances_frame)
    
  }



############## get_market_values_over_time #################
# inputs: league, league_id
# outputs: should return a data frame for a series of season in a certain league
# that contain information about a teams market value development over time

get_market_values_over_time <- function(league, league_id) {
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
  
  # iterate through all possible cut off dates
  for (i in 1:length(cut_off_dates)) {
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
      # rename all columns
      select(-X1) %>%
      rename(
        club = X3,
        league_then = X4,
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
        squad_size_then = as.integer(squad_size_then),
        current_squad_size = as.integer(current_squad_size)
      )
    
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




############## get_market_values_player #################
# inputs: league, league_id
# outputs: should return a data frame in a certain league
# that contain information about player market value for top 100 players

get_players_market_values <- function(league, league_id) {
  # create empty variable to store market values of players for one page
  player_market_values <- as.data.frame(matrix(nrow = 1, ncol = 5))
  
  # create empty variable to store all market values of players
  players_values <- as.data.frame(matrix(nrow = 0, ncol = 5))
  
  # create base url to extract the market values of players
  url <-
    paste0(
      "https://www.transfermarkt.com/",
      league,
      "/marktwerte/",
      "wettbewerb/",
      league_id,
      "/ajax/yx1/"
    )
  
  page <- 1
  while (page <= 4) {
    url_end <- paste0("page/", page)
    
    # paste the whole url together
    final_url <- paste0(url, url_end)
    
    # extract all available market value for players
    player_market_values <-
      read_html(final_url) %>%
      html_nodes(css = "#yw1 .even :nth-child(4) , #yw1 .even .zentriert:nth-child(1), #yw1 .inline-table tr:nth-child(2) td, #yw1 .inline-table tr:nth-child(2) td, #yw1 .odd .zentriert:nth-child(1), #yw1 .odd :nth-child(4), #yw1 .hauptlink") %>%
      html_text()
    
    # convert it into a data frame
 
    player_market_values <- 
      player_market_values %>%
      data.frame() %>%
      rename("value" = ".")

    player_market_values <- 
      player_market_values %>%
      data.frame() %>%
      rename("value" = ".")
    
    # clean the data frame
    player_market_values$variable <-
      rep(
        c("ranking", "player name", "position", "age", "market value"),
        nrow(player_market_values) / 5
      )
    player_market_values <- player_market_values %>% unstack()
    
    player_market_values <-
      player_market_values[, c("ranking",
                               "player_name",
                               "position",
                               "age",
                               "market_value")]
    
    player_market_values$ranking <-
      as.numeric(player_market_values$ranking)
    player_market_values$age <-
      as.numeric(player_market_values$age)
    
    # so far we only have the values
    # but we also want to know which clubs they come from
    # so we extract the information from logos included in the table
    image_club_names <- read_html(final_url) %>%
      html_nodes(css = "#yw1 a img") %>%
      html_attr("alt") %>% as.data.frame()
    
    player_market_values <-
      cbind(player_market_values, image_club_names)
 
    player_market_values <- player_market_values %>%
      data.frame() %>%
      rename("club" = ".")

    player_market_values <- player_market_values %>%
      data.frame() %>%
      rename("club" = ".")
 
    
    # append the data for the current page to the list which stores
    # all pages
    players_values <- rbind(players_values,
                            player_market_values)
    
    # increase page by 1
    page <- page + 1
    
    # wait for 5 seconds before scraping the next page
    Sys.sleep(5)
  }
  
  # return the full data frame of market values
  return(players_values)
}



############## get_performance_players #################
# inputs: league, league_id
# outputs: should return a data frame for players which contains the
# information about the performance of players in one certain season
# i.e. goals, assists, appearance, substitution on/off

get_performance_players <- function(league, league_id, season) {
  # create empty variable to store performance of players for one page
  player_performance <- as.data.frame(matrix(nrow = 1, ncol = 10))
  
  # create empty variable to store performance of players
  players_season_performance <-
    as.data.frame(matrix(nrow = 0, ncol = 10))
  
  # create base url to extract the market values of players
  url <-
    paste0(
      "https://www.transfermarkt.com/",
      league,
      "/scorerliste/",
      "wettbewerb/",
      league_id,
      "/saison_id/",
      season,
      "/altersklasse/alle/plus/1"
    )
  
  page <- 1
  while (TRUE) {
    #create dynamic url_end with the page variable
    url_end <- paste0("/page/", page)
    
    # paste the whole url together
    final_url <- paste0(url, url_end)
    
    # extract all available market value for players
    player_performance <-
      read_html(final_url) %>%
      html_nodes(css = ".even .zentriert:nth-child(1) , .even :nth-child(10), .even :nth-child(9), .even :nth-child(8), .even :nth-child(7), .even :nth-child(6), .even :nth-child(5), .hauptlink, .odd :nth-child(10), .odd :nth-child(9), .odd :nth-child(8), .odd :nth-child(7), .odd :nth-child(6), .odd :nth-child(5), .inline-table tr:nth-child(2) td, .hauptlink a, .odd .zentriert:nth-child(1)") %>%
      html_text() %>% data.frame()
    
    # clean the data frame
    player_performance$variable <-
      rep(
        c(
          "ranking",
          "player name1",
          "player name",
          "position",
          "age",
          "appearances",
          "substitution on",
          "substitution off",
          "goals",
          "assists",
          "total points"
        ),
        nrow(player_performance) / 11
      )
    player_performance <- player_performance %>% unstack()
    player_performance <- player_performance[, -6]
    
    player_performance <-
      player_performance[, c(
        "ranking",
        "player_name",
        "position",
        "age",
        "appearances",
        "substitution_on",
        "substitution_off",
        "goals",
        "assists",
        "total_points"
      )]
    
    # convert to numeric type
    cols.num <-
      c(
        "ranking",
        "age",
        "appearances",
        "substitution_on",
        "substitution_off",
        "goals",
        "assists",
        "total_points"
      )
    player_performance[cols.num] <-
      sapply(player_performance[cols.num], as.numeric)
    
    
    # append the data for the current page to the data frame which stores
    # all pages
    players_season_performance <-
      rbind(players_season_performance,
            player_performance)
    
    # if last page, then stop
    if (nrow(player_performance) < 25) {
      break
    }
    
    # increase page by 1
    page <- page + 1
    
    # wait for 5 seconds before scraping the next page
    Sys.sleep(5)
    
  }
  
  # return the full data frame of player performance over time
  return(players_season_performance)
}





# function should return a data frame for a running league table over the season
# where the league and the season are given 
get_running_table <- function(league, league_id, season){
  # create empty variable to store the league table over time
  table_over_time <- NULL
  
  # create base url to extract the market values
  url <- paste0("https://www.transfermarkt.com/", league, "/spieltagtabelle/",
                "wettbewerb/", league_id, "?saison_id=", season)
  
  # extract the number of matchdays dynamically
  number_matchdays <- read_html(url) %>%
    html_nodes(xpath = "//select[@name='spieltag']/option[@value]") %>%
    html_attrs() %>%
    length()
  
  # extract the actual content in a loop
  for(i in 1:number_matchdays){
    # create the final url with the current matchday 
    final_url <- paste0(url, "&spieltag=", i)
    
    # extract the matchday table and clean it
    matchday_table <- read_html(final_url) %>%
      html_nodes(xpath = "//table") %>%
      # creates a list of data frames
      html_table() %>%
      # select the last data frame which contains the wanted table
      .[[5]] %>%
      # drop the second column
      .[, -2] %>%
      # separate the column Goals (e.g., 8:2, i.e., the team scored 8 goals and
      # got 2 goals against it) into a column goals_for and goals_against
      # by the separator :
      separate(col = Goals, into = c("goals_for", "goals_against"),
               sep = ":")
    
    # to be able to mutate the newly created columns into numeric variables
    # we have to change the colnames because the matchday column has now name
    # up to this point
    colnames(matchday_table) <- c("rank", "club", "matchday", "wins",
                                  "draws", "losses", "goals_for", "goals_against",
                                  "goal_diff", "points")
    
    # transform the goals columns into numeric variables
    matchday_table <- matchday_table %>%
      mutate(goals_for = as.numeric(goals_for),
             goals_against = as.numeric(goals_against),
             # add season and league information
             season_start_year = season,
             season_end_year = (season + 1),
             league = str_to_title(league)) 
    
    # add cumulative points
    cum_points <- matchday_table %>%
      # group by club 
      group_by(club) %>% 
      # add up all points for a given club to a given matchday
      summarize(cum_points = sum(points))
    
    # join the two frames by clubname
    matchday_table <- inner_join(matchday_table, cum_points,
                                 by = "club")
      
    # append the data frame of the current matchday to the data frame
    # of all matchdays
    table_over_time <- bind_rows(table_over_time,
                                 matchday_table)
      
    # wait for 2 seconds before scraping the next page
    Sys.sleep(2)
    
  }
  
  # return the table which contains all matchdays
  return(table_over_time)
  
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
  
  # iterate through all club urls
  for(i in 1:length(club_names_url)){
    # paste the final url together
    final_url <- paste0("https://www.transfermarkt.com", club_names_url[i],
                        "/plus/1")
    
    # store the page once to do not have to scrape it all the time
    page_html <- read_html(final_url)
    
    # print the current iteration for debugging
    print(i)
    
    # extract the current club name from the url
    # curr_club <- club_names_url[i] %>%
    #   str_extract(., pattern = "/.*/kader") %>%
    #   str_remove(., pattern = "kader") %>%
    #   str_remove_all(., pattern = "\\/") %>%
    #   str_replace_all(., pattern = "-", replacement = " ") %>%
    #   str_to_title()
    
    # if(season == max_season){
    #    
    # }
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
    
    # extract the name and additional information
    # such as birth date or height
    # and again also the name of the player to be able to join
    # using it as key
    position <- page_html %>%
      html_nodes(xpath = paste0("//table[@class='inline-table']//tr[2]/td")) %>%
      html_text(trim = TRUE)
    
    player_number <- page_html %>%
      html_nodes(xpath = paste0("//table[@class='items']//div[@class='rn_nummer']")) %>%
      html_text(trim = TRUE)
    
    player_information <- page_html %>%
      html_nodes(xpath = "//table[@class='items']//tr") 
    
    birth_date <- player_information %>% 
      html_nodes(xpath = paste0(".//td[3]")) %>%
      html_text(trim = TRUE)
    
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
    
    # joined_date <- page_html %>%
    #   html_nodes(xpath = "//div[@class='responsive-table']//table[@class='items']//td[8]") %>%
    #   html_text()
    
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
      # transform various columns
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
      mutate(player_market_value_in_million_euro = ifelse(endsWith(player_market_value, "m"),
                                                          as.numeric(str_remove(player_market_value, 
                                                                                pattern = "m")),
                                                          as.numeric(str_remove(player_market_value, 
                                                                                pattern = "Th."))/1000),
             club = curr_club,
             league_name = str_to_title(league),
             season_year = season) %>%
      select(league = league_name, season = season_year,
             club, player_name, player_number, player_position,
             player_nationality, player_birth_date, player_age,
             player_height, player_foot, player_contract_date,
             player_previous_club, player_joining_date,
             player_market_value_in_million_euro)
    
    curr_squad_information$club <- sapply(curr_squad_information$club,
                                          club_name_mapping) %>%
      unname()
    
    
    # bind all together into the earlier created variable
    # by bind_rows (more flexible than rbind)
    all_squads_in_season <- bind_rows(all_squads_in_season,
                                      curr_squad_information)
    
    closeAllConnections()
    gc()
    
    Sys.sleep(5)
  }
  
  # return all squads for this season
  return(all_squads_in_season)
  
}



############## get_lineups_by_season_tm #################
# inputs: league, league_id, season
# outputs: returns all lineups for ended fixtures in a given season

get_lineups_by_season_tm <- function(league, league_id, season, port = NULL,
                                     matchday = NULL){
  # paste together the url we want to scrape
  url <- paste0("https://www.transfermarkt.com/", league, "/spieltagtabelle",
                      "/wettbewerb/", league_id, "?saison_id=",
                      season)
  
  # create an empty list to store our information
  lineup_information <- NULL
  
  # create a flag variable to check whether the match is in the future
  is_future_match <- FALSE
  
  # create a driver from Rselenium
  rD <- rsDriver(browser = "firefox", port = port)
  
  # get the client
  remDr <- rD$client
  
  # set time outs to give the page the change to first fully load before
  # we try to get information form it
  remDr$setTimeout(type = "implicit", milliseconds = 50000)
  remDr$setTimeout(type = "script", milliseconds = 100000)
  # remDr$setTimeout(type = "page load", milliseconds = 30000)
  
  # navigate to the created url
  remDr$navigate(url)
  
  Sys.sleep(2)
  
  # switch to the pop-up cookies frame
  remDr$switchToFrame(remDr$findElement(using = "xpath",
                                        "//iframe[@title='SP Consent Message']"))
  
  Sys.sleep(2)
  
  # access the "ACCEPT ALL" button
  cookie_elem <- remDr$findElement(using = "xpath", 
                                   "//button[@title='ACCEPT ALL']")
  
  Sys.sleep(2)
  
  # click it
  cookie_elem$clickElement()
  
  # after that we have to switch back to the default frame
  remDr$switchToFrame(NA)
  
  # extract the html for the given url
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  if(is.null(matchday)){
    # extract the number of matchdays by getting the length
    # of the elements
    number_matchdays <- page_html %>%
      html_nodes(xpath = "//select[@name='spieltag']/option[@value]") %>%
      html_attrs() %>%
      length()

    starting_matchday <- 1
    ending_matchday <- number_matchdays
    
  } else {
    starting_matchday <- matchday
    ending_matchday <- matchday
  }
  
  # iterate through all matchdays
  for(i in starting_matchday:ending_matchday){
    
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
  
    if(length(matchday_refs) == 0){
      return(lineup_information)
    }
    
    
    Sys.sleep(1)
    
    # iterate through all matches
    for(j in 1:length(matchday_refs)){
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
      
      Sys.sleep(3)
 
      # find the button which leads to the lineup page     
      elem <- remDr$findElement(using = "xpath", "//ul[@id='submenu']//li[@id='line-ups']/a")
      
      Sys.sleep(5)
      
      elem$sendKeysToElement(list("line-ups", key = "enter"))
      
      
      Sys.sleep(5)
    
      # extract the html for the given url
      page_html <- read_html(remDr$getPageSource()[[1]])
      
      # print(remDr$getCurrentUrl())
      
      Sys.sleep(2)
      
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
      
      date <- date_information[[1]][1] %>%
        mdy()
      
      print(paste0("Date: ", date))
      
      if(date >= Sys.Date()){
        is_future_match <- TRUE
        break
      }
      
      time_12h <- paste(date_information[[1]][3], date_information[[1]][4], collapse = "")
      print(time_12h)
      time <- format(strptime(time_12h, "%I:%M %p"), "%H:%M")
     
      Sys.sleep(2)
      
      # extract the html for the given url
      page_html <- read_html(remDr$getPageSource()[[1]])
      
      Sys.sleep(3)
      
      # get the data for the starting grid
      
      name_and_age_nodes <- page_html %>%
        html_nodes(xpath = "//div[@class='row sb-formation'][1]//table[@class='items']")
        # extract the nodes for the player name, position and number
        
      name <- name_and_age_nodes %>%
        html_nodes(xpath = paste0(".//table[@class='inline-table']//tr[1]//td[2]/a")) %>%
        html_text(trim = TRUE)
      
      player_number <- page_html %>%
        html_nodes(xpath = paste0("//*[@class='row sb-formation'][1]//",
                                  "div[@class='rn_nummer']")) %>%
        html_text(trim = TRUE)
      
      age <- name_and_age_nodes %>%
        html_nodes(xpath = paste0(".//table[@class='inline-table']//tr[1]//td[2]/text()")) %>%
        html_text(trim = TRUE) %>%
        str_extract(., pattern = "[0-9]+") %>%
        .[!is.na(.)]
      
      nationality <- page_html %>%
        html_nodes(xpath = paste0("//div[@class='row sb-formation'][1]", "
                                  //table[@class='items']/tbody/tr/td[3]/img[1]")) %>%
        html_attr("alt")
      
      rating <- page_html %>%
        html_nodes(xpath = paste0("//div[@class='row sb-formation'][1]",
                                  "//div[@class='large-6 columns']",
                                  "//table[@class='items']/tbody/tr/td[4]/span")) %>%
        html_text(trim = TRUE)
      
      pos_and_market_value <- page_html %>%
        html_nodes(xpath = paste0("//div[@class='row sb-formation'][1]",
                                  "//table[@class='inline-table']//tr[2]/td")) %>%
        html_text() %>%
        str_split(pattern = ",") 
      
      pos <- pos_and_market_value %>%
        sapply(extract2, 1) %>%
        trimws()
      
      market_value <- pos_and_market_value %>%
        unlist() %>%
        str_extract(., pattern = "[0-9]+.*") %>%
        .[!is.na(.)]
        # sapply(extract2, 2) %>%
        # str_remove()
      
      transfer_status <- page_html %>%
        html_nodes(xpath = paste0("//div[@class='row sb-formation'][1]",
                                  "//table[@class='inline-table']")) %>%
        html_nodes(xpath = "..") %>%
        html_attr("class")
      
      new_transfer <- rep(NA, 22)
      new_winter_transfer <- rep(NA, 22)
      returnee <- rep(NA, 22)
      
      
      for(k in 1:length(transfer_status)){
        new_transfer[k] <- ifelse(transfer_status[k] == "neuzugang",
                                  TRUE, FALSE)
        
        new_winter_transfer[k] <- ifelse(transfer_status[k] == "winter_neuzugang",
                                         TRUE, FALSE)
        
        returnee[k] <- ifelse(transfer_status[k] == "rueckkehrer",
                              TRUE, FALSE)
      }


      starting_lineups <- c(name, player_number, pos, age, nationality, market_value,
                            rating, new_transfer, new_winter_transfer, returnee) %>%
        matrix(ncol = 10, byrow = FALSE) %>%
        data.frame() %>%
        mutate(match_group = rep(row_number(), length.out = n(), each = 11),
               team = ifelse(match_group == 1,
                             team_names[1],
                             team_names[2]),
               player_market_value_in_million_euro = ifelse(endsWith(X6, "m"),
                                                            as.numeric(str_remove(X6,
                                                                                  pattern = "m")),
                                                            as.numeric(str_remove(X6, 
                                                                                  pattern = "Th."))/1000),
               player_rating_tm = as.numeric(X7),
               player_number = as.numeric(X2),
               player_age = as.numeric(X4),
               matchday = i,
               match_date = date,
               match_time = time,
               season,
               league = str_to_title(league),
               player_is_new_transfer = as.logical(X8),
               player_is_new_winter_transfer = as.logical(X9),
               player_is_returnee = as.logical(X10),
               player_is_starting_grid = TRUE) %>%
        select(league, season, matchday, match_date, match_time,
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
      
      for(team_subs in 1:length(team_names)){
        # get the data for the substitution players
        name_and_age_nodes <- page_html %>%
          html_nodes(xpath = paste0("//div[@class='row sb-formation'][2]",
                                    "//div[@class='large-6 columns'][", team_subs, "]",
                                    "//table[@class='items']"))
        # extract the nodes for the player name, position and number
        
        name <- name_and_age_nodes %>%
          html_nodes(xpath = paste0(".//table[@class='inline-table']//tr[1]//td[2]/a")) %>%
          html_text(trim = TRUE)
        
        player_number <- page_html %>%
          html_nodes(xpath = paste0("//*[@class='row sb-formation'][2]",
                                    "//div[@class='large-6 columns'][", team_subs, "]",
                                    "//div[@class='rn_nummer']")) %>%
          html_text(trim = TRUE)
        
        age <- name_and_age_nodes %>%
          html_nodes(xpath = paste0(".//table[@class='inline-table']//tr[1]//td[2]/text()")) %>%
          html_text(trim = TRUE) %>%
          str_extract(., pattern = "[0-9]+") %>%
          .[!is.na(.)]
        
        nationality <- page_html %>%
          html_nodes(xpath = paste0("//div[@class='row sb-formation'][2]",
                                    "//div[@class='large-6 columns'][", team_subs, "]",
                                    "//table[@class='items']/tbody/tr/td[3]/img[1]")) %>%
          html_attr("alt")
        
        rating <- rep(NA, length(name))
        
        for(player_row in 1:length(name)){
          curr_rating <- page_html %>%
            html_nodes(xpath = paste0("//div[@class='row sb-formation'][2]",
                                      "//div[@class='large-6 columns'][", team_subs, "]",
                                      "//table[@class='items']/tbody/tr[", player_row, "]/td[4]")) %>%
            html_text(trim = TRUE)
          
          rating[player_row] <- ifelse(length(curr_rating) == 0,
                                       NA,
                                       curr_rating)
        }
        
        
        pos_and_market_value <- page_html %>%
          html_nodes(xpath = paste0("//div[@class='row sb-formation'][2]",
                                    "//div[@class='large-6 columns'][", team_subs, "]",
                                    "//table[@class='inline-table']//tr[2]/td")) %>%
          html_text() %>%
          str_split(pattern = ",") 
        
        pos <- pos_and_market_value %>%
          sapply(extract2, 1) %>%
          trimws()
        
        market_value <- pos_and_market_value %>%
          unlist() %>%
          str_extract(., pattern = "[0-9]+.*") %>%
          .[!is.na(.)]
        
        transfer_status <- page_html %>%
          html_nodes(xpath = paste0("//div[@class='row sb-formation'][2]",
                                    "//div[@class='large-6 columns'][", team_subs, "]",
                                    "//table[@class='inline-table']")) %>%
          html_nodes(xpath = "..") %>%
          html_attr("class")
        
        new_transfer <- rep(NA, length(name))
        new_winter_transfer <- rep(NA, length(name))
        returnee <- rep(NA, length(name))
        
        
        for(k in 1:length(transfer_status)){
          new_transfer[k] <- ifelse(transfer_status[k] == "neuzugang",
                                    TRUE, FALSE)
          
          new_winter_transfer[k] <- ifelse(transfer_status[k] == "winter_neuzugang",
                                           TRUE, FALSE)
          
          returnee[k] <- ifelse(transfer_status[k] == "rueckkehrer",
                                TRUE, FALSE)
        }
        
        substitute_lineups <- c(name, player_number, pos, age, nationality, market_value,
                                rating, new_transfer, new_winter_transfer, returnee) %>%
          matrix(ncol = 10, byrow = FALSE) %>%
          data.frame() %>%
          mutate(team = ifelse(team_subs == 1,
                               team_names[1],
                               team_names[2]),
                 player_market_value_in_million_euro = ifelse(endsWith(X6, "m"),
                                                              as.numeric(str_remove(X6,
                                                                                    pattern = "m")),
                                                              as.numeric(str_remove(X6, 
                                                                                    pattern = "Th."))/1000),
                 player_rating_tm = as.numeric(X7),
                 player_number = as.numeric(X2),
                 player_age = as.numeric(X4),
                 matchday = i,
                 match_date = date,
                 match_time = time,
                 season,
                 league = str_to_title(league),
                 player_is_new_transfer = as.logical(X8),
                 player_is_new_winter_transfer = as.logical(X9),
                 player_is_returnee = as.logical(10),
                 player_is_starting_grid = FALSE) %>%
          select(league, season, matchday, match_date, match_time,
                 team, player_name = X1, player_number, player_pos = X3,
                 player_age, player_nationality = X5, 
                 player_market_value_in_million_euro, 
                 player_rating_tm,
                 player_is_new_transfer,
                 player_is_new_winter_transfer,
                 player_is_returnee,
                 player_is_starting_grid)
        
        substitute_players <- bind_rows(substitute_players,
                                        substitute_lineups)
        
        Sys.sleep(2)
        
      }

      lineup_information <- bind_rows(lineup_information,
                                      starting_lineups,
                                      substitute_players)
      
      # wait for 2 seconds before continuing
      Sys.sleep(2)
      
    }
  }
  
  # close the driver (client) and the server
  remDr$close()
  rD$server$stop()
  rm(rD)
  gc()
    
  # returns the list containing all lineups for every match
  # on every matchday
  return(lineup_information)
  
}


get_fixture_detailed_info <- function(league, league_id, season, port = 1234L,
                                      matchday = NULL, max_matchday = NULL){
  # paste together the url we want to scrape
  url <- paste0("https://www.transfermarkt.com/", league, "/spieltagtabelle",
                "/wettbewerb/", league_id, "?saison_id=",
                season)
  
  # create an empty list to store our information
  match_infos_all_season <- NULL
  
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
  
  # switch to the pop-up cookies frame
  remDr$switchToFrame(remDr$findElement(using = "xpath",
                                        "//iframe[@title='SP Consent Message']"))
  
  # access the "ACCEPT ALL" button
  cookie_elem <- remDr$findElement(using = "xpath", 
                                   "//button[@title='ACCEPT ALL']")
  
  # click it
  cookie_elem$clickElement()
  
  # after that we have to switch back to the default frame
  remDr$switchToFrame(NA)
  
  # extract the html for the given url
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  date <- NULL
  
  if(is.null(matchday)){
    # extract the number of matchdays by getting the length
    # of the elements
    number_matchdays <- page_html %>%
      html_nodes(xpath = "//select[@name='spieltag']/option[@value]") %>%
      html_attrs() %>%
      length()
    
    if(is.null(max_matchday)){
      starting_matchday <- 1
      ending_matchday <- number_matchdays
    } else {
      starting_matchday <- 1
      ending_matchday <- max_matchday
    }
    
  } else {
    starting_matchday <- matchday
    ending_matchday <- matchday
  }
  
  
  # iterate through all matchdays
  for(i in starting_matchday:ending_matchday){
    # print for debugging
    print(paste0("Matchday ", i))
    
    # create an empty variable
    current_matchday_stats <- NULL
    
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
    
    Sys.sleep(3)
    
    # iterate through all matches
    for(j in 1:length(matchday_refs)){
      # print for debugging
      print(paste0("Match_number ", j))
      
      # paste the url we want to scrape data from
      match_url <- paste0("https://www.transfermarkt.com",
                          matchday_refs[j])
      
      # navigate to the url of the specific match
      remDr$navigate(match_url)
      
      # store the webpage
      page_html <- read_html(remDr$getPageSource()[[1]])
      
      Sys.sleep(3)
      
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
      
      # extract the date and convert it into a proper format
      date <- date_information[[1]][1] %>%
        mdy()
      
      # if the date is in the future break
      if(date >= Sys.Date()){
        break
      }
      
      # get the time the match takes place
      time_12h <- paste(date_information[[1]][3], date_information[[1]][4], collapse = "")
      # convert it into a 24h format
      time <- format(strptime(time_12h, "%I:%M %p"), "%H:%M")
      
      # matchday information
      matchday <- page_html %>%
        # extract nodes
        html_nodes(xpath = paste0("//div[@class='sb-spieldaten']",
                                  "/p[@class='sb-datum hide-for-small']/a[1]")) %>%
        html_text(trim = TRUE) %>%
        # get the the actual number and convert it into numeric
        str_extract(., pattern = "[0-9]+") %>%
        as.numeric()
      
      # venue information
      venue <- page_html %>%
        # extract nodes
        html_nodes(xpath = paste0("//div[@class='sb-spieldaten']",
                                  "/p[@class='sb-zusatzinfos']/span[@class='hide-for-small']/a")) %>%
        html_text(trim = TRUE)
      
      # referee information
      referee <- page_html %>%
        # extract nodes
        html_nodes(xpath = paste0("//div[@class='sb-spieldaten']",
                                  "/p[@class='sb-zusatzinfos']/a")) %>%
        html_text(trim = TRUE)
      
      # attendance information
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
      
      
      # store the data in a temporary data frame
      current_data <- data.frame("league" = str_to_title(league),
                                 "season" = season,
                                 "matchday" = matchday,
                                 "match_date" = date,
                                 "match_time" = time,
                                 "home_team" = team_names[1],
                                 "away_team" = team_names[2],
                                 "venue" = venue,
                                 "referee" = referee,
                                 "attendance" = attendance)

      # add the temporary data frame to the data frame that store the data
      # of the whole season
      match_infos_all_season <- bind_rows(match_infos_all_season,
                                          current_data)
      
      Sys.sleep(2)
      
    }
    
    # if the date is in the future break
    if(date >= Sys.Date()){
      break
    }
  }
  
  # transform the season and attendance variable into a numeric variable
  # for the attendance variable we first need to remove the dot
  match_infos_all_season <- match_infos_all_season %>%
    mutate(season = as.numeric(season),
           attendance = as.numeric(str_remove(attendance, pattern = "\\.")))
      
  
  # close the driver (client) and the server
  remDr$close()
  rD$server$stop()
  rm(rD)
  gc()
      
  return(match_infos_all_season)
      
}