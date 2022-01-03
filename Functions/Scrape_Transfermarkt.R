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



############## get_ball_possession_per_season #################
# inputs: league, league_id, season
# outputs: function should return a data frame for a given season in a certain league
# that contain information about the ball possession of the teams
# example: the average possessions at home, away, in total and against which opponents
# the teams have the highest/lowest possession value

get_ball_possession_per_season <-
  function(league, league_id, season) {
    # create the url endpoint for the given season and league
    url <-
      paste0(
        "https://www.transfermarkt.com/",
        league,
        "/ballbesitz/wettbewerb/",
        league_id,
        "/saison_id/",
        season,
        "/plus/1"
      )
    
    # get the html of the url
    page_html <- read_html(url)
    
    # create the data frame
    possession_frame <- page_html %>%
      # extract only the body of the table
      html_nodes(xpath = "//table/tbody") %>%
      # convert the extracted nodes into a table
      html_table() %>%
      # convert it into a data frame
      data.frame() %>%
      # remove those columns which are empty (contain only NAs)
      remove_empty("cols") %>%
      # rename all columns
      rename(
        club = X2,
        average_bp_home = X3,
        average_bp_away = X4,
        highest_bp_value = X5,
        result_in_highest_bp_match = X7,
        lowest_bp_value = X9,
        result_in_lowest_bp_match = X11,
        average_bp_total = X13
      )
    
    # add an additional column for the season the information
    possession_frame$season <-
      rep(paste0(season, "/", (season + 1)), nrow(possession_frame))
    
    # so far we only have the values for those matches where the teams have their
    # highest/lowest scores. But we also want to know which opponent that was
    # so we extract the information from logos included in the table
    images_club_names <- page_html %>%
      html_nodes(css = ".tiny_wappen") %>%
      html_attr("alt")
    
    # convert the text extracted above into a data frame with 5 columns
    # by row, i.e., column by column and a next row after 5 columns
    club_names_frame <-
      as.data.frame(matrix(images_club_names, ncol = 5, byrow = TRUE)) %>%
      rename(
        club = V1,
        highest_bp_home_team = V2,
        highest_bp_away_team = V3,
        lowest_bp_home_team = V4,
        lowest_bp_away_team = V5
      ) %>%
      # add a new variable to track if the highest possession value was at home
      # and the lowest was away
      mutate(
        highest_bp_is_home = ifelse(highest_bp_home_team == club, TRUE, FALSE),
        lowest_bp_is_away = ifelse(lowest_bp_away_team == club, TRUE, FALSE)
      )
    
    # join these two frames into one frame by the club columns
    possession_frame <-
      inner_join(possession_frame, club_names_frame,
                 by = "club")
    
    # return the completed data frame
    return(possession_frame)
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
    curr_club <- club_names_url[i] %>%
      str_extract(., pattern = "/.*/kader") %>%
      str_remove(., pattern = "kader") %>%
      str_remove_all(., pattern = "\\/") %>%
      str_replace_all(., pattern = "-", replacement = " ") %>%
      str_to_title()
                    
    # extract the name and nationality of the players playing
    # in the current club
    name_and_nationality <- page_html %>%
      html_nodes(xpath = paste0("//td[@class='hauptlink']/div[1]/span/a", " | ",
                                "//table[@class='items']/tbody/tr/td[4]/img[1]")) %>%
      html_attr("title")
    
    # convert the name and nationality in a data frame
    # with 2 columns
    name_and_nationality <- as.data.frame(
      matrix(name_and_nationality, 
             ncol = 2,
             byrow = TRUE)
      )
    
    # extract the positions of the players and store it
    # into a data frame
    position <- page_html %>%
      html_nodes(css = ".inline-table tr:nth-child(2) td") %>%
      html_text() %>%
      data.frame()
    
    # bind the name, nationality and position into
    # one data frame
    name_nation_pos <- bind_cols(name_and_nationality,
                                 position)
    
    
    # extract the name and additional information
    # such as birth date or height
    # and again also the name of the player to be able to join
    # using it as key
    name_and_other_information <- page_html %>%
      html_nodes(xpath = paste0("//td[@class='hauptlink']/div[1]/span[@class='hide-for-small']/a/text()|",
                                "//*[@id='yw1']/table/tbody/tr/td[3]|",
                                "//*[@id='yw1']/table/tbody/tr/td[5]|",
                                "//*[@id='yw1']/table/tbody/tr/td[6]|",
                                "//*[@id='yw1']/table/tbody/tr/td[7]|",
                                "//*[@id='yw1']/table/tbody/tr/td[8]|",
                                "//*[@id='yw1']/table/tbody/tr/td[9]|",
                                "//*[@id='yw1']/table/tbody/tr/td[10]|",
                                "//*[@id='yw1']/table/tbody/tr/td[11]/text()")) %>%
      
      html_text()
    
    # convert it from a vector into a data frame
    # with 6 columns
    name_and_other_information <- 
      as.data.frame(matrix(name_and_other_information, 
                           ncol = 8, 
                           byrow = TRUE)) %>%
      select(-6)
    
    
    # join the name_nation_pos and the scraped
    # name and other information together by player name
    # (here given by the column V1)
    full_squad_frame <- inner_join(name_nation_pos,
                                   name_and_other_information,
                                   by = "V1") 
    
    #### cleaning the frame ####
    full_squad_frame_clean <- full_squad_frame %>%
      # renaming all columns
      rename(., 
             player_name = V1,
             country = `V2.x`,
             position = `.`,
             birth_date_age = `V2.y`,
             height = V3,
             foot = V4,
             joining_date = V5,
             contract_date = V7,
             market_value = V8) %>%
      # separate the birth_date_age column
      # where the age is in the birth_date column in parantheses
      # into two separate columns birth_date and age
      separate(col = birth_date_age,
               into = c("birth_date", "age"),
               sep = " \\(|\\)") %>%
      # transform various columns
      mutate(birth_date = mdy(birth_date),
             age = as.integer(age),
             height = as.numeric(str_replace(str_remove(height, pattern = " m"), 
                                             pattern = ",",
                                             replacement = ".")),
             # dates in appropriate format
             joining_date = mdy(joining_date),
             contract_date = mdy(contract_date),
             # remove euro sign from market value
             market_value = str_trim(str_remove(market_value, pattern = "\u20ac"))) 
    
    # final cleaning
    full_squad_frame_clean <- full_squad_frame_clean %>%
      # market value to market value in million euro by removing m (millions)
      # or Th. (thousands)
      mutate(market_value_in_million_euro = ifelse(endsWith(market_value, "m"),
                                                   as.numeric(str_remove(market_value, 
                                                                         pattern = "m")),
                                                   as.numeric(str_remove(market_value, 
                                                                         pattern = "Th."))/1000),
             club_name = curr_club,
             season = season) %>%
      # drop previous market value
      select(-market_value)
    
    # bind all together into the earlier created variable
    # by bind_rows (more flexible than rbind)
    all_squads_in_season <- bind_rows(all_squads_in_season,
                                      full_squad_frame_clean)
  }
  
  # return all squads for this season
  return(all_squads_in_season)
  
}



############## get_lineups_all_ended_fixture_by_season #################
# inputs: league, league_id, season
# outputs: returns all lineups for ended fixtures in a given season

get_lineups_all_ended_fixture_by_season <- function(league, league_id, season,
                                                    port = NULL){
  # paste together the url we want to scrape
  url <- paste0("https://www.transfermarkt.com/", league, "/spieltagtabelle",
                      "/wettbewerb/", league_id, "?saison_id=",
                      season)
  
  # create an empty list to store our information
  lineup_information <- list()
  
  # create a driver from Rselenium
  rD <- rsDriver(browser = "firefox", port = port)
  
  # get the client
  remDr <- rD$client
  
  # navigate to the created url
  remDr$navigate(url)
  
  Sys.sleep(5)
  
  # switch to the pop-up cookies frame
  remDr$switchToFrame(remDr$findElement(using = "xpath",
                                        "//iframe[@title='SP Consent Message']"))
  
  Sys.sleep(3)
  
  # access the "ACCEPT ALL" button
  cookie_elem <- remDr$findElement(using = "xpath", 
                                   "//button[@title='ACCEPT ALL']")
  
  Sys.sleep(3)
  
  # click it
  cookie_elem$clickElement()
  
  # after that we have to switch back to the default frame
  remDr$switchToFrame(NA)
  
  # extract the html for the given url
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  # extract the number of matchdays by getting the length
  # of the elements
  number_matchdays <- page_html %>%
    html_nodes(xpath = "//select[@name='spieltag']/option[@value]") %>%
    html_attrs() %>%
    length()
  
  # iterate through all matchdays
  for(i in 1:34){#length(number_matchdays)){
    # print for debugging
    print(paste0("Matchday ", i))
    
    # create an empty list
    current_matchday_info <- list()
    
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
      
      # store the webpage
      page_html <- read_html(remDr$getPageSource()[[1]])
      
      Sys.sleep(1)
      
      # extract the formations for each team
      formations <- page_html %>%
        html_nodes(xpath = paste0("//div[@class='large-7 aufstellung-vereinsseite",
                                  " columns small-12 unterueberschrift ",
                                  "aufstellung-unterueberschrift']")) %>%
        html_text(trim = TRUE)
      
      Sys.sleep(2)
 
      # find the button which leads to the lineup page     
      elem <- remDr$findElement(using = "xpath", "//*[@id='line-ups']/a")
      
      # click the button
      elem$clickElement()
      
      # extract the html for the given url
      page_html <- read_html(remDr$getPageSource()[[1]])
      
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
      
      time_12h <- paste(date_information[[1]][3], date_information[[1]][4], collapse = "")
      time <- format(strptime(time_12h, "%I:%M %p"), "%H:%M")
     
      Sys.sleep(3)
      
      
      # extract starting line ups 
      starting_line_ups <- page_html %>%
        # extract the nodes for the player name, position and number
        html_nodes(xpath = paste0("//*[@class='row sb-formation'][1]//",
                                  "table[@class='items']//a[@class='wichtig']",
                                  "|//*[@class='row sb-formation'][1]",
                                  "//table[@class='inline-table']//tr[2]/td/text()[1]",
                                  "|//*[@class='row sb-formation'][1]//",
                                  "div[@class='rn_nummer']")) %>%
        html_text(trim = TRUE) %>%
        str_remove_all(., pattern = ",.*") %>%
        str_trim() %>%
        # convert the data into a matrix (3 columns)
        # and then into a data frame
        matrix(., ncol = 3,
               byrow = TRUE) %>%
        data.frame()
      
      # get the home lineup (11 players)
      starting_line_ups_home <- starting_line_ups %>%
        .[1:11,]
      
      # get the away lineup (11 players)
      starting_line_ups_away <- starting_line_ups %>%
        .[12:22,]
      
      # bind the home and away lineups together
      starting_line_ups_clean <- bind_cols(starting_line_ups_home,
                                           starting_line_ups_away)
      
      # set the colnames appropriately
      colnames(starting_line_ups_clean) <- c("player_number_home",
                                             "player_name_home",
                                             "player_position_home",
                                             "player_number_away",
                                             "player_name_away",
                                             "player_position_away")
      
      # substitute players 
      substitute_line_ups <- page_html %>%
        # extract the nodes
        html_nodes(xpath = paste0("//*[@class='row sb-formation'][2]//",
                                  "table[@class='items']//a[@class='wichtig']",
                                  "|//*[@class='row sb-formation'][2]",
                                  "//table[@class='inline-table']//tr[2]/td/text()[1]",
                                  "|//*[@class='row sb-formation'][2]//",
                                  "div[@class='rn_nummer']")) %>%
        # convert it into strings
        html_text(trim = TRUE) %>%
        # remove the ","
        str_remove_all(., pattern = ",.*") %>%
        # trim the whitespaces
        str_trim() %>%
        # convert it into a matrix with 3 columns
        # and then into a data frame
        matrix(., ncol = 3, byrow = TRUE) %>%
        data.frame()
      
      # set the colnames of the substitutes accordingly
      colnames(substitute_line_ups) <- c("player_number",
                                         "player_name",
                                         "player_position")
      
      # club_names for perfect dynamic function later
      
      # club_names <- sapply(substitute_line_ups$player_name,
      #                      get_club_by_player_name,
      #                      league = league,
      #                      season_start = season) %>%
      #   unlist() %>%
      #   unname() %>%
      #   unique()
      
      # get the club name for a given player by using
      # the function get_club_by_player_name
      substitute_line_ups$club_name <- sapply(substitute_line_ups$player_name,
                                                get_club_by_player_name,
                                                # league = league,
                                                season_start = season)
      
      # check if for one player there are multiple clubs available, 
      # i.e., the player transfered from one club to the other in one season
      # indices <- lapply(enframe(substitute_line_ups$club_name)$value,
      #                   length) %>%
      #   which.max()
      #   
      # 
      # for(index in indices){
      #   clubs <- unlist(substitute_line_ups[index, ]$club_name)
      #   clubs <- sapply(clubs,
      #                   club_name_mapping) %>%
      #     unname()
      #   
      #   substitute_line_ups[index, ]$club_name <-
      #     clubs[clubs %in% team_names]
      # }
      # 
      # substitute_line_ups$club_name <- sapply(substitute_line_ups$club_name,
      #                                         club_name_mapping) %>%
      #   unlist()
      # 
      
      # create a list of the current match by
      # storing all the information into it
      current_match_info <- list(
        "matchday" = i,
        "formations" = formations,
        "team_names" = team_names,
        "date" = date,
        "time" = time,
        "starting_lineups" = starting_line_ups_clean,
        "substitute_lineups" = substitute_line_ups
      )
      
      # store the data of the current match as a list
      # into a list containing data of all matches
      current_matchday_info <- append(
        current_matchday_info,
        list(current_match_info)
      )
      
      # wait for 2 seconds before continuing
      Sys.sleep(2)
      
    }
    
    # store the data of the whole matchday into 
    # the overall list of all matchdays
    lineup_information <- append(
      lineup_information,
      list(current_matchday_info)
    )
      
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



############## get_fixture_stats_tm #################
# inputs: league, league_id, season
# outputs: returns all stats available for the games played in a given season
# for a given league

get_fixture_stats_tm <- function(league, league_id, season, port = 4321L){
  # paste together the url we want to scrape
  url <- paste0("https://www.transfermarkt.com/", league, "/spieltagtabelle",
                "/wettbewerb/", league_id, "?saison_id=",
                season)
  
  # create an empty list to store our information
  fixture_stats_all_season <- NULL
  
  # create a driver from Rselenium
  rD <- rsDriver(browser = "firefox", port = port)
  
  # get the client
  remDr <- rD$client
  
  # set time outs to give the page the change to first fully load before
  # we try to get information form it
  remDr$setTimeout(type = "implicit", milliseconds = 7000)
  remDr$setTimeout(type = "page load", milliseconds = 7000)
  
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
  
  # extract the number of matchdays by getting the length
  # of the elements
  number_matchdays <- page_html %>%
    html_nodes(xpath = "//select[@name='spieltag']/option[@value]") %>%
    html_attrs() %>%
    length()
  
  # iterate through all matchdays
  for(i in 1:number_matchdays){
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
    
    if(length(matchday_refs) == 0){
      return(lineup_information)
    }
    
    
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
      
      Sys.sleep(2)
      
      # find the button which leads to the statistics page     
      elem <- remDr$findElement(using = "css", "#statistics .megamenu")
      
      # the click did not work most often so we use send keys to simulate an
      # enter-key button
      elem$sendKeysToElement(list("stats", key = "enter"))

      Sys.sleep(1)
      
      # extract the html for the given url
      page_html <- read_html(remDr$getPageSource()[[1]])
      
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
      
      time_12h <- paste(date_information[[1]][3], date_information[[1]][4], collapse = "")
      time <- format(strptime(time_12h, "%I:%M %p"), "%H:%M")
      
      Sys.sleep(2)
      
      # extract the actual statistics
      current_match_stats <- page_html %>%
        # get the xpaths for the different information
        html_nodes(xpath = paste0("//div[@class='large-8 columns']//div[@class='unterueberschrift']",
                                  "|",
                                  "//div[@class='sb-statistik-zahl']")) %>%
        # convert them into strings
        html_text(trim = TRUE) %>%
        # drop the first element
        .[-1] %>%
        # convert it into a data frame with 3 columns
        matrix(ncol = 3, byrow = TRUE) %>%
        data.frame() %>%
        # transform the data frame to convert rows into columns
        t() %>%
        data.frame() %>%
        .[-1, ]
      
      # set new column names
      colnames(current_match_stats) <- c("total_shots", "shots_off_target",
                                         "shots_saved", "corners",
                                         "free_kicks", "fouls", "offsides")
      
      # create new variables for the early extracted team names
      # and date information
      current_match_stats <- current_match_stats %>%
        mutate(team = team_names,
               fixture_date = date,
               fixture_time = time) %>%
        select(fixture_date, fixture_time,
               team, everything())
      
    
      # add the data extracted for the match to the frame
      # containing all stats for all matches on that matchday
      current_matchday_stats <- bind_rows(
        current_matchday_stats,
        current_match_stats
      )
      
    }
    
    # add data from the current matchday to the frame
    # of all matchdays
    fixture_stats_all_season <- bind_rows(
      fixture_stats_all_season,
      current_matchday_stats
    )
    
  }
  
  return(fixture_stats_all_season)
      
}



############## get_regeneration_days #################
# inputs: league_name, season
# outputs: returns all stats available for the games played in a given season
# for a given league
get_regeneration_days <- function(league, league_id, season, port = 4321L){

  # paste together the url we want to scrape
  url <- paste0("https://www.transfermarkt.com/", league, "/startseite/",
                "wettbewerb/", league_id, "/plus/?saison_id=", season)
  
  # create a driver from Rselenium
  rD <- rsDriver(browser = "firefox", port = port)
  
  # get the client
  remDr <- rD$client
  
  # set time outs to give the page the change to first fully load before
  # we try to get information form it
  remDr$setTimeout(type = "implicit", milliseconds = 7000)
  remDr$setTimeout(type = "page load", milliseconds = 7000)
  
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
  
  # extract the html of the page
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  # get all teams of the season in the selected league
  club_refs <- page_html %>%
    html_nodes(xpath = paste0("//table[@class='items']//td[@class='hauptlink ",
                              "no-border-links hide-for-small hide-for-pad']//a")) %>%
    html_attr("href") %>%
    str_remove_all("#") %>%
    str_subset(pattern = ".+")
  
  club_names <- page_html %>%
    html_nodes(xpath = paste0("//table[@class='items']//td[@class='hauptlink ",
                              "no-border-links hide-for-small hide-for-pad']//a")) %>%
    html_text(trim = TRUE) %>%
    str_subset(pattern = ".+")
  
  Sys.sleep(2)
  
  # create an empty data frame to store the data
  all_matches_frame <- NULL
  
  
  for(i in 1:length(club_refs)){
    print(club_refs[i])
    
    match_plan_url <- paste0("https://www.transfermarkt.com", club_refs[i]) %>%
      str_replace("startseite", replacement = "vereinsspielplan") %>%
      paste0(., "/heim_gast//plus/1")
    
    remDr$navigate(match_plan_url)
    
    # extract the html of the page
    page_html <- read_html(remDr$getPageSource()[[1]])
    
    # get the correct club name 
    club_name <- page_html %>%
      html_nodes(xpath = paste0("//div[@id='verein_head']//h1[@itemprop='name']/span")) %>%
      html_text(trim = TRUE)
    
    # get all teams of the season in the selected league
    club_leagues <- page_html %>%
      html_nodes(xpath = paste0("//div[@class='responsive-table']//table",
                                "/tbody/tr/td/img")) %>%
      html_attr("title")
    
    club_infos <- page_html %>%
      html_nodes(xpath = paste0("//div[@class='responsive-table']//table")) %>%
      html_table(convert = TRUE) %>%
      .[[1]] %>%
      .[, !duplicated(colnames(.), fromLast = TRUE)] %>%
      mutate(Attendance = ifelse(!is.numeric(Attendance),
                                 as.numeric(Attendance),
                                 Attendance))
    
    correct_club_names <- page_html %>%
      html_nodes(xpath = paste0("//div[@class='responsive-table']//table",
                                "/tbody/tr/td[7]/a",
                                "|",
                                "//div[@class='responsive-table']//table",
                                "/tbody/tr/td[9]/a")) %>%
      html_attr("title") %>%
      matrix(ncol = 2, byrow = TRUE) %>%
      data.frame()
    
    club_match_infos <- club_infos %>%
      cbind(club_leagues, correct_club_names) %>%
      select(league = club_leagues, matchday = Matchday, date = Date, time = Time,
             home_team = X1, away_team = X2,
             coach = Coach, result = Result) %>%
      filter(home_team == club_name |
               away_team == club_name) %>%
      # remove all characters up to the first white space to remove the weekday
      # and convert the date to a real date to be able to make computations on it
      mutate(date = mdy(sub(".*? ", "", date)),
             # create a variable indicating how many days a team has break in between
             # two matches
             regeneration_days = as.integer(difftime(date, lag(date, 1), units = c("days"))),
             team_name = club_name,
             season_start_year = season,
             time = format(strptime(time, "%I:%M %p"), "%H:%M")) %>%
      filter(date <= Sys.Date()) %>%
      select(season_start_year, team_name, everything())
    
    
    # add the data of the current club to the data frame holding all the data
    all_matches_frame <- bind_rows(
      all_matches_frame,
      club_match_infos
    )
    
    Sys.sleep(2)

  }
    
  return(all_matches_frame)
  
}

