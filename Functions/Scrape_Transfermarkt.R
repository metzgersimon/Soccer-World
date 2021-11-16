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
      player_market_values %>%　data.frame() %>% rename("value" = ".")
    
    # clean the data frame
    player_market_values$variable <-
      rep(
        c("ranking", "player name", "position", "age", "market value"),
        nrow(player_market_values) / 5
      )
    player_market_values <- player_market_values %>% unstack()
    
    player_market_values <-
      player_market_values[, c("ranking",
                               "player.name",
                               "position",
                               "age",
                               "market.value")]
    
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
    player_market_values <-
      player_market_values %>%　rename("club" = ".")
    
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
        "player.name",
        "position",
        "age",
        "appearances",
        "substitution.on",
        "substitution.off",
        "goals",
        "assists",
        "total.points"
      )]
    
    # convert to numeric type
    cols.num <-
      c(
        "ranking",
        "age",
        "appearances",
        "substitution.on",
        "substitution.off",
        "goals",
        "assists",
        "total.points"
      )
    player_performance[cols.num] <-
      sapply(player_performance[cols.num], as.numeric)
    
    # so far we only have the values
    # but we also want to know which clubs they come from
    # so we extract the information from logos included in the table
    #image_club_names <- read_html(final_url) %>%
    #html_nodes(xpath = "//a[@title]") %>%
    #html_attr("title") %>% as.data.frame()
    
    #image_club_names <- image_club_names[c(3:55),]
    #player_performance <- cbind(player_performance, image_club_names)
    #player_performance <- player_performance %>%　rename("club" = ".")
    
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

