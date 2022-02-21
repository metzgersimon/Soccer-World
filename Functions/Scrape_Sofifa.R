############## get_team_stats_fifa_all_leagues #################
# function should return the current fifa team stats for all leagues
# given by a vector
get_team_stats_fifa_all_leagues <- function(leagues, fifa_version, date){
  # create a variable to store all the team stats for all leagues
  all_leagues_team_stats <- NULL
  
  # iterate over all leagues given as a vector of strings
  for(i in 1:length(leagues)){
    # for every league, get the fifa team stats for the given fifa version
    # and date
    curr_league_stats <- get_team_stats_fifa(fifa_version, league = leagues[i],
                                                  max_date_in_database = date)
    
    # check if the stats are null and if so return NULL
    if(is.null(curr_league_stats)){
      return(NULL)
    }
    
    # append the curr league stats to the overall data frame
    all_leagues_team_stats <- bind_rows(all_leagues_team_stats,
                                        curr_league_stats)
    
    Sys.sleep(30)
  }
  
  return(all_leagues_team_stats)
}




############## get_team_stats_fifa #################
# outputs: should return a data frame which contains the
# information about team-based data from sofifa.com
# example: includes the FIFA game rating for each club and some other information
# such as the number of players in the club
get_team_stats_fifa <- function(fifa_version, league = "bundesliga", 
                                max_date_in_database = NULL) {
  # create the url endpoint based on the league parameter
  league <- str_to_title(league)
  
  # set the base url
  base_url <- "https://sofifa.com/teams?type=all"
  
  # depending on the given league we have to append a certain url
  # to our base url
  if(league == "Bundesliga"){
    league_url <- paste0(base_url, "&lg%5B%5D=19")
  } else if(league == "Bundesliga 2"){
    league_url <- paste0(base_url, "&lg%5B%5D=20")
  } else if(league == "Premier League"){
    league_url <- paste0(base_url, "&lg%5B%5D=13")
  } else if(league == "Serie A"){
    league_url <- paste0(base_url, "&lg%5B%5D=31")
  } else if(league == "La Liga"){
    league_url <- paste0(base_url, "&lg%5B%5D=53")
  } else if(league == "Ligue 1"){
    league_url <- paste0(base_url, "&lg%5B%5D=16")
  }

  # create the final url by also appending urls for the columns we want to
  # extract
  final_url <- paste0(league_url, 
                      "&showCol%5B%5D=oa&showCol%5B%5D=at&",
                      "showCol%5B%5D=md&showCol%5B%5D=df&showCol%5B%5D=dp&",
                      "showCol%5B%5D=ip&showCol%5B%5D=sa&",
                      "showCol%5B%5D=ta")
  
  
  # if no date is given we want to extract all fifa versions that are at least
  # the given fifa version
  if(is.null(max_date_in_database)){
    fifa_versions <- get_available_fifa_versions() %>%
      .[. >= fifa_version]
    # if a date is given we only want to extract the fifa version that is actually
    # given via parameter
  } else {
    fifa_versions <- get_available_fifa_versions() %>%
      .[. == fifa_version]
  }
  
  
  # sleep for 5 seconds to avoid timeouts
  Sys.sleep(5)
  
  # initialize the empty frame
  club_stats_frame <- NULL
  
  # iterate through all extracted fifa versions
  for (i in 1:length(fifa_versions)) {
    
    # get the available urls (for a given date) and the actual dates
    available_refs_dates <- get_available_refs_and_dates(final_url, fifa_versions[i])
    
    # if there is no date given we just extract the urls and the dates as
    # they are
    if(is.null(max_date_in_database)){
      url_for_date <- available_refs_dates[[1]]
      available_dates <- available_refs_dates[[2]]
      # if we have a date given we also extract them
    } else {
      url_for_date <- available_refs_dates[[1]]
      available_dates <- available_refs_dates[[2]]
      
      # but here we filter the available dates to only contain dates 
      # that are greater than the given date
      max_date_in_database <- ymd(max_date_in_database)
      available_dates_curr <- mdy(available_dates) %>%
        .[. > max_date_in_database]
      
      # if the max available date is already in the data base
      # the available_dates_curr element is empty and we return NULL
      if(length(available_dates_curr) == 0){
        return(NULL)
      }
      
      # now also filter the urls to only contain the corresponding urls
      # for the subset of dates
      url_for_date <- url_for_date[1:length(available_dates_curr)]
      
    }
    
    # again, wait for 5 seconds
    Sys.sleep(5)
    
    # iterate through all extracted date urls
    for (j in 1:length(url_for_date)) {
      # now extract the table of all clubs for each date (in each fifa version)
      club_stats <- read_html(url_for_date[j]) %>%
        html_nodes(xpath = "//table") %>%
        # convert it directly into a data frame
        html_table() %>%
        data.frame() %>%
        # drop misparsed column for the logo
        select(-`Var.1`)
      
      # short pause
      Sys.sleep(1)
      
      # split the Name column into club and league by removing the the country/nationality
      if(league %in% c("Bundesliga", "Bundesliga 2")){
        club_stats <- club_stats %>%
          separate(col = Name,
                   into = c("club", "league"),
                   sep = " German")
      } else if(league == "Premier League"){
        club_stats <- club_stats %>%
          separate(col = Name,
                   into = c("club", "league"),
                   sep = " English")
      } else if(league == "Serie A"){
        club_stats <- club_stats %>%
          separate(col = Name,
                   into = c("club", "league"),
                   sep = " Italian")
      } else if(league == "La Liga"){
        club_stats <- club_stats %>%
          separate(col = Name,
                   into = c("club", "league"),
                   sep = " Spain")
      } else if(league == "Ligue 1"){
        club_stats <- club_stats %>%
          separate(col = Name,
                   into = c("club", "league"),
                   sep = " French")
      }

      # now make some mutations to the extracted data frame
      club_stats <- club_stats %>%
        # add variables for the date and the fifa version
        # and remove the uncessesary parantheses in the league column
        mutate(
          date = mdy(available_dates[j]),
          fifa_vers = fifa_versions[i],
          club = trimws(club),
          league = trimws(str_remove_all(league, pattern = "\\(.*\\)"))) %>%
        # rename the stats to more intuitive names
        rename(fifa_overall_rating = OVA,
               fifa_attack_rating = ATT,
               fifa_midfield_rating = MID,
               fifa_def_rating = DEF,
               fifa_domestic_prestige = DP,
               fifa_international_prestige = IP,
               starting_XIV_age_avg = SAA,
               total_age_avg = TAA) %>%
        # we have to rename the league if it is for the bundesliga or the
        # bundesliga 2 because we need it in a format we can work with
        mutate(league = ifelse(
          str_detect(league, pattern = "1. Bundesliga"),
          "Bundesliga",
          ifelse(str_detect(league, pattern = "2. Bundesliga"),
                 "Bundesliga 2", league)))
      
      # append the newly extracted rows to the current data frame
      club_stats_frame <- bind_rows(club_stats_frame, club_stats)
      
      # because it is good practise, we wait again before we
      # go to the next iteration
      Sys.sleep(5)
    }
    
  }
  
  # map the club names 
  club_stats_frame$club <- sapply(club_stats_frame$club,
                                  club_name_mapping)
  
  # reorder the columns
  club_stats_frame <- club_stats_frame %>%
    select(fifa_vers, date, league, club, everything()) %>%
    # and sort the rows
    arrange(fifa_vers, date, league, club)
  
  # return the data frame
  return(club_stats_frame)
  
}


############## get_squads_fifa_all_leagues #################
# function should return the current fifa squads for all leagues
# given by a vector
get_squads_fifa_all_leagues <- function(leagues, fifa_version, date){
  # create a variable to store all the squads for all leagues
  all_leagues_squads <- NULL
  
  # iterate over all leagues given as a vector of strings
  for(i in 1:length(leagues)){
    # first get a random port
    ran_port <- randomPort()
    # for every league, get the fifa squads for the given fifa version
    # and date
    curr_league_squads <- get_squads_fifa(fifa_version, league = leagues[i],
                                               max_date_in_database = date,
                                               single_fifa_version = TRUE,
                                               port = ran_port)
    
    # check if the squads are null and if so return NULL
    if(is.null(curr_league_squads)){
      return(NULL)
    }
    
    # append the curr league squads to the overall data frame
    all_leagues_squads <- bind_rows(all_leagues_squads,
                                    curr_league_squads)
    
    Sys.sleep(30)
  }
  
  return(all_leagues_squads)
}


############## get_squads_fifa #################
# outputs: should return a data frame which contains squad data (and statistics)
# example: includes the FIFA player potential rating 
get_squads_fifa <- function(league_id, fifa_version, port = 4124L,
                            max_date_in_database = NULL,
                            single_fifa_version = TRUE){
  
  
  # create a driver from Rselenium
  rD <- rsDriver(browser = "firefox", extraCapabilities = list(
    "moz:firefoxOptions" = list(
      args = list('--headless'))),
    port = port)
  
  # get the client
  remDr <- rD$client
  
  # set time outs to give the page the chance to first fully load before
  # we try to get information form it
  remDr$setTimeout(type = "implicit", milliseconds = 50000)
  remDr$setTimeout(type = "page load", milliseconds = 500000)
  remDr$setTimeout(type = "script", milliseconds = 50000)
  
  # use the external function set_implicit_wait
  set_implicit_wait(remDr, milliseconds = 50000)
  
  # create the base url
  base_url <- "https://sofifa.com"
  
  # uses the given league id to create the url for the league
  league_url <- paste0(base_url, "/teams?lg=", league_id)
  
  
  # get the fifa versions we want to depending on our input parameters
  if(is.null(max_date_in_database)){
    # extract only the current fifa version if single_fifa_version is set to
    # true and there is no date given
    if(single_fifa_version){
      fifa_versions <- get_available_fifa_versions() %>%
        .[. == fifa_version]
      # else get all the fifa versions
    } else {
      fifa_versions <- get_available_fifa_versions() %>%
        .[. >= fifa_version]
    }
    
    # we also want to get just the current fifa version if there is a date given
  } else {
    fifa_versions <- get_available_fifa_versions() %>%
      .[. == fifa_version]
  }
  
  # wait for a short time
  Sys.sleep(1)
  
  # initialize the empty frame
  club_squads_frame <- NULL
  
  # iterate through all extracted fifa versions
  for (i in 1:length(fifa_versions)) {
    print(paste0("Current fifa version: ", fifa_versions[i]))
    # get the available urls (for a given date) and the actual dates
    available_refs_dates <- get_available_refs_and_dates(league_url, fifa_versions[i])
    
    # if there is no date given we just extract the urls and the dates as
    # they are but converting the dates into actual dates of the form YYYY-MM-DD
    if(is.null(max_date_in_database)){
      url_for_date <- available_refs_dates[[1]]
      available_dates <- available_refs_dates[[2]] %>%
        mdy()
      # in case there is a date given we also extract the urls and the dates
    } else {
      url_for_date <- available_refs_dates[[1]]
      available_dates <- available_refs_dates[[2]]
      
      available_dates_curr <- mdy(available_dates) %>%
        .[. > max_date_in_database]
      
      # if the max available date is already in the data base
      # the available_dates_curr element is empty and we return NULL
      if(length(available_dates_curr) == 0){
        return(NULL)
      }
      
      # but filtering for only those dates that are greater than the given date
      max_date_in_database <- ymd(max_date_in_database)
      available_dates <- mdy(available_dates) %>%
        .[. > max_date_in_database]
      
      # also take only the corresponding urls to the subset of dates
      url_for_date <- url_for_date[1:length(available_dates)]
    }
    
    # wait for 1 second for good practice
    Sys.sleep(1)
    
    # iterate through all extracted date urls
    for (j in 1:length(url_for_date)) {
      # extrac the current date
      curr_date <- available_dates[j]
      print(paste0("Current date: ", curr_date))
      
      # map the league id with the helper function to get the name
      # of the league
      league_name <- fifa_league_id_mapping(league_id)
      
      # navigate to the created url
      remDr$navigate(url_for_date[j])
      
      # extract the content of the given url
      page_content <- read_html(remDr$getPageSource()[[1]])
      
      # store the team names and urls
      team_names_and_refs <- page_content %>%
        html_nodes(xpath = "//table[@class='table table-hover persist-area']//td[@class='col-name-wide']/a[1]")
        
      # extract the team names into a vector of strings
      team_names <- team_names_and_refs %>%
        html_text(trim = TRUE)
      
      # extract the team refs
      team_refs <- team_names_and_refs %>%
        html_attr("href")
      
      Sys.sleep(1)
  
      # iterate over all teams in the league
      for(k in 1:length(team_refs)){
        print(paste0("Current team: ", team_refs[k]))
        
        # prepare the url for the team by extracting a certain part of the string
        # which indicates the the date
        fifa_vers_and_date_ending <- url_for_date[j] %>%
          str_extract(., pattern = "r=.*")
        
        # paste together the final url for the current team
        team_url <- paste0(base_url, team_refs[k], "?", fifa_vers_and_date_ending)
        
        # navigate to the created url
        remDr$navigate(team_url)
        
        # extract the html of the team page
        curr_team_html <- read_html(remDr$getPageSource()[[1]])
        
        # extract the names of the players
        full_names_players <- curr_team_html %>%
          html_nodes(xpath = "//table[@class='table table-hover persist-area']//td[@class='col-name']//a[1]") %>%
          html_attr("aria-label")
        
        # extract the other information available such as
        # rating, age or potential
        players_all_infos <- curr_team_html %>%
          html_nodes(xpath = "//table[@class='table table-hover persist-area']") %>%
          html_nodes(xpath = paste0(".//td[@class='col-name']//a[1]",
                                    "|.//td[@class='col col-ae']",
                                    "|.//td[@class='col col-oa']//span",
                                    "|.//td[@class='col col-pt']//span",
                                    "|.//td[@class='col col-vl']")) %>%
          html_text(trim = TRUE) %>%
          # convert it into a data frame with 5 columns
          matrix(ncol = 5, byrow = TRUE) %>%
          as.data.frame() %>%
          # rename variables
          select("fifa_player_name_short" = V1,
                 "fifa_player_age" = V2,
                 "fifa_player_overall_rating" = V3,
                 "fifa_player_potential" = V4,
                 "fifa_player_market_value" = V5) %>%
          # bind together the name and the values
          cbind("fifa_player_name_long" = full_names_players) %>%
          # create a new variable for the player name depending on whether
          # the name is shortened by a "." or not
          mutate(fifa_player_name = ifelse(str_detect(fifa_player_name_short,
                                                      pattern = "\\."),
                                           fifa_player_name_long,
                                           fifa_player_name_short),
                 # create other variables for the fifa version, the date 
                 # or the league
                 fifa_version = fifa_versions[i],
                 date = ymd(curr_date),
                 league = league_name,
                 team = team_names[k],
                 # transform the market values into a proper numerical format
                 fifa_player_market_value_mil_euro = str_remove_all(fifa_player_market_value,
                                                                    "\u20ac"),
                 fifa_player_market_value_mil_euro = ifelse(endsWith(fifa_player_market_value_mil_euro, "M"),
                                                            as.numeric(str_remove(fifa_player_market_value_mil_euro,
                                                                                  pattern = "M")),
                                                            as.numeric(str_remove(fifa_player_market_value_mil_euro, 
                                                                                  pattern = "K"))/1000),
                 # convert all variables suited to numerics
                 across(c(fifa_player_age:fifa_player_potential), as.numeric)) %>%
          # reorder the data frame
          select(fifa_version, date, league, team, fifa_player_name, 
                 fifa_player_name_short, fifa_player_name_long, fifa_player_age, 
                 fifa_player_overall_rating, fifa_player_potential, 
                 fifa_player_market_value_mil_euro)
        
        # map the team names 
        players_all_infos$team <- sapply(players_all_infos$team,
                                         club_name_mapping) %>%
          unname()
        
        
        # bind together the current team data with the overall data frame
        club_squads_frame <- bind_rows(club_squads_frame,
                                       players_all_infos)
        
        Sys.sleep(2)
      }
      
      Sys.sleep(2)
    }
  }
  
  # close the driver (client) and the server
  remDr$close()
  rD$server$stop()
  rm(rD)
  gc()
  closeAllConnections()
  
  return(club_squads_frame)
      
}



############## fifa_league_id_mapping ############## 
# function should map a given league id into a league name
# which is then be used in the get_fifa_squads function
fifa_league_id_mapping <- function(league_id){
  # convert the given league id into a numeric value
  league_id <- as.numeric(league_id)
  
  # create an empty variable to store the mapped name
  league_name <- NULL
  
  # based on the league_id set the league_name accordingly
  if(league_id == 19){
    league_name <- "Bundesliga"
  } else if(league_id == 20){
    league_name <- "Bundesliga 2"
  } else if(league_id == 13){
    league_name <- "Premier League"
  } else if(league_id == 31){
    league_name <- "Serie A"
  } else if(league_id == 53){
    league_name <- "La Liga"
  } else if(league_id == 16){
    league_name <- "Ligue 1"
  }
  
  return(league_name)
}



############## get_available_fifa_versions ############## 
# function returns all available fifa versions 
get_available_fifa_versions <- function(){
  
  # create base url
  base_url <- "https://sofifa.com"
  
  # get the fifa versions of the game (back until 2010)
  available_fifa_versions <- read_html(base_url) %>%
    html_nodes(css = "a.bp3-menu-item") %>%
    html_text() %>%
    # make all values lower case
    tolower() %>%
    # get all elements which contain the string fifa
    # all the FIFA versions look like this: FIFA XX where XX stands for a
    # version, e.g., 22
    .[str_detect(., pattern = "fifa")] %>%
    # extract the numbers
    str_extract(pattern = "[0-9]+") %>%
    # transform them into a numeric value
    as.numeric() %>%
    # reverse order of the vector
    rev(.)
  
  return(available_fifa_versions)
}



############## get_available_refs_and_dates ############## 
# function returns all available dates (and date refs) for a given
# fifa version
get_available_refs_and_dates <- function(final_url, fifa_version){
  # create the url endpoint for the current fifa version
  curr_fifa_url <-
    paste0(final_url, "&r=", fifa_version, "0001",
           "&set=true")
  
  # because seasons usually start in summer of year 1 and last until
  # autumn, there are two values for the year, year 1 and year 2
  # here, we want to see if the year is two digits long, if not we append
  # a 0 which is necessary to get the correct url
  fifa_version_cur <-
    ifelse(nchar(trunc(abs(fifa_version))) == 2,
           fifa_version,
           paste0("0", fifa_version))
  
  fifa_version_prev <-
    ifelse(nchar(trunc(abs(fifa_version - 1))) == 2,
           fifa_version - 1, 
           paste0("0", (fifa_version - 1)))
  
  # extract all selectable dates for a given fifa version
  curr_fifa_page <- read_html(curr_fifa_url)
  
  # get the available dates
  available_dates <- curr_fifa_page %>%
    html_nodes(css = "a.bp3-menu-item") %>%
    html_text() %>%
    # extract only those elements which contains 4 digits for the year
    .[str_detect(., pattern = paste0("20", fifa_version_cur, "|20",
                                     fifa_version_prev, ""))]
  
  # get the urls for the dates extracted above
  url_for_date <- curr_fifa_page %>%
    html_nodes(css = "a.bp3-menu-item") %>%
    # extract all hyperlinks
    html_attr("href") %>%
    # get only those which end with r= followed by the fifa version and
    # &set=true. $ marks the end of the string
    .[str_detect(., pattern = paste0("r=", fifa_version, ".*&set=true$"))] %>%
    unique() %>%
    # to be able to use the url, we paste the sofifa.com beginning to it
    paste0("https://sofifa.com", .)
  
  # returns a list with first element the urls and the second element
  # the actual dates
  return(list(url_for_date, available_dates))
}

