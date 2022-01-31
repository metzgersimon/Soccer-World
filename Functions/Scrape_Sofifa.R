############## get_team_stats_full_fifa #################
# inputs
# outputs: should return a data frame which contains the
# information about team-based data from sofifa.com
# example: includes the FIFA game rating for each club and some other information
# such as the number of players in the club

get_team_stats_full_fifa <- function(fifa_version, league = "bundesliga", max_date_in_database = NULL) {
  # create the url endpoint based on the league parameter
  league <- str_to_title(league)
  
  base_url <- "https://sofifa.com/teams?type=all"
  
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

  final_url <- paste0(league_url, 
                      "&showCol%5B%5D=oa&showCol%5B%5D=at&",
                      "showCol%5B%5D=md&showCol%5B%5D=df&showCol%5B%5D=dp&",
                      "showCol%5B%5D=ip&showCol%5B%5D=sa&",
                      "showCol%5B%5D=ta")
  
  # get the fifa versions we want to use
  if(is.null(max_date_in_database)){
    fifa_versions <- get_available_fifa_versions() %>%
      .[. >= fifa_version]
  } else {
    fifa_versions <- get_available_fifa_versions() %>%
      .[. == fifa_version]
  }
  
  
  Sys.sleep(1)
  
  # initialize the empty frame
  club_stats_frame <- NULL
  
  # iterate through all extracted fifa versions
  for (i in 1:length(fifa_versions)) {
    
    available_refs_dates <- get_available_refs_and_dates(final_url, fifa_versions[i])
    
    if(is.null(max_date_in_database)){
      url_for_date <- available_refs_dates[[1]]
      available_dates <- available_refs_dates[[2]]
    } else {
      url_for_date <- available_refs_dates[[1]][1]
      available_dates <- available_refs_dates[[2]][1]
      
      max_date_in_database <- ymd(max_date_in_database)
      available_dates_curr <- mdy(available_dates)
      
      if(!(available_dates_curr > max_date_in_database)){
        return(NULL)
      }
    }
    
    
    Sys.sleep(1)
    
    # iterate through all extracted date urls
    for (j in 1:length(url_for_date)) {
      # now extract the table of all clubs for each date (in each fifa version)
      club_stats <- read_html(url_for_date[j]) %>%
        html_nodes(xpath = "//table") %>%
        html_table() %>%
        data.frame() %>%
        # drop misparsed column for the logo
        select(-`Var.1`)
      
      Sys.sleep(1)
      
      # split the Name column into club and league by removing the word of the country
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
                   sep = " Italy")
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

      club_stats <- club_stats %>%
        # add variables for the date and the fifa version
        # and remove the uncessesary parantheses in the league column
        mutate(
          date = mdy(available_dates[j]),
          fifa_vers = fifa_versions[i],
          league = trimws(str_remove_all(league, pattern = "\\(.*\\)"))) %>%
        rename(fifa_overall_rating = OVA,
               fifa_attack_rating = ATT,
               fifa_midfield_rating = MID,
               fifa_def_rating = DEF,
               fifa_domestic_prestige = DP,
               fifa_international_prestige = IP,
               starting_XIV_age_avg = SAA,
               total_age_avg = TAA) %>%
        mutate(league = ifelse(
          str_detect(league, pattern = "1. Bundesliga"),
          "Bundesliga",
          ifelse(str_detect(league, pattern = "2. Bundesliga"),
                 "Bundesliga 2", NA)))
      
      # append the newly extracted rows to the current data frame
      club_stats_frame <- bind_rows(club_stats_frame, club_stats)
      
      # because it is good practise, we wait for 1 second before we
      # go to the next iteration
      Sys.sleep(2)
    }
    
  }
  
  # map the club names 
  club_stats_frame$club <- sapply(club_stats_frame$club,
                                  club_name_mapping)
  
  # return the data frame
  return(club_stats_frame)
  
}



get_squads_full_fifa <- function(league_id, fifa_version, port = 4124L, max_date_in_database = NULL,
                                 single_fifa_version = TRUE){
  # create a driver from Rselenium
  rD <- rsDriver(browser = "firefox", port = port)
  
  # get the client
  remDr <- rD$client
  
  # set time outs to give the page the change to first fully load before
  # we try to get information form it
  remDr$setTimeout(type = "implicit", milliseconds = 10000)
  remDr$setTimeout(type = "page load", milliseconds = 10000)
  
  base_url <- "https://sofifa.com"
  
  league_url <- paste0(base_url, "/teams?lg=", league_id)
  
  
  # get the fifa versions we want to use
  if(is.null(max_date_in_database)){
    if(single_fifa_version){
      fifa_versions <- get_available_fifa_versions() %>%
        .[. == fifa_version]
    } else {
      fifa_versions <- get_available_fifa_versions() %>%
        .[. >= fifa_version]
    }
    
  } else {
    fifa_versions <- get_available_fifa_versions() %>%
      .[. == fifa_version]
  }
  
  Sys.sleep(1)
  
  # initialize the empty frame
  club_squads_frame <- NULL
  
  # iterate through all extracted fifa versions
  for (i in 1:length(fifa_versions)) {
    available_refs_dates <- get_available_refs_and_dates(league_url, fifa_versions[i])
    
    if(is.null(max_date_in_database)){
      url_for_date <- available_refs_dates[[1]]
      available_dates <- available_refs_dates[[2]]
    } else {
      url_for_date <- available_refs_dates[[1]][1]
      available_dates <- available_refs_dates[[2]][1]
      
      max_date_in_database <- ymd(max_date_in_database)
      available_dates_curr <- mdy(available_dates)
      
      if(!(available_dates_curr > max_date_in_database)){
        return(NULL)
      }
    }
    
    
    Sys.sleep(1)
    
    # iterate through all extracted date urls
    for (j in 1:length(url_for_date)) {
      # now extract the table of all clubs for each date (in each fifa version)

      curr_date <- available_dates[j]
      
      league_name <- fifa_league_id_mapping(league_id)
      
      # navigate to the created url
      remDr$navigate(url_for_date[j])
      
      page_content <- read_html(remDr$getPageSource()[[1]])
      
      team_names_and_refs <- page_content %>%
        html_nodes(xpath = "//table[@class='table table-hover persist-area']//td[@class='col-name-wide']/a[1]")
        
      team_names <- team_names_and_refs %>%
        html_text(trim = TRUE)
      
      team_refs <- team_names_and_refs %>%
        html_attr("href")
      
      Sys.sleep(1)
  
      # iterate over all teams in the league
      for(k in 1:length(team_refs)){
        print(paste0("Current fifa version: ", fifa_versions[i]))
        print(paste0("Current date: ", curr_date))
        print(paste0("Current team: ", team_refs[k]))
        
        fifa_vers_and_date_ending <- url_for_date[j] %>%
          str_extract(., pattern = "r=.*")
        
        team_url <- paste0(base_url, team_refs[k], "?", fifa_vers_and_date_ending)
        
        # navigate to the created url
        remDr$navigate(team_url)
        
        curr_team_html <- read_html(remDr$getPageSource()[[1]])
        
        full_names_players <- curr_team_html %>%
          html_nodes(xpath = "//table[@class='table table-hover persist-area']//td[@class='col-name']//a[1]") %>%
          html_attr("aria-label")
        
        players_all_infos <- curr_team_html %>%
          html_nodes(xpath = "//table[@class='table table-hover persist-area']") %>%
          html_nodes(xpath = paste0(".//td[@class='col-name']//a[1]",
                                    "|.//td[@class='col col-ae']",
                                    "|.//td[@class='col col-oa']//span",
                                    "|.//td[@class='col col-pt']//span",
                                    "|.//td[@class='col col-vl']")) %>%
          html_text(trim = TRUE) %>%
          matrix(ncol = 5, byrow = TRUE) %>%
          as.data.frame() %>%
          select("fifa_player_name_short" = V1,
                 "fifa_player_age" = V2,
                 "fifa_player_overall_rating" = V3,
                 "fifa_player_potential" = V4,
                 "fifa_player_market_value" = V5) %>%
          cbind("fifa_player_name_long" = full_names_players) %>%
          mutate(fifa_player_name = ifelse(str_detect(fifa_player_name_short,
                                                      pattern = "\\."),
                                           fifa_player_name_long,
                                           fifa_player_name_short),
                 fifa_version = fifa_versions[i],
                 date = mdy(curr_date),
                 league = league_name,
                 team = team_names[k],
                 fifa_player_market_value_mil_euro = str_remove_all(fifa_player_market_value,
                                                                    "\u20ac"),
                 fifa_player_market_value_mil_euro = ifelse(endsWith(fifa_player_market_value_mil_euro, "M"),
                                                            as.numeric(str_remove(fifa_player_market_value_mil_euro,
                                                                                  pattern = "M")),
                                                            as.numeric(str_remove(fifa_player_market_value_mil_euro, 
                                                                                  pattern = "K"))/1000),
                 across(c(fifa_player_age:fifa_player_potential), as.numeric)) %>%
          select(fifa_version, date, league, team, fifa_player_name, 
                 fifa_player_name_short, fifa_player_name_long, fifa_player_age, 
                 fifa_player_overall_rating, fifa_player_potential, 
                 fifa_player_market_value_mil_euro)
        
        players_all_infos$team <- sapply(players_all_infos$team,
                                         club_name_mapping) %>%
          unname()
        
        
        club_squads_frame <- bind_rows(club_squads_frame,
                                       players_all_infos)
        
        Sys.sleep(2)
          
      }
    }
  }
  
  # close the driver (client) and the server
  remDr$close()
  rD$server$stop()
  rm(rD)
  gc()
  
  return(club_squads_frame)
      
}


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


get_available_fifa_versions <- function(){
  
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
  
  return(list(url_for_date, available_dates))
}

