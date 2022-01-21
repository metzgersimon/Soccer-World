get_match_dates <- function(league, season){
  # based on the league parameter get the correct match data and
  # filter it to only contain data for the selected season
  if(league == "Bundesliga"){
    league_matches <- tbl(con, "buli_matches_2010_2021") %>%
      filter(league_season == season) %>%
      data.frame()
  } else if(league == "Bundesliga 2"){
    league_matches <- tbl(con, "buli2_matches_2011_to_2021") %>%
      filter(league_season == season) %>%
      data.frame()
  } else if(league == "Premier League"){
    league_matches <- tbl(con, "pl_matches_2010_to_2021") %>%
      filter(league_season == season) %>%
      data.frame()
  } else if(league == "La Liga"){
    league_matches <- tbl(con, "la_liga_matches_2010_to_2021") %>%
      filter(league_season == season) %>%
      data.frame()
  }
  
  # set the system locale to get the abbreviated months in english and not german
  Sys.setlocale("LC_TIME", "C")
  
  # extract the match dates and the respective matchday for a given date
  match_dates <- league_matches %>%
    select(fixture_date, league_round) %>%
    filter(fixture_date < Sys.Date()) %>%
    # convert the dates into an appropriate format for the understat page
    mutate(fixture_date = format(as.Date(fixture_date, "%Y-%m-%d"), "%b %d, %Y")) %>%
    unique()
  
  
  
  return(match_dates)
  
}


get_matchday_table_undestat <- function(league, season, port = 5143L){
  # convert league to first letter uppercase
  league <- str_to_title(league)
  # get all matchdays which are in the selected league and season
  # and are in the past
  match_dates <- get_match_dates(league, season) %>%
    filter(!is.na(league_round))
  
  # paste together base url
  url <- paste0("https://understat.com/league/", league, "/", season)
  
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
  
  # store all data
  season_running_table <- NULL
  

  for(i in 1:max(match_dates$league_round)){
    # get start date selection of the table
    start_date_button <- remDr$findElement(using = "xpath",
                                           paste0("//div[@class='filters']//input[@name='date-start']"))
    
    # start_date <- match_dates %>%
    #   group_by(league_round) %>%
    #   filter(league_round == 1,
    #          fixture_date == min(fixture_date)) %>%
    #   select(fixture_date) %>%
    #   pull() %>%
    #   str_split(" ") %>%
    #   .[[1]]
    
    curr_matchday <- match_dates %>%
      group_by(league_round) %>%
      summarize(min_date = min(fixture_date),
        max_date = max(fixture_date)) %>%
      filter(league_round == i)
    
    start_date <- str_split(curr_matchday$min_date, " ") %>%
      .[[1]]
    
    start_date_month <- paste0(start_date[1], " ")
    start_date_day <- paste0(start_date[2], " ")
    
    start_date_button$clickElement()
    start_date_button$sendKeysToElement(c(start_date_month, start_date_day,
                                          start_date[3]))
    
    
    end_date_button <- remDr$findElement(using = "xpath",
                                         paste0("//div[@class='filters']//input[@name='date-end']"))
    
    
    
    end_date <- str_split(curr_matchday$max_date, " ") %>%
      .[[1]]
    end_date_month <- paste0(end_date[1], " ")
    end_date_day <- paste0(end_date[2], " ")
    
    end_date_button$clickElement()
    end_date_button$sendKeysToElement(c(end_date_month, end_date_day,
                                        end_date[3])) 
    
    
    # dummy button to click to active the date filters
    dummy_button <- remDr$findElement(using = "xpath",
                                      "//div[@class='filter']//label[@for='scheme1']")
    
    dummy_button$clickElement()
    
    curr_matchday_table <- read_html(remDr$getPageSource()[[1]]) %>%
      html_nodes(xpath = "//table") %>%
      .[[1]] %>%
      html_table() %>%
      separate(col = xG, into = c("exp_goals", "diff_exp_goals_goals"), sep = "[+|-]") %>%
      separate(col = xGA, into = c("exp_goals_against", 
                                   "diff_exp_goals_against_goals_against"),
               sep = "[+|-]") %>%
      separate(col = xPTS, into = c("exp_points", "diff_exp_points_points"), 
               sep = "[+|-]") %>%
      mutate(across(c(exp_goals:diff_exp_points_points), as.numeric))
    
    curr_matchday_table$Team <- sapply(curr_matchday_table$Team,
                                       club_name_mapping)
    
    curr_matchday_table <- curr_matchday_table %>%
      mutate(Team = unname(Team))
    
    season_running_table <- bind_rows(season_running_table,
                                      curr_matchday_table)
    
    start_date_clear <- remDr$findElement(using = "xpath",
                                        paste0("//div[@class='filters']//span[@class='datepicker-icon datepicker-clear'][1]"))
    
    
    end_date_clear <- remDr$findElement(using = "xpath",
                                        paste0("//div[@class='filters']//span[@class='datepicker-icon datepicker-clear'][2]"))
    
    start_date_clear$clickElement()
    end_date_clear$clickElement()
    
    Sys.sleep(2)
    
    
  }
  
  season_running_table <- season_running_table %>%
    select(rank = "\U2116", club = Team, matches_played = M, 
           wins = W, draws = D, losses = L, goals = G, goals_against = GA,
           points = PTS, everything()) %>%
    data.frame()
  
  # close the driver (client) and the server
  remDr$close()
  rD$server$stop()
  rm(rD)
  gc()
  
  return(season_running_table)
  
}

  # # get the button which corresponds to the previous week
  # previous_week_button <- remDr$findElement(using = "xpath",
  #                                           paste0("//div[@class='calendar']//button[@class='calendar-prev']"))
  # 
  # # flag variable to check if there is a previous week available
  # previous_week_enabled <- ifelse(previous_week_button$isElementEnabled(), 
  #                                 TRUE, FALSE)
  # 
  # # create variable for season_stats
  # season_stats <- NULL
  # 
  # # create a matchday counter
  # matchday <- 1
  # 
  # extract the path to the previous week button until there is no previous
  # week available, i.e., we are at the beginning of the season
  # while(previous_week_enabled){
  #   # click the previous week button 
  #   previous_week_button$clickElement()
  #   
  #   # set the previous_week_enabled variable again to be able to check whether there is 
  #   # a previous week available
  #   previous_week_enabled <- ifelse(previous_week_button$isElementEnabled(), 
  #                                   TRUE, FALSE)
  #   
  #   # increase the matchday counter
  #   matchday <- matchday + 1
  # }
  
  # get the button which corresponds to the next week
  # next_week_button <- remDr$findElement(using = "xpath",
  #                                           paste0("//div[@class='calendar']//button[@class='calendar-next']"))
  # 
  # # flag variable to check if there is a next week available
  # next_week_enabled <- ifelse(next_week_button$isElementEnabled(), 
  #                                 TRUE, FALSE)
  
  
  
  # extract the path to the next week button until there is no next
  # week available, i.e., we are at the end of the season
  # while(next_week_enabled){
  #   
  #   print(paste0("Matchday: ", matchday))
  #   
  #   # extract the html of the page
  #   page_html <- read_html(remDr$getPageSource()[[1]])
  #   
  #   # get all match refs of the selected matchday (week)
  #   match_refs <- page_html %>%
  #     html_nodes(xpath = "//a[@class='match-info']") %>%
  #     html_attr("href")
  #   
  #   Sys.sleep(2)
  #   
  #   # create empty variable to store the matchday stats
  #   matchday_stats <- NULL
  #   
  #   # iterate over all matches
  #   for(i in 1:length(match_refs)){
  #     print(paste0("Match: ", i))
  #     # paste together the url of the current match
  #     match_url <- paste0("https://understat.com/", match_refs[i])
  #     
  #     # navigate to the created url
  #     remDr$navigate(match_url)
  #     
  #     # get the html of the current page 
  #     page_html <- read_html(remDr$getPageSource()[[1]])
  #     
  #     # get the date of the match
  #     match_date <- page_html %>%
  #       html_nodes(xpath = "//ul[@class='breadcrumb']//li[3]") %>%
  #       html_text(trim = TRUE) %>%
  #       # convert it into ymd format
  #       mdy()
  #     
  #     if(match_date >= Sys.Date()){
  #       break
  #     }
  #     
  #     # get the stats button which leads to the match stats
  #     stats_button <- remDr$findElement(using = "xpath",
  #                                       paste0("//div[@class='filter']//label[@for='scheme3']"))
  #     
  #     # click the stats button
  #     stats_button$clickElement()
  #     
  #     # extract the match stats from the chart
  #     match_stats <- page_html %>%
  #       # html_nodes(xpath = paste0("//div[@data-scheme='stats']")) %>%
  #       html_nodes(xpath = paste0("//div[@data-scheme='stats']//div[@class='progress-bar teams-titles']//div[@class='progress-title']",
  #                                 "|//div[@data-scheme='stats']//div[@class='progress-bar teams-titles']//div[@class='progress-value']",
  #                                 "|//div[@data-scheme='stats']//div[@class='progress-bar'][position() > 1]//div[@class='progress-title']",
  #                                 "|//div[@data-scheme='stats']/div[@class='progress-bar'][position() > 1]//div[@class='progress-value']")) %>%
  #       html_text(trim = TRUE) %>%
  #       # remove line breaks
  #       # str_split(pattern = "\\n") %>%
  #       # .[[1]] %>%
  #       # str_split(pattern = "\\t") %>%
  #       # # remove tabs
  #       # unlist() %>%
  #       # remove empty strings
  #       str_subset(pattern = ".+")
  #     
  #     # split the team_names and the stats into two variables
  #     # because we do not want to have the chances stats and these are in between
  #     team_names <- match_stats[1:3]
  #     
  #     # check where the GOALS variable (first stat we want) is
  #     # if(match_stats[8] == "GOALS"){
  #     #   stats <- match_stats[8:length(match_stats)]
  #     # } else if(match_stats[7] == "GOALS"){
  #     #   stats <- match_stats[7:length(match_stats)]
  #     # }
  #     
  #     # now we have to combine the string back together
  #     # match_stats <- c(team_names, stats)
  #     
  #     # convert it into a data frame with 3 columns
  #     match_stats <- match_stats %>%
  #       matrix(ncol = 3, byrow = TRUE) %>%
  #       data.frame() %>%
  #       # rename variables
  #       select(
  #         statistic = X1,
  #         stat_home_team = X2,
  #         stat_away_team = X3
  #       )
  #     
  #     # transform match stats to get only one row per team
  #     match_stats <- t(match_stats) %>%
  #       data.frame()
  #     
  #     # set the column names to the first row of the data frame
  #     colnames(match_stats) <- match_stats[1, ]
  #     # reset the row names
  #     rownames(match_stats) <- 1:nrow(match_stats)
  #     
  #     # remove the first row 
  #     match_stats <- match_stats[-1, ]
  #     
  #     # append the current match stats to the matchday stats frame
  #     matchday_stats <- bind_rows(matchday_stats,
  #                                 match_stats)
  #     
  #     
  #     Sys.sleep(1)
  #     
  #   }
  #   
  #   matchday_stats <- matchday_stats %>%
  #     mutate(matchday = matchday)
  #   
  #   season_stats <- bind_rows(season_stats,
  #                             matchday_stats)
  #   
  #   # increase the matchday counter
  #   matchday <- matchday + 1
  # 
  #   
  #   Sys.sleep(3)
  #     
  #   # # navigate to the created url
  #   remDr$navigate(url)
  #   
  #   # get the button which corresponds to the previous week
  #   previous_week_button <- remDr$findElement(using = "xpath",
  #                                             paste0("//div[@class='calendar']//button[@class='calendar-prev']"))
  #   
  #   # set the previous_week_enabled variable again to be able to check whether there is
  #   # a previous week available
  #   previous_week_enabled <-
  #     ifelse(previous_week_button$isElementEnabled(),
  #            TRUE, FALSE)
  #     
  #   # extract the path to the previous week button until there is no previous
  #   # week available, i.e., we are at the beginning of the season
  #   while (previous_week_enabled) {
  #     # click the previous week button
  #     previous_week_button$clickElement()
  #     
  #     # set the previous_week_enabled variable again to be able to check whether there is
  #     # a previous week available
  #     previous_week_enabled <-
  #       ifelse(previous_week_button$isElementEnabled(),
  #              TRUE, FALSE)
  #   }
  #     
  #   # counter to get to the correct matchday
  #   counter <- 1
  #   
  #   while (counter <= matchday) {
  #     # get the button which corresponds to the next week
  #     next_week_button <- remDr$findElement(using = "xpath",
  #                                           paste0(
  #                                             "//div[@class='calendar']//button[@class='calendar-next']"
  #                                           ))
  #     # flag variable to check if there is a next week available
  #     next_week_enabled <-
  #       ifelse(next_week_button$isElementEnabled(),
  #              TRUE, FALSE)
  #     
  #     
  #     # click the next week button
  #     next_week_button$clickElement()
  #     
  #     counter <- counter + 1
  #   }
  #   
  #   # append the current matchday stats to the season stats frame
  #   # season_stats <- bind_rows(season_stats,
  #   #                           matchday_stats)
  #   
  #   
  #   
  #   # get the button which corresponds to the next week
  #   next_week_button <- remDr$findElement(using = "xpath",
  #                                         paste0("//div[@class='calendar']//button[@class='calendar-next']"))
  #   # 
  #   # # click the next week button 
  #   # next_week_button$clickElement()
  #   
  #   # set the next_week_enabled variable again to be able to check whether there is 
  #   # a next week available
  #   next_week_enabled <- ifelse(next_week_button$isElementEnabled(), 
  #                                   TRUE, FALSE)
  #   
  #   Sys.sleep(2)
  # }
  # 
  # # clean the data frame
  # season_stats <- season_stats %>%
  #   mutate(league = league,
  #          season = season) %>%
  #   mutate(across(c(GOALS:xPTS), as.numeric)) %>%
  #   rename(teams = TEAMS,
  #          goals = GOALS,
  #          shots = SHOTS,
  #          shots_on_goal = `SHOTS ON TARGET`,
  #          deep_passes = DEEP,
  #          passes_defensive_action = PPDA)
  # 

#   # close the driver (client) and the server
#   remDr$close()
#   rD$server$stop()
#   rm(rD)
#   gc()
#   
#   return(season_stats)
#   
# }
