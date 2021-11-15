############## get_number_of_teams_in_season #################
# inputs: root_url, league, season
# outputs: should return a data frame which contains the
# information about the number of teams playing in a given season

get_number_of_teams_in_season <- function(root_url, league, season) {
  # get the URL (of the first matchday)
  url <- paste0(root_url, league, "/", season, "/1")
  
  ranking_page <- read_html(url)
  
  # extract the ranking of the first match day
  ranking_content <-
    html_nodes(ranking_page, css = ".ranking-number")
  
  # extract the number of teams by converting the ranking into
  # number of teams
  number_of_teams <- html_text(ranking_content) %>%
    length() - 1
  
  return(number_of_teams)
}



############## get_results_for_season #################
# inputs: root_url, league, season, number_of_teams
# outputs: should return a data frame which contains the
# information about the results for every match day for a given league

get_results_for_season <-
  function(root_url, league, season, number_of_teams) {
    # compute the number of matchdays (there are a home and away match for every
    # opponent team in the league)
    number_of_matchdays <- (number_of_teams - 1) * 2
    
    # pre-allocate a data frame to store all matches
    total_length <- number_of_matchdays * (number_of_teams / 2)
    
    results_whole_season <- data.frame(
      "date" = rep(NA, total_length),
      "matchday" = rep(NA, total_length),
      "home_team" = rep(NA, total_length),
      "away_team" = rep(NA, total_length),
      "final_goals_home" = rep(NA, total_length),
      "final_goals_away" = rep(NA, total_length),
      "halftime_goals_home" = rep(NA, total_length),
      "halftime_goals_away" = rep(NA, total_length)
    )
    
    # get the current date
    current_date <- Sys.Date()
    
    # loop through all matchdays to get the results
    for (i in 1:number_of_matchdays) {
      ### TO DO: ###
      # Store the correct date for every match, not just for every matchday
      # Matches on a given matchday can be played at friday, saturday or sunday
      # (at least most of the time). Exceptions are possible (postponed games)
      
      # get the URL for the i-th matchday in a given season in a given league
      url <- paste0(root_url, league, "/", season, "/", i)
      
      # read in the html-page
      matchday_results_page <- read_html(url)
      
      # extract the dates for the i-th matchday
      ## (in the future we need html_nodes, because we have multiple dates) ##
      matchday_date <-
        html_node(matchday_results_page, css = ".datum-row") %>%
        html_text()
      
      # extract all the results for every match on the i-th matchday
      matchday_results <-
        html_nodes(matchday_results_page, css = ".detils a") %>%
        html_text()
      
      # clean the data by transforming it into a data frame
      # and extract the results to store it in a more processable way, i.e.,
      # store the result 1:1 into a column home_team_goals = 1 and away_team_goals = 1
      matchday_results_clean <-
        as.data.frame(matrix(matchday_results, ncol = 3,
                             byrow = TRUE))
      
      # pre-allocate the needed data frame
      matches_length <- nrow(matchday_results_clean)
      matchday_results_goals <-
        data.frame(
          "final_goals_home" = rep(NA, matches_length),
          "final_goals_away" = rep(NA, matches_length),
          "halftime_goals_home" = rep(NA, matches_length),
          "halftime_goals_away" = rep(NA, matches_length)
        )
      
      # loop through the number of matches on the given day
      for (j in 1:matches_length) {
        # convert the result, which is given as a final result and a result
        # on halftime, into halftime_goals_home/away and final_goals_home/away
        matchday_results_game <-
          substring(matchday_results_clean$V2[j],
                    seq(1, nchar(matchday_results_clean$V2[j]) -
                          1, 3),
                    seq(3, nchar(matchday_results_clean$V2[j]), 3)) %>%
          t() %>%
          data.frame() %>%
          separate(
            col = "X1",
            into = c("final_goals_home", "final_goals_away"),
            sep = ":"
          ) %>%
          separate(
            col = "X2",
            into = c("halftime_goals_home", "halftime_goals_away"),
            sep = ":"
          )
        
        # store the extracted data into the respective position in the data frame
        matchday_results_goals[j,] <-
          c(
            matchday_results_game$final_goals_home,
            matchday_results_game$final_goals_away,
            matchday_results_game$halftime_goals_home,
            matchday_results_game$halftime_goals_away
          )
        
      }
      
      # clean the data even more by renaming the columns, dropping unneeded ones
      # and bind the data frame for every matchday to it
      matchday_results_clean2 <- matchday_results_clean %>%
        rename(home_team = V1,
               away_team = V3) %>%
        select(-V2) %>%
        cbind(matchday_results_goals)
      
      matchday <- i
      
      # store all extracted data into the data frame (date, matchday and results)
      results_whole_season[((i - 1) * matches_length + 1):(i * matches_length),] <-
        c(matchday_date, matchday, matchday_results_clean2)
      
      # wait for two seconds to follow the rules of Fussballdaten.de
      # regarding the scrape delay
      Sys.sleep(2)
    }
    
    return(results_whole_season)
    
  }

