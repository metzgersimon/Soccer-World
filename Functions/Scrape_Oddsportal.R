# ############## get_number_of_teams_in_season #################
# inputs: root_url, league, season
# outputs: should return a list which contains the
# information about the past odd results for every game played in a given season
# for a given sport in a given country in a given league

get_past_odd_results <-
  function(sport, country, league, season, port = 5281L) {
    # create a base url with a given sport, country and league
    url_base <-
      paste0("https://www.oddsportal.com/",
             sport,
             "/",
             country,
             "/",
             league)
    
    # check if the season is 2022 (ongoing) => later more dynamic
    if (season == 2022) {
      # if it is season 2022 just set the season_url to a slash
      season_url <- "/"
    } else {
      # for every other season create a season_url by pasting the years of the
      # season together (of the form 2021/2022; the "-" and the "/" are just for
      # correct url formatting)
      season_url <- paste0("-", (season - 1), "-", season, "/")
    }
    
    # initialize an empty list to store the multiple pages of content for
    # the results of the season
    season_results_list <- list()
    
    # Because we deal with a dynamic web page we need to use a packgage such as
    # RSelenium to scrape the data
    
    # we initialize a driver for the chrome browser with dynamic version
    #port <- netstat::unassigned_ports()[1]
    rD <-
      rsDriver(browser = "chrome",
               port = port,
               chromever = get_stable_chrome_version())
    
    remDr <- rD$client
    
    # initialize a counter variable to iterate through all the pages available
    # for the given season in that endpoint
    page_counter <- 1
    
    while (TRUE) {
      #create dynamic url_end with the page_counter variable
      url_end <- paste0("results/#/page/", page_counter)
      # paste the whole url together
      final_url <- paste0(url_base, season_url, url_end)
      
      # navigate to the dynamic url
      remDr$navigate(final_url)
      
      # extract the html for the given url
      page_html <- read_html(remDr$getPageSource()[[1]])
      
      # extract the table with the dates, teams, results and odds
      table_content <-
        page_html %>% html_nodes("#tournamentTable") %>%
        html_table(trim = TRUE)
      
      # store the current scraped table
      odds_data <- table_content[[1]]
      
      # if there is no more page available there is just one row in odds_data
      # and it contains an error message which says "Unfortunately, no matches..."
      if (nrow(odds_data) == 1) {
        if (startsWith(odds_data[[1]], "Unfortunately")) {
          break
        }
      }
      
      # append the data for the current page to the list which stores
      # all pages
      season_results_list <- append(season_results_list,
                                    list(odds_data))
      
      # increase page_counter by 1
      page_counter <- page_counter + 1
      
      # wait for 10 seconds
      # Otherwise it happened that there is no data stored in odds_data
      Sys.sleep(10)
      
    }
    
    # close the driver (client) and the server
    remDr$close()
    rD$server$stop()
    rm(rD)
    gc()
    
    # it happened in the past that there are duplicates
    # when the Sys.sleep is to small
    #season_results_list <- unique(season_results_list)
    
    return(season_results_list)
    
  }

