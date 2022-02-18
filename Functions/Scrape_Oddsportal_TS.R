# ############## get_past_odds_ts #################
# inputs: sport, country, league, season, port
# outputs: should return a frame with matchups and quote time series

get_past_odds_ts <- 
  function(sport, country, league, season, port = 5281L) {
  
#===============================================================================
  # find all the links to the matchups of a given season
  
  # general result table
  ress <- data.frame()
  
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
  
  # we initialize a driver for the chrome browser with dynamic version
  rD <-
    rsDriver(browser = "firefox",  port)#extraCapabilities = list(
      # "moz:firefoxOptions" = list(
        # args = list('--headless'))),
      # port = port)
  
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
                                  remDr$getPageSource()[[1]])
    
    # increase page_counter by 1
    page_counter <- page_counter + 1
    
    # wait for 10 seconds
    # Otherwise it happened that there is no data stored in odds_data
    Sys.sleep(5)
    
  }
  
  test <- season_results_list
  
#===============================================================================
  
  # list to fill with links to matchups
  final <- list()
  
  # this loop looks for the links to the respective matchups
  for (k in 1:length(test)) {
  
    xml <- read_html(test[[k]])
    
    link <- xml_find_all(xml, '//*[@id="tournamentTable"]/tbody/tr')
    
    # if there is no error the href attr will be appended to final
    for (i in 1:length(link)){
      skip_to_next <- FALSE
      tryCatch(
        xml_attrs(xml_child(xml_child(link[[i]], 2), 1))[["href"]],
        error = function(e) { skip_to_next <<- TRUE})
      
      if(skip_to_next)
        { next } 
      else
        final <- append(final,
                        xml_attrs(xml_child(xml_child(link[[i]], 2), 1))[["href"]])
    }
  
  }
  
#===============================================================================
  # next: work with the matchup links from final
  
  ex <-  unlist(final, use.names = FALSE)
  
  for(j in 1:length(ex)){ #(ll in ex) {
    
    # navigate to matchup page
    print(paste("https://www.oddsportal.com", ex[j], sep = ''))
    Sys.sleep(1)
    remDr$navigate(paste("https://www.oddsportal.com", ex[j], sep = ''))
    
    # extract opponents and datetime of beginning
    beginning <- unlist(remDr$findElement(using='xpath','//*[@id="col-content"]/p[1]')$getElementText(), use.names = FALSE)
    print(beginning)
    opponents <- unlist(remDr$findElement(using='xpath','//*[@id="col-content"]/h1')$getElementText(), use.names = FALSE)
    print(opponents)
    more <- unlist(remDr$findElement(using='xpath', '//*[@id="odds-data-table"]/div[1]/table/tfoot/tr[1]/td/a/strong')$getElementText(), use.names = FALSE)
    
    
    # this snippet finds the relevant bookies on the matchpage
    b <- remDr$findElements(using = 'class', 'name')
    bookmaker <- NULL
    
    r <- 1
    while(TRUE) {
      
      check <- b[[r]]$getElementText()[[1]]
      
      if(check == "Pinnacle"){
        bookmaker <- check
        break
      } else {
        r <- r + 1
        next
      }
    }
    
  }
    
    # this is the final vector containing the bookmakers
    # bookies <- unlist(bookies, use.names = FALSE)
    
#===============================================================================
    # the next part concerns the context menus with timestamps and odds
    
    # find all the odds elements
    # elements <- remDr$findElements(using = "class", "right odds")
    elements <- remDr$findElements(using = "xpath", 
                                   paste0("//div[@id='odds-data-table']//",
                                   "div[@class='table-container']//table[@class='table-main detail-odds sortable'][1]",
                                          "//td[contains(@class, 'right odds')]"))
    nele <- list()
    
    # get the row with the pinnacle bookmaker
    pinnacle_row <- c((((r - 1) * 3) + 1):(((r - 1) * 3) + 3))
    
    for (i in pinnacle_row[1]:pinnacle_row[3]) {
      
      element <- elements[i]
      # hover over each element
      locati <- element[[1]]
      remDr$mouseMoveToLocation(webElement = locati)
      
      
      # //*[@id="tooltipdiv"] location of the odds were changed on the site
      # extract the additional tooltip information popping up
      n_element <- remDr$findElement(using = "xpath", '//*[@id="tooltipdiv"]')
      
      nele <- append(nele, n_element$getElementAttribute('innerHTML')[[1]])
      
    }
    
#===============================================================================
  # next: extract the odds from the nele list 
    
    otc <- c("1", "x", "2")
    
    print(length(nele))
    subs <- data.frame()
    
    # create a data frame with the info that is currently available
    match_odds_frame <- data.frame(opponents, beginning, bookmaker)
    
    # iterate over all odds for the pinnacle bookmaker
    for (t in 1:(length(nele))) {
      
      oddslist <- strsplit(nele[[t]], split = "<.*?>")
      # extract the odds of the hoverinfo and unlist them
      oddslist_elements <- oddslist[[1]] %>%
        .[. != ""] %>%
        trimws() %>%
        .[. != ""]
      
      # take only the first (last odds before the match) and the opening odds
      important_odds <- oddslist_elements %>%
        .[c(1:2, (length(.) - 1): length(.))] %>%
        unique()

      
      # assign the type of odds (1 means home team, 2 means draw,..)
      outcome <- otc[(1+(t-1)%%3)]
      bookmaker <- bookies
      
      if (is.na(bookmaker)) break
      
      # check if there is not only one odd for closing and opening available
      if (length(important_odds) %% 2 == 0) {
        date_end <- important_odds[1]
        odd_end <- important_odds[2]
        date_start <- important_odds[3]
        odd_start <- important_odds[4]
        
        if(t == 1){
          colnames_curr <- c("home_win_date_start", "home_win_odd_start",
                             "home_win_date_end", "home_win_odd_end")
        } else if(t == 2){
          colnames_curr <- c("draw_date_start", "draw_odd_start",
                             "draw_date_end", "draw_odd_end")
        } else {
          colnames_curr <- c("away_win_date_start", "away_win_odd_start",
                             "away_win_date_end", "away_win_odd_end")
        }
        
        
        # create a data frame for the current info (for the current outcome)
        curr_frame <- data.frame(date_start, odd_start, date_end, odd_end)
        
        # set the colnames
        colnames(curr_frame) <- colnames_curr
        
        # combine the data
        match_odds_frame <- cbind(match_odds_frame, curr_frame)
      }
      
      else {
        # extract the elements
        date <- important_odds[2]
        odd <- important_odds[3]
        
        if(t == 1){
          colnames_curr <- c("home_win_date_start", "home_win_odd_start",
                             "home_win_date_end", "home_win_odd_end")
        } else if(t == 2){
          colnames_curr <- c("draw_date_start", "draw_odd_start",
                             "draw_date_end", "draw_odd_end")
        } else {
          colnames_curr <- c("away_win_date_start", "away_win_odd_start",
                             "away_win_date_end", "away_win_odd_end")
        }
        
        
        # create a data frame for the current info (for the current outcome)
        curr_frame <- data.frame(date, odd, date, odd)

        # set the colnames
        colnames(curr_frame) <- colnames_curr
        
        # combine the data
        match_odds_frame <- cbind(match_odds_frame, curr_frame)
        
      }
      
    }
    
    # bind the info from the current match to the overall results frame
    ress <- rbind(ress, match_odds_frame)
    
  }
  
  #===============================================================================
  # close the driver (client) and the server
  remDr$close()
  rD$server$stop()
  rm(rD)
  gc()

  write.csv(ress, paste("Data/oddsdata/bundesliga2", season, ".csv"))
  
  Sys.sleep(30)
  
}


