get_league_best_scorers_by_season <- function(region_id, country, league, 
                                              league_id, season_id, stage_id){
  url <- paste0("https://www.whoscored.com/Regions/", region_id,
               "/Tournaments/", league_id, "/Seasons/", season_id,
               "/Stages/", stage_id, "/PlayerStatistics/",
               str_to_title(country), "-", str_to_title(league))
  
  rD <- rsDriver(browser = "chrome", port = 2643L, chromever = get_stable_chrome_version())
  remDr <- rD$client
  
  remDr$navigate(url)
  
  # extract the html for the given url
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  # table_content <- page_html %>%
  #   html_nodes(css = "#BestEleven") %>%
  #   html_text(trim = TRUE)
  #
  
  total_pages <- page_html %>%
    html_nodes(xpath = "//div[@id='statistics-paging-summary']/div") %>%
    html_text() %>%
    str_match("[0-9]+/[0-9]+") %>%
    str_split(pattern = "/") %>%
    .[[1]] %>%
    .[-1] %>%
    as.numeric()
  
    total_pages <- total_pages[[1]]
  
  for(i in 1:total_pages){
    player_stats <- page_html %>%
      html_nodes(xpath = "//table[@id='top-player-stats-summary-grid']/body") %>%
      html_text()
    elem <- remDr$findElement(using = "xpath", "//dd/a[@id='next']")
    elem$clickElement()
  }
    
  
  
  # table_content <-  %>%
  #   html_table() %>%
  #   .[[1]]
  # 
  # 
  # test <- str_split(table_content, "\\*")
  
  
}