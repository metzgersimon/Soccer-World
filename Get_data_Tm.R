
season <- 2021
matchday <- 21

newest_matchday_info <- get_fixture_detailed_info(league = "bundesliga",
                                                  league_id = "L1",
                                                  matchday = matchday,
                                                  port = 2222L)



seasons <- c(2015:2019)
ports <- c(2040L:2050L)

fixture_info_tm_2010_2021 <- NULL

for(i in 1:length(seasons)){
  print(paste0("Current season: ", seasons[i]))
  curr_infos <- get_fixture_detailed_info(league = "bundesliga",
                                          league_id = "L1",
                                          season = seasons[i],
                                          port = ports[i])
  
  fixture_info_tm_2010_2021 <- bind_rows(fixture_info_tm_2010_2021,
                                         curr_infos)
  
  Sys.sleep(5)
  
  
}
