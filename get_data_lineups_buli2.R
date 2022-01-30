seasons <- 2016
ports <- 7016L
buli2_fixture_lineups_2016_2017 <- NULL
Sys.setlocale("LC_TIME", "C")

for(i in 1:length(seasons)){
  curr_season_lineups <- get_lineups_by_season_tm("2-bundesliga", league_id = "L2",
                                                  season = seasons[i],
                                                  port = ports[i], matchday = 1)
  
  buli2_fixture_lineups_2016_2017 <- bind_rows(
    buli2_fixture_lineups_2016_2017,
    curr_season_lineups
  )
  
  save(buli2_fixture_lineups_2016_2017, file = paste0("Data/buli2_fixture_lineups_2016.RData"))
  
  Sys.sleep(10)
}


seasons <- c(2016)
ports <- c(5002L:5305L)

buli2_fixture_lineups_2016_tm <- NULL

for(i in 1:length(seasons)){
  for(k in 1:1){
    curr_season_lineups <- get_lineups_by_season_tm("2-bundesliga", league_id = "L2",
                                                    season = seasons[i],
                                                    port = ports[i*k],
                                                    matchday = k)
    
    buli2_fixture_lineups_2016_tm <- bind_rows(
      buli2_fixture_lineups_2016_tm,
      curr_season_lineups
    )
    
    save(buli2_fixture_lineups_2016_tm, file = paste0("Data/buli2_fixture_lineups_", seasons[i], 
                                                          "matchday",k, ".RData"))
    
    Sys.sleep(5)
    
  }
  
  Sys.sleep(60)
}
