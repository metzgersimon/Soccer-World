get_next_fixture_by_team <- function(team_name, league_id, season){
  curr_date <- Sys.Date()
  
  next_fixture <- fixtures_bundesliga_2010_2021 %>%
    mutate(fixture_date = ymd(fixture_date)) %>%
    filter(club_name_home == team_name |
             club_name_away == team_name,
           league_season == season,
           league_id == league_id) %>%
    mutate(diff_time = as.numeric(difftime(fixture_date, curr_date))) %>%
    filter(diff_time > 0) %>%
    filter(diff_time == min(diff_time)) %>%
    select(-diff_time)
  
  return(next_fixture)
}

get_last_fixtures_by_team <- function(team_name, league_id, season,
                                      last_n){
  curr_date <- Sys.Date()
  
  last_five_fixtures <- fixtures_bundesliga_2010_2021 %>%
    mutate(fixture_date = ymd(fixture_date)) %>%
    filter(club_name_home == team_name |
             club_name_away == team_name,
           league_season == season,
           league_id == league_id) %>%
    mutate(diff_time = as.numeric(difftime(fixture_date, curr_date))) %>%
    filter(diff_time < 0) %>%
    arrange(desc(diff_time)) %>%
    top_n(last_n) %>%
    select(-diff_time)
  
  return(last_five_fixtures)
}
