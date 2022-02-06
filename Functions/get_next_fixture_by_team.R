############## get_next_fixture_by_team #################
# inputs: team_name, league_id, season
# outputs: returns for a given team in a given season the next match

get_next_fixture_by_team <- function(team_name, league_id, season){
  # save the current date
  curr_date <- Sys.Date()
  
  # get information for the next match
  next_fixture <- fixtures_bundesliga_2010_2021 %>%
    # convert the date into ymd
    mutate(fixture_date = ymd(fixture_date)) %>%
    # filter for the team name and the league_id
    # and the season
    filter(club_name_home == team_name |
             club_name_away == team_name,
           league_season == season,
           league_id == league_id) %>%
    # compute the difference in dates from the current date
    # to all matches
    mutate(diff_time = as.numeric(difftime(fixture_date, curr_date))) %>%
    # take only those where the diff_time is > 0, i.e., it is a match
    # in the future
    filter(diff_time > 0) %>%
    # take the match with the smallest diff (it is closest to today)
    filter(diff_time == min(diff_time,na.rm = TRUE)) %>%
    # drop the diff_time column from the data frame
    select(-diff_time)
  
  # return data frame for the next match
  return(next_fixture)
}



############## get_last_fixtures_by_team #################
# inputs: team_name, league_id, season, last_n
# outputs: returns for a given team in a given season the n last match
# where n is given by the parameter last_n

get_last_fixtures_by_team <- function(team_name, league_id, season,
                                      last_n){
  # save the current date
  curr_date <- Sys.Date()
  
  # get information for the last_n matches
  last_n_fixtures <- fixtures_bundesliga_2010_2021 %>%
    # convert the date into ymd
    mutate(fixture_date = ymd(fixture_date)) %>%
    # filter for the team name and the league_id
    # and the season
    filter(club_name_home == team_name |
             club_name_away == team_name,
           league_season == season,
           league_id == league_id) %>%
    # compute the difference in dates from the current date
    # to all matches
    mutate(diff_time = as.numeric(difftime(fixture_date, curr_date))) %>%
    # take only those where the diff_time is < 0, i.e., it is a match
    # in the past
    filter(diff_time < 0) %>%
    # order the data frame based on diff_time in descending order, i.e.,
    # the last match on top
    arrange(desc(diff_time)) %>%
    # get the first last_n elements
    top_n(last_n) %>%
    # drop the diff_time column from the data frame
    select(-diff_time)
  
  # return the last_n fixtures of the selected team
  return(last_n_fixtures)
}
