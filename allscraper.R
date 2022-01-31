############## get_all_available_leagues_by_country #################
# inputs: none
# outputs: none
# functionality: appends most recent data to specified SQL databease

source("Setup.R")

# need a function for season and matchday
current_season <- 2021
newest_matchday <- 18

con <- dbConnect(RMariaDB::MariaDB(), 
                 host='127.0.0.1',
                 dbname='testdb',
                 username='root',
                 password='my-secret-pw')

# Fetch Team_ids from this season and league
current_team_id_q <- dbSendQuery(conn = con, paste(
                                              "SELECT DISTINCT club_id_home 
                                              FROM buli_matches_2010_2021
                                              WHERE league_season =",
                                              as.character(current_season) 
                                              ))

current_team_ids <- dbFetch(current_team_id_q)
dbClearResult(current_team_id_q)


# Fetch fixtur ids from this matchday in season and league
current_fixtures_id_q <- dbSendQuery(conn = con,  paste(
                                              "SELECT DISTINCT club_id_home 
                                              FROM buli_matches_2010_2021
                                              WHERE league_season =",
                                              as.character(current_season),  
                                              "AND league_round = ",
                                              as.character(newest_matchday)))

current_fixtures_ids <- dbFetch(current_fixtures_id_q)
dbClearResult(current_fixtures_id_q)



#===============================================================================
# Football API

team_stats_in_league <- get_team_stats_in_league_by_season()

get_fixtures_in_league_by_season()

get_fixture_stats()

get_player_stats_fixture()


#===============================================================================
# Current Odds

#===============================================================================
# fivethirtyeight

#===============================================================================
# Scrape_Sofifa
# Probleme: Timeouts

get_team_stats_full

get_squads_full_by_season

#===============================================================================
# Scrape_Transfermarkt
# Probleme: Timeouts

get_market_value_over_time()

get_squad_club_urls()

get_squads_by_season()

get_lineups_by_season()

get_fixture_detailed_info()

#===============================================================================
# Scrape_Transfermarkt

get_match_dates()

get_matchdate_table_understat()

#===============================================================================
# Scrape kicker



#buli_stats_kicker <- tbl(con, "buli_stats_kicker_2013_to_2021") %>%
# filter(league_season == current_season) %>%
#  data.frame()

# check for the latest available matchday
#latest_matchday <- buli_stats_kicker %>%
#  filter(season_start_year == current_season) %>%
#  filter(matchday == max(matchday)) %>%
#  select(matchday) %>%
#  pull() %>%
#  unique()

# get the newest matchday


# new matchday stats
# name of the sql table: 
buli_newest_matchday_stats <- get_fixture_stats_kicker(league = "bundesliga",
                                                  season = current_season,
                                                  matchday = newest_matchday)

# implement when fixed for newest data
buli_all_rest_days <- get_rest_days_kicker(league_name = "bundesliga",
                                         season = current_season,
                                         port = 5476L)



# implement when fixed for newest data
# get_team_stats_full()






con <- dbConnect(RMariaDB::MariaDB(),
                 host='127.0.0.1',
                 dbname='testdb',
                 username='root',
                 password='my-secret-pw')

dbWriteTable(con, name = "buli_rest_days", all_rest_days,
             overwrite = TRUE)

dbWriteTable(con, name = "buli_newest_matchday_stats", buli_newest_matchday_stats,
             overwrite = TRUE)


dbDisconnect(con)