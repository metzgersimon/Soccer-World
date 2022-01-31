max_date <- fifa_team_stats_buli_2015_2021 %>% 
  filter(date == max(date)) %>% 
  select(date) %>% 
  pull() %>% 
  unique()

max_fifa <- fifa_team_stats_buli_2015_2021 %>%
  filter(fifa_vers == max(fifa_vers)) %>% 
  select(fifa_vers) %>% 
  pull() %>% 
  unique()

########## get newest team stats ############
fifa_team_stats_new <- get_team_stats_full_fifa(fifa_version = max_fifa,
                                                league = "bundesliga",
                                                max_date_in_database = max_date)

if(!is.null(fifa_team_stats_new)){
  dbWriteTable(con, "fifa_team_stats_buli_2015_2022", fifa_team_stats_new,
               append = TRUE)
}

########## get newest squad stats ############
fifa_squad_stats_new <- get_squads_full_fifa(league_id = 19,
                                             fifa_version = max_fifa,
                                             port = 7777L,
                                             max_date_in_database = max_date)

if(!is.null(fifa_squad_stats_new)){
  dbWriteTable(con, "fifa_squads_buli_2015_2022", fifa_squad_stats_new,
               append = TRUE)
}
