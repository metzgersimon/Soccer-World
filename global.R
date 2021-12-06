# global file should provide the shiny app with the needed
# packages and data 
# for this, we source the setup file to get all the needed functions
# and rdata objects for the shiny app
source("Setup.R")
# set a color scheme
colors <- viridis_pal(option = "D")(30)
# create the seasons
seasons <- c(paste0(sort(c(2015:2021), 
                         decreasing = TRUE),
                    "/", sort(c(2016:2022), 
                              decreasing = TRUE))
)

# map all club names in all available data frames 
# with the mapping function club_name_mapping we created
# to be able to join over the names
all_seasons_running_table$club <- sapply(all_seasons_running_table$club, 
                                         club_name_mapping)

bundesliga_2020_infos$team_name <- sapply(bundesliga_2020_infos$team_name, 
                                         club_name_mapping)

squads_season_2020$club_name <- sapply(squads_season_2020$club_name, 
                                          club_name_mapping)

fixtures_bundesliga_2010_2021$club_name_home <- 
  sapply(fixtures_bundesliga_2010_2021$club_name_home,
         club_name_mapping)

fixtures_bundesliga_2010_2021$club_name_away <- 
  sapply(fixtures_bundesliga_2010_2021$club_name_away,
         club_name_mapping)

all_seasons_from_2010_squads$club_name <- 
  sapply(all_seasons_from_2010_squads$club_name,
         club_name_mapping)

# buli_2021_first90_fixture_stats$team_name <-
#   sapply(buli_2021_first90_fixture_stats$team_name,
#          club_name_mapping)

squads_by_season_2010_2021$club_name <- 
  sapply(squads_by_season_2010_2021$club_name,
         club_name_mapping)


# combine all data frames together
season_players_joined <- all_seasons_running_table %>%
  inner_join(squads_season_2020,
             by = c("club" = "club_name",
                    "season_start_year" = "season")) %>%
  inner_join(bundesliga_2020_infos,
             by = c("club" = "team_name"))

# fixtures_with_stats 2021
fixtures_with_stats_2021 <- buli_2021_first100_fixture_stats  %>%
  left_join(fixtures_bundesliga_2010_2021, by = "fixture_id")

fixtures_with_stats_2021$team_name <- sapply(fixtures_with_stats_2021$team_name,
                                             club_name_mapping)

fixtures_with_stats_2021$club_name_home <- sapply(fixtures_with_stats_2021$club_name_home,
                                             club_name_mapping)

fixtures_with_stats_2021$club_name_away <- sapply(fixtures_with_stats_2021$club_name_away,
                                             club_name_mapping)


