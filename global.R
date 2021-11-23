# global file should provide the shiny app with the needed
# packages and data 
# for this, we source the setup file to get all the needed functions
# and rdata objects for the shiny app
source("Setup.R")
# set a color scheme
colors <- viridis_pal(option = "D")(30)
# create the seasons
seasons <- c(paste0(sort(c(1995:2021), 
                         decreasing = TRUE),
                    "/", sort(c(1996:2022), 
                              decreasing = TRUE))
)

# map all club names in all available data frames 
# with the mapping function club_name_mapping we created
# to be able to join over the names
all_seasons_running_table$club <- lapply(all_seasons_running_table$club, 
                                         club_name_mapping)

bundesliga_2020_infos$team_name <- lapply(bundesliga_2020_infos$team_name, 
                                         club_name_mapping)

squads_season_2020$club_name <- lapply(squads_season_2020$club_name, 
                                          club_name_mapping)


# combine all data frames together
season_players_joined <- all_seasons_running_table %>%
  inner_join(squads_season_2020,
             by = c("club" = "club_name",
                    "season_start_year" = "season")) %>%
  inner_join(bundesliga_2020_infos,
             by = c("club" = "team_name"))
