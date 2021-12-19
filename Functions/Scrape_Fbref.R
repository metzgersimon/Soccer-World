# function returns a vector of all urls which represents a season
# for a given league
get_season_urls_for_league <- function(league, league_id){
  # create the url endpoint for the given league history
  url <- paste0("https://fbref.com/en/comps/", 
                # str_to_title makes the first letter uppercase
                league_id, "/history/", str_to_title(league),
                "-Seasons")
  
  # get the html of the url
  page_html <- read_html(url)
  
  # extract the urls for various season for a given league
  season_urls <- page_html %>%
    html_nodes(css = "th a") %>%
    # get only the links for the different seasons
    html_attr("href") %>%
    # paste the fbref.com in front of the actual url to be able to use it 
    # later on as full url
    paste0("https://fbref.com", .) %>%
    # remove stats to be more adaptable
    str_remove_all("Stats")
  
  # return urls
  return(season_urls)
}

get_squad_standard_stats <- function(season_url){
  # build the final url
  url <- paste0(season_url, "Stats")
  # get the html of the url
  page_html <- read_html(season_url)
  
  # extract the standard squad stats
  standard_squad_stats <- page_html %>%
    # set the css to the table
    html_nodes(css = "#stats_squads_standard_for") %>%
    # convert the table into a data frame
    html_table() %>%
    # extract the data frame from a listelement that contains a data frame
    # to a plain data frame
    data.frame()
  
  # set the columnnames of the data frame to the first row of the frame
  # (because of misparsed data)
  colnames(standard_squad_stats) <- standard_squad_stats[1,]
  
  # drop the first row after we set the colnames
  standard_squad_stats <- standard_squad_stats[-1,]
  
  
  # extract the standard squad stats against a club
  standard_squad_stats_against <- page_html %>%
    # set the css to the table
    html_nodes(css = "#stats_squads_standard_against") %>%
    # convert the table into a data frame
    html_table() %>%
    # extract the data frame from a listelement that contains a data frame
    # to a plain data frame
    data.frame()
  
  # set the columnnames of the data frame to the first row of the frame
  # (because of misparsed data)
  colnames(standard_squad_stats_against) <- standard_squad_stats_against[1,]
  
  # drop the first row after we set the colnames
  standard_squad_stats_against <- standard_squad_stats_against[-1,]
  
  
  # combine the two data frames into a list of data frames
  # containing one data frame for the "normal" stats and one
  # for the stats against a club
  standard_squad_combined <- list(standard_squad_stats,
                                  standard_squad_stats_against)
  
  # return the list of data frames
  return(standard_squad_combined)
}

get_player_standard_stats <- function(season_url){
  # build the final url
  url <- paste0(season_url, "Stats")
  # get the html of the url
  page_html <- read_html(season_url)
  
  # extract the standard squad stats
  player_standard_stats <- page_html %>%
    # set the css to the table
    html_nodes(css = "#stats_standard") %>%
    # convert the table into a data frame
    html_table() %>%
    # extract the data frame from a listelement that contains a data frame
    # to a plain data frame
    data.frame()
}


