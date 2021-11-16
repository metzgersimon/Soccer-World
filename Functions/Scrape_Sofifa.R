############## get_team_stats_full #################
# inputs
# outputs: should return a data frame which contains the
# information about team-based data from sofifa.com
# example: includes the FIFA game rating for each club and some other information
# such as the number of players in the club

get_team_stats_full <- function() {
  # create the url endpoint for the first three german leagues
  # with the information we want selected
  base_url <-
    paste0(
      "https://sofifa.com/teams?type=all&lg%5B0%5D=19&lg%5B1%5D=20",
      "&lg%5B2%5D=2076&showCol%5B%5D=oa&showCol%5B%5D=at&",
      "showCol%5B%5D=md&showCol%5B%5D=df&showCol%5B%5D=dp&",
      "showCol%5B%5D=ip&showCol%5B%5D=ps&showCol%5B%5D=sa&",
      "showCol%5B%5D=ta"
    )
  
  # get the fifa versions of the game (back until 2010)
  fifa_versions <- read_html(base_url) %>%
    html_nodes(css = "a.bp3-menu-item") %>%
    html_text() %>%
    # make all values lower case
    tolower() %>%
    # get all elements which contain the string fifa
    # all the FIFA versions look like this: FIFA XX where XX stands for a
    # version, e.g., 22
    .[str_detect(., pattern = "fifa")] %>%
    # extract the numbers
    str_extract(pattern = "[0-9]+") %>%
    # transform them into a numeric value
    as.numeric() %>%
    .[1:13]
  
  # initialize the empty frame
  club_stats_frame <- NULL
  
  # iterate through all extracted fifa versions
  for (i in 1:length(fifa_versions)) {
    # create the url endpoint for the current fifa version
    curr_fifa_url <-
      paste0(base_url, "&r=", fifa_versions[i], "0001",
             "&set=true")
    
    # because seasons usually start in summer of year 1 and last until
    # autumn, there are two values for the year, year 1 and year 2
    # here, we want to see if the year is two digits long, if not we append
    # a 0 which is necessary to get the correct url
    fifa_version_cur <-
      ifelse(nchar(trunc(abs(fifa_versions[i]))) == 2,
             fifa_versions[i],
             paste0("0", fifa_versions[i]))
    
    fifa_version_prev <-
      ifelse(nchar(trunc(abs(
        fifa_versions[i] - 1
      ))) == 2,
      fifa_versions[i] - 1, paste0("0", (fifa_versions[i] - 1)))
    
    # extract all selectable dates for a given fifa game
    available_dates <- read_html(curr_fifa_url) %>%
      html_nodes(css = "a.bp3-menu-item") %>%
      html_text() %>%
      # extract only those elements which contains 4 digits for the year
      .[str_detect(.,
                   pattern = paste0("20", fifa_version_cur, "|20",
                                    fifa_version_prev, ""))]
    
    # get the urls for the dates extracted above
    url_for_date <- read_html(curr_fifa_url) %>%
      html_nodes(css = "a.bp3-menu-item") %>%
      # extract all hyperlinks
      html_attr("href") %>%
      # get only those which end with r= followed by the fifa version and
      # &set=true. $ marks the end of the string
      .[str_detect(., pattern = paste0("r=", fifa_versions[i], ".*&set=true$"))] %>%
      unique() %>%
      # to be able to use the url, we paste the sofifa.com beginning to it
      paste0("https://sofifa.com", .)
    
    # iterate through all extracted date urls
    for (j in 1:length(url_for_date)) {
      # now extract the table of all clubs for each date (in each fifa version)
      club_stats <- read_html(url_for_date[j]) %>%
        html_nodes(xpath = "//table") %>%
        html_table() %>%
        data.frame() %>%
        # drop misparsed column for the logo
        select(-`Var.1`) %>%
        # split the Name column into club and league by removing the word German
        separate(col = Name,
                 into = c("club", "league"),
                 sep = " German") %>%
        # add variables for the date and the fifa version
        # and remove the uncessesary parantheses in the league column
        mutate(
          date = available_dates[j],
          fifa_vers = fifa_versions[i],
          league = trimws(str_remove_all(league, pattern = "\\(.*\\)"))
        ) %>%
        mutate(league = ifelse(
          str_detect(league, pattern = "1."),
          "Bundesliga",
          ifelse(str_detect(league, pattern = "2."),
                 league, "3. Liga")
        ))
      
      # append the newly extracted rows to the current data frame
      club_stats_frame <- bind_rows(club_stats_frame, club_stats)
      
      # because it is good practise, we wait for 1 second before we
      # go to the next iteration
      Sys.sleep(1)
    }
    
  }
  
  # return the data frame
  return(club_stats_frame)
  
}

