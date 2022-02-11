get_venue_coordinates <- function(port = 9741L){
  full_addresses <- major_six_leagues_team_infos_2010_2021 %>%
    filter(season >= 2015,
           league_id != 80)
  # full_addresses <- venues_with_coordinates %>%
  #   filter(!str_detect(longitude, pattern = "[0-9]+\\.[0-9]+"),
  #          !str_detect(latitude, pattern = "[0-9]+\\.[0-9]+"))
  
  # get the unique venue names
  venue_names <- unique(full_addresses$venue_name)
  
  venue_names <- ifelse(
    venue_names == "Mercedes-Benz-Arena",
    "Mercedes-Benz-Arena (Stuttgart)",
    ifelse(
      venue_names == "Stamford Bridge",
      "Stamford Bridge (Stadion)",
      ifelse(
        venue_names == "Etihad Stadium",
        "Etihad Stadium (Manchester)",
        ifelse(
          venue_names == "Abanca-Balaídos",
          "Estadio Balaídos",
          ifelse(
            venue_names == "Allianz Stadium",
            "Juventus Stadium",
            ifelse(
              venue_names == "Estadio Abanca-Riazor",
              "Estadio Riazor",
              ifelse(
                venue_names == "Estadio de Mendizorroza",
                "Estadio Mendizorrotza",
                ifelse(
                  venue_names == "Estadio de Mestalla",
                  "Estadio Mestalla",
                  ifelse(
                    venue_names == "Estadio de Vallecas",
                    "Campo de Fútbol de Vallecas",
                    ifelse(
                      venue_names == "Estadio Municipal José Zorrilla",
                      "Nuevo Estadio Municipal José Zorrilla",
                      ifelse(
                        venue_names == "Estadio Wanda Metropolitano",
                        "Wanda Metropolitano",
                        ifelse(
                          venue_names == "Estadio Nuevo Mirandilla",
                          "Estadio Ramón de Carranza",
                          ifelse(
                            venue_names == "Jahnstadion Regensburg",
                            "Jahnstadion Regensburg (2015)",
                            ifelse(
                              venue_names == "MAPEI Stadium - Città del Tricolore",
                              "Mapei Stadium – Città del Tricolore",
                              ifelse(
                                venue_names == "Red Bull Arena",
                                "Red Bull Arena (Leipzig)",
                                ifelse(
                                  venue_names == "Stade Auguste-Delaune II",
                                  "Stade Auguste-Delaune",
                                  ifelse(
                                    venue_names == "Stade Crédit Agricole de la Licorne",
                                    "Stade de la Licorne",
                                    ifelse(
                                      venue_names == "Stade de la Beaujoire - Louis Fonteneau",
                                      "Stade Louis-Fonteneau",
                                      ifelse(
                                        venue_names == "Stade Yves Allainmat - Le Moustoir",
                                        "Stade du Moustoir",
                                        ifelse(
                                          venue_names == "Stadio Adriatico-Giovanni Cornacchia",
                                          "Stadio Adriatico – Giovanni Cornacchia",
                                          ifelse(
                                            venue_names == "Stadio Artemio Franchi",
                                            "Stadio Artemio Franchi (Florenz)",
                                            ifelse(
                                              venue_names == "Stadio Comunale Luigi Ferraris",
                                              "Stadio Luigi Ferraris",
                                              ifelse(
                                                venue_names == "Stadio Comunale Via del Mare",
                                                "Stadio Via del Mare",
                                                ifelse(
                                                  venue_names == "Stadio Marc'Antonio Bentegodi",
                                                  "Stadio Marcantonio Bentegodi",
                                                  ifelse(
                                                    venue_names == "Swansea.com Stadium",
                                                    "Liberty Stadium",
                                                    ifelse(
                                                      venue_names == "The American Express Community Stadium",
                                                      "Falmer Stadium",
                                                      ifelse(
                                                        venue_names == "The MKM Stadium",
                                                        "MKM Stadium",
                                                        ifelse(
                                                          venue_names == "Visit Mallorca Estadi",
                                                          "Estadi de Son Moix",
                                                          venue_names
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  
  # create a driver from Rselenium
  rD <- rsDriver(browser = "firefox", port = port)
  
  # get the client
  remDr <- rD$client
  
  # set time outs to give the page the change to first fully load before
  # we try to get information form it
  remDr$setTimeout(type = "implicit", milliseconds = 50000)
  
  # create an empty variable to store the coordinates of all clubs
  all_club_coordinates <- NULL
  
  # empty flag variable
  # has_dash <- FALSE
  dash_counter <- 0
  
  # iterate over all venues in the data base
  for(i in 1:length(venue_names)){
    print(paste0("Current iteration: ", i))
    print(paste0("Current venue name: ", venue_names[i]))
   
    base_url <- "https://www.wikipedia.org/"
    
    # navigate to the created url
    remDr$navigate(base_url)
    
    # store the webpage
    page_html <- read_html(remDr$getPageSource()[[1]])
    
    # check if the string contains a dash
    # if(str_detect(venue_names[i], pattern = "-")){
    #   has_dash <- TRUE
    # }
    dash_counter <- str_count(venue_names[i], pattern = "-")
    
    # split the venue name into pieces because the function needs it that way
    # first, replace "-" with a space
    modified_name <- str_replace_all(venue_names[i], pattern = "-",
                            replacement = " ")
    
    # then split the strings by the space
    modified_name <- str_split(modified_name, pattern = " ") %>%
      unlist()
    
    # cut the name into pieces and add spaces
    final_vector <- c(rep(NA, length(modified_name)))
    
    # word counter
    word_counter <- 1
    # iterate over the modified_name vector and add after every string that
    # is not the last a space
    # for(curr_string in 1:length(modified_name)){
    #   if(curr_string != length(modified_name)){
    #     while(word_counter <= dash_counter){
    #       final_vector[curr_string] <- paste0(modified_name[curr_string], "-")
    #     }
    #     
    #     final_vector[curr_string] <- paste0(modified_name[curr_string], " ")
    #     
    #   } else {
    #     final_vector[curr_string] <- modified_name[curr_string]
    #   }
    # }
    
    # deal with the case that there is just one string
    # then we need to add another empty string to be able to use
    # the sendkeystoelement function
    # if(length(final_vector) == 1){
    #   final_vector[2] <- ""
    # }
    
    Sys.sleep(5)
    
    # search the search input field
    search_field <- remDr$findElement(using = "xpath",
                                      paste0("//div[@id='search-input']//input"))
    
    # click it
    search_field$clickElement()
    # write into the field the string we dismembered above
    # search_field$sendKeysToElement(final_vector)
    search_field$sendKeysToElement(list(venue_names[i], key = "enter"))
    # hit the enter button to browse this string
    # search_field$sendKeysToElement(list(key = "enter"))
    
    Sys.sleep(10)
    
    # store the web page
    page_html <- read_html(remDr$getPageSource()[[1]])
    
    # get the coordinates
    coordinates_link <- page_html %>%
      html_nodes(xpath = paste0("//span[@id='coordinates']//a")) %>%
      html_attr("href") %>%
      .[1]
    
    Sys.sleep(3)
    
    # navigate to the created url
    remDr$navigate(coordinates_link)
    
    # store the web page
    page_html <- read_html(remDr$getPageSource()[[1]])
    
    # extract the actual coordinates
    # latitude
    latitude <- page_html %>%
      html_nodes(xpath = "//span[@class='geo']//span[@class='latitude']") %>%
      html_text(trim = TRUE)
    
    # longitude
    longitude <- page_html %>%
      html_nodes(xpath = "//span[@class='geo']//span[@class='longitude']") %>%
      html_text(trim = TRUE)
    
    # bind together the coordinates
    curr_coordinates <- c(venue_names[i], longitude, latitude) %>%
      matrix(ncol = 3, byrow = TRUE) %>%
      data.frame()
    
    # set the colnames
    colnames(curr_coordinates) <- c("venue_name", "longitude", "latitude")
    
    # paste them together 
    all_club_coordinates <- bind_rows(
      all_club_coordinates,
      curr_coordinates
    )
    
    if(i %% 10 == 0){
      save(all_club_coordinates, file = paste0("all_club_coordinates_", i, ".RData"))
    }
    
    Sys.sleep(2)

  }
  
  # finally join the extracted coordinates with the other venue information
  all_club_coordinates <- inner_join(full_addresses,
                                     all_club_coordinates,
                                     by = c("venue_name")) %>%
    # convert the coordinates to numerics
    mutate(longitude = as.numeric(longitude),
           latitude = as.numeric(latitude))
  
  # close the driver (client) and the server
  remDr$close()
  rD$server$stop()
  rm(rD)
  gc()
  
  
  return(all_club_coordinates)
}
