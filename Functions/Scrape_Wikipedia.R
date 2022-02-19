### get_venue_coordinates ###
# function should return the venue information combined with 
# the geospatial data (longitude and latitude) of these venues
get_venue_coordinates <- function(){
  # get the venue information for all teams with seasons newer than 2015
  # and drop the "3. Liga"
  full_addresses <- venues_with_coordinates %>%
    filter(season >= 2015,
           league_id != 80)
  
  # get an random port 
  port <- randomPort()

  # get the unique venue names
  venue_names <- unique(full_addresses$venue_name)
  
  # rename a lot of venues such that they can be entered in wikipedia
  # and the correct venue is directly found 
  # otherwise we do not land on the wikipedia page itself but on a page 
  # that contains different proposals to match the query
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
          venue_names == "Jahnstadion Regensburg",
          "Jahnstadion Regensburg (2015)",
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
                      venue_names == "Swansea.com Stadium",
                      "Liberty Stadium",
                      ifelse(
                        venue_names == "The American Express Community Stadium",
                        "Falmer Stadium",
                        ifelse(venue_names == "The MKM Stadium",
                               "MKM Stadium",
                               venue_names))))))))))))
                      
                                                        
  
  # create a driver from Rselenium without a browser windows
  rD <- rsDriver(browser = "firefox", extraCapabilities = list(
    "moz:firefoxOptions" = list(
      args = list('--headless'))),
    port = port)
  
  # get the client
  remDr <- rD$client
  
  # set time outs to give the page the change to first fully load before
  # we try to get information form it
  remDr$setTimeout(type = "implicit", milliseconds = 50000)
  remDr$setTimeout(type = "script", milliseconds = 50000)
  remDr$setTimeout(type = "page load", milliseconds = 500000)
  
  # use the external function set_implicit_wait
  set_implicit_wait(remDr, milliseconds = 50000)
  
  # create an empty variable to store the coordinates of all clubs
  all_club_coordinates <- NULL
  
  # iterate over all venues in the data base
  for(i in 1:length(venue_names)){
   
    # set the base url
    base_url <- "https://www.wikipedia.org/"
    
    # navigate to the created url
    remDr$navigate(base_url)
    
    # store the webpage
    page_html <- read_html(remDr$getPageSource()[[1]])
    
    
    Sys.sleep(5)
    
    # look for the search input field
    search_field <- remDr$findElement(using = "xpath",
                                      paste0("//div[@id='search-input']//input"))
    
    # click it
    search_field$clickElement()

    # send^the current venue name to the input field and hit enter
    search_field$sendKeysToElement(list(venue_names[i], key = "enter"))
    
    # wait for 10 seconds to prevent the page from breaking down
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
  closeAllConnections()
  
  
  return(all_club_coordinates)
}
