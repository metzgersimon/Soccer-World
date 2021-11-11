# Function returns an access token which is needed in order to be able 
# to retrieve data from the ElenaSportAPI
get_elena_access_token <- function(){
  # execute a POST request with the API key to retrieve the access token
  post_request <- POST("https://oauth2.elenasport.io/oauth2/token",
                       add_headers(.headers = c('Content-Type' = 'application/x-www-form-urlencoded',
                                                'Authorization' = elenasport_api_key)),
                       body = 'grant_type=client_credentials')
  
  # extract the access token from the request
  access_token <- content(post_request)$access_token
  
  # return the access token
  return(access_token)
}