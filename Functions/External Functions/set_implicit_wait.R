# function is not from us but was found on:
# https://github.com/ropensci/RSelenium/issues/212
# Posted by: TheVandalyst
# Posted on: 2019-10-03
set_implicit_wait <- function(remDr, milliseconds){
  qpath <- sprintf("%s/session/%s/timeouts", remDr$serverURL,
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, method = "POST", qdata = toJSON(list(type = "implicit", ms = milliseconds), 
                                                       auto_unbox = TRUE))
}  


