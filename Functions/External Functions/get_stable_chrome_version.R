get_stable_chrome_version <- function(){
  # Get installed stable Google Chrome version ...
  if (xfun::is_unix()) {
    
    chrome_driver_version <-
      system2(command = ifelse(xfun::is_macos(),
                               "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
                               "google-chrome-stable"),
              args = "--version",
              stdout = TRUE,
              stderr = TRUE) %>%
      stringr::str_extract(pattern = "(?<=Chrome )(\\d+\\.){3}")
    
    ## on Windows a plattform-specific bug prevents us from calling the Google Chrome binary directly to get its version number
    ## cf. https://bugs.chromium.org/p/chromium/issues/detail?id=158372
  } else if (xfun::is_windows()) {
    
    chrome_driver_version <-
      system2(command = "wmic",
              args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
              stdout = TRUE,
              stderr = TRUE) %>%
      stringr::str_extract(pattern = "(?<=Version=)(\\d+\\.){3}")
    
  } else rlang::abort(message = "Your OS couldn't be determined (Linux, macOS, Windows) or is not supported.")
  
  # ... and determine most recent ChromeDriver version matching it
  chrome_driver_version %<>%
    magrittr::extract(!is.na(.)) %>%
    stringr::str_replace_all(pattern = "\\.",
                             replacement = "\\\\.") %>%
    paste0("^",  .) %>%
    stringr::str_subset(string =
                          binman::list_versions(appname = "chromedriver") %>%
                          dplyr::last()) %>%
    as.numeric_version() %>%
    max() %>%
    as.character()
  
  return(chrome_driver_version)
}

