sql_pusher <- function (folder = "./Clean Data files/") {
  # this function solves the problem of loading and renaming .RData objects
  # source: https://stackoverflow.com/questions/5577221/how-can-i-load-an-object
  # -into-a-variable-name-that-i-specify-from-an-r-data-file
  # Posted by user: https://stackoverflow.com/users/1453172/ricardo
  get_data <- function(name){
    load(name)
    get(ls()[ls() != "name"])
  }
  con <- dbConnect(RMariaDB::MariaDB(), 
                   host='127.0.0.1',
                   dbname='Soccer_Prediction_Data',
                   username='root',
                   password='my-secret-pw')
  
  file_names <- list.files(path = substr(folder, 1, nchar(folder)-1))
  old <- dbListTables(con)
  file_names <- setdiff(file_names, paste(old, ".RData", sep = ""))
  
  if (length(file_names) >= 1) {
    
    for (naming in file_names) {
      interm <- get_data(paste(folder, naming, sep = ""))
      naming <- substr(folder, 1, nchar(folder)-5)
      print(naming)
      dbWriteTable(con,name = naming, interm, overwrite = TRUE )
    }
  } else
    print("no changes to push")
  
}

