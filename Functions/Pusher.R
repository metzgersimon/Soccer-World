sql_pusher <- function (folder = "./Clean Data files/") {
  loadRData <- function(fileName){
    #loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }
  
  con <- dbConnect(RMariaDB::MariaDB(), 
                   host='127.0.0.1',
                   dbname='Soccer_Prediction_Data',
                   username='root',
                   password='my-secret-pw')
  
  file_names = list.files(path = substr(folder, 1, nchar(folder)-1))
  
  for (naming in file_names) {
    interm <- loadRData(paste(folder, naming, sep = ""))
    print(naming)
    dbWriteTable(con,name = naming, interm, overwrite = TRUE )
  }
  
}

