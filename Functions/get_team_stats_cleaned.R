############## get_team_stats_cleaned #################
# inputs: stats_data, total, desired_order
# outputs: returns the team stats in a cleaned way regarding the ordering 
# of the columns and the structure of the data frame
get_team_stats_cleaned <- function(stats_data, total = TRUE, desired_order){
  # get only specific colums
  stats_data_cleaned <- stats_data %>%
    select(contains("home"),
           contains("away"),
           contains("total"),
           # drop the ones contain minute in the name
           -contains("minute")) %>%
    # reorder the column names
    select(order(colnames(.))) 
  
  # if total is true, there is a total column
  if(total){
    # we want to remove the endings _away, _home and _total
    row_names <- str_remove_all(colnames(stats_data_cleaned),
                                pattern = "_away|_home|_total") %>%
      unique()
    
    # and then convert it into a data frame with 3 columns filled by row
    stats_data_cleaned <- stats_data_cleaned %>%
      unlist() %>%
      matrix(ncol = 3,
             byrow = TRUE) %>%
      as.data.frame()
    
    # set the column and the row names accordingly
    colnames(stats_data_cleaned) <- c("away", "home", "total")
    rownames(stats_data_cleaned) <- row_names
    
    # if total is not set to true
  } else {
    # only remove the ones with _away and _home in the name
    row_names <- str_remove_all(colnames(stats_data_cleaned),
                                pattern = "_away|_home") %>%
      unique()
    
    # create a data frame with 2 columns and fill it by row
    stats_data_cleaned <- stats_data_cleaned %>%
      unlist() %>%
      matrix(ncol = 2,
             byrow = TRUE) %>%
      as.data.frame()
    
    # set the column and the row names accordingly
    colnames(stats_data_cleaned) <- c("away", "home")
    rownames(stats_data_cleaned) <- row_names
  }
  
  # in every case we have to transform the variable a bit
  stats_data_cleaned <- stats_data_cleaned %>%
    # make every column numeric
    mutate(across(everything(),
                  as.numeric)) %>%
    # convert the row names as a new column
    rownames_to_column() %>%
    # and order this column based on the desired order given via parameter
    mutate(rowname = factor(rowname, levels = desired_order)) %>%
    # reorder the rows
    arrange(rowname)
  
  # set the row names to the first column
  rownames(stats_data_cleaned) <- stats_data_cleaned[, 1]
  
  # and then drop the first column
  stats_data_cleaned <- stats_data_cleaned %>%
    select(-1)
 
  
  return(stats_data_cleaned)
}