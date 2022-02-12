# function should fill empty objects with NAs
fill_empty_with_na <- function(frame_to_observe){
  # checks the number of columns of the data frame
  number_columns <- length(colnames(frame_to_observe))
  
  # if there is no data in the data frame
  if(nrow(frame_to_observe) == 0){
    # we fill it with NAs
    filled_frame <- data.frame(t(rep(NA, number_columns)))
    # and set the colnames
    colnames(filled_frame) <- colnames(frame_to_observe)
    
    return(filled_frame)
  }
  
  return(frame_to_observe)
}