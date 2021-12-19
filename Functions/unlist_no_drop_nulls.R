############## unlist_no_drop_nulls #################
# inputs: element_to_unlist
# outputs: returns all the elements in the element_to_unlist list
# but not excluding the NULL elements as a normal unlist would do
unlist_no_drop_nulls <- function(element_to_unlist){
  # create an empty variable to store the elements
  all_elements <- NULL
  
  # get the element names of the list
  list_names <- names(element_to_unlist)
  
  for(i in 1:length(element_to_unlist)){
    # extract the current element
    current_element <- element_to_unlist[[i]]
    
    # convert the NULL elements into NA elements, leave the other ones
    # as they are
    current_element[sapply(current_element, is.null)] <- NA
    
    # then use the normal unlist function to unlist the data
    current_element <- unlist(current_element)
    
    # set the names accordingly
    names(current_element) <- paste0(list_names[[i]], "_", names(current_element))
    
    # store all elements in the variable created above
    all_elements <- c(all_elements,
                      current_element)
  }
  
  return(all_elements)
  
}