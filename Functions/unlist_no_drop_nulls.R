############## unlist_no_drop_nulls #################
# inputs: element_to_unlist
# outputs: returns all the elements in the element_to_unlist list
# but not excluding the NULL elements as a normal unlist would do
unlist_no_drop_nulls <- function(element_to_unlist, list_element = "player"){
  # create an empty variable to store the elements
  all_elements <- NULL
  
  for(i in 1:length(element_to_unlist)){
    # extract the current element
    current_element <- element_to_unlist[[i]]
    
    # convert the NULL elements into NA elements, leave the other ones
    # as they are
    extracted_curr_element <- current_element[[list_element]]
    
    # get the element names of the list
    list_names <- names(extracted_curr_element)
    
    extracted_curr_element[sapply(extracted_curr_element, is.null)] <- NA
    
    # then use the normal unlist function to unlist the data
    current_element <- unlist(extracted_curr_element)
    
    # set the names accordingly
    names(current_element) <- paste0(list_element, "_", names(current_element))
    
    # store all elements in the variable created above
    all_elements <- c(all_elements,
                      current_element)
  }
  
  return(all_elements)
  
}
