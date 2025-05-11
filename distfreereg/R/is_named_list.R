is_named_list <- function(named_list){
  if(!is.list(named_list)){
    return(FALSE)
  } else {
    if(length(named_list) > 0){
      element_names <- names(named_list)
      if(is.null(element_names) || any(nchar(element_names) == 0)){
        return(FALSE)
      } else {
        return(TRUE)
      }
    } else {
      # If empty list, return TRUE
      return(TRUE)
    }
  }
}
