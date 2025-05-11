validate_named_list <- function(named_list, valid_names = NULL){
  input_name <- deparse1(substitute(named_list))
  if(!is.list(named_list)){
    stop(input_name, " must be a named list")
  } else {
    if(length(named_list) > 0){
      element_names <- names(named_list)
      if(is.null(element_names) || any(nchar(element_names) == 0))
        stop("All elements of ", input_name, " must be named")
      if(!is.null(valid_names)){
        bad_names <- element_names[which(!(element_names %in% valid_names))]
        if(length(bad_names) > 0)
          stop(input_name, " has the following bad names: ",
               paste(bad_names, collapse = ", "))
      }
    }
  }
}
