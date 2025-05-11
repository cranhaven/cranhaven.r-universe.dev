validate_order_columns <- function(data, cols){
  # First verify no NAs.
  cols <- unlist(cols)
  if(any(is.na(cols))) stop("Columns for ordering has at least one NA")
  # If character, verify that all elements are column names.
  if(is.character(cols)){
    bad_names <- cols[which(!(cols %in% colnames(data)))]
    if(length(bad_names) > 0)
      stop("Ordering columns not found in data: ",
           paste(bad_names, collapse = ", "))
    
  } else {
    # If numeric, verify that all values are within the correct interval.
    if(is.numeric(cols)){
      cols <- as.integer(cols)
      if(any(cols <= 0, cols > ncol(data)))
        stop("Invalid ordering column indices specified")
    } else {
      stop("Invalid column specification passed to ordering argument")
    }
  }
}
