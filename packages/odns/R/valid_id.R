#' Basic check of whether the characters in a string is equal to 36
#' 
#' @param x A character string.
#' 
#' @return logical value indicating whether the string checked consists of 36
#'  characters.
valid_id <- function(x) {
  
  return(nchar(as.character(x)) == 36)
  
}
