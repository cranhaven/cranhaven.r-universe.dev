#' Produces error if input exceeds 2000 characters.
#' 
#' @description Used to ensure constructed URLs do not exceed 2000 characters.
#' 
#' @param x a character string to check.
#' 
#' @return invisible.
cap_url <- function(x) {
  
  stopifnot(
    "url exceeds 2000 characters, please try a simpler query." = nchar(as.character(x)) <= 2000
  )
  
  return(invisible())
}
