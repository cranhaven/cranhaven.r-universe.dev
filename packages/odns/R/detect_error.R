#' Detects http errors and provides enhanced details.
#' 
#' @param result A http response.
#' 
#' @return invisible
detect_error <- function(result) {
  
  if (httr::http_error(result)) {
    
    stop(glue::glue(
      "API request error with status code {httr::status_code(result)}.\n\n",
      "Attempted method was {result$request$method}\n\n",
      "Attempted URL was \"{result$request$url}\""
    ), call. = FALSE)
    
  }
  return(invisible())
}
