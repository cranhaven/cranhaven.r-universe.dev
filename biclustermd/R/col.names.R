#' A generic to gather column names
#' 
#' @param x an object to retrieve column names from 
#' 
#' @export
#' 
col.names <- function(x) {
  UseMethod("col.names")
}

