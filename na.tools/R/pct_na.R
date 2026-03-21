#' Counts how many values are NA
#' 
#' Returns the number of values that are NA
#' 
#' @param x object to count how many values are `NA`
#' 
#' @details 
#' 
#' `n_na` counts the number of missing values. `na.n` is an alias in the dplyr
#' style.
#' 
#' `pct_na` gives the percentage of values that are `NA`
#' 
#' @return 
#' 
#' `n_na` returns an integer. `pct_na` returns a numeric value 0-1. 
#' 
#' @examples
#'   x <- c( 1, NA, NA, 4:5 )
#'   n_na(x)
#'   pct_na(x)
#' 
#' @export
 
n_na <- function(x) 
  sum( is.na(x) )  

#' @rdname n_na
#' @export

na.howmany <- n_na 

#' @rdname n_na 
#' @export
na.n <- n_na



#' @rdname n_na
#' @export
pct_na <- function(x) 
  n_na(x) / length(x)

#' @rdname n_na
#' @export

na.pct <- pct_na
