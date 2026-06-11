#' Convert Odds to Probability
#' 
#' Defined simply as \code{log(x / (x + 1))}.
#' 
#' @param x Numeric vector.
#' 
#' @return Numeric vector.

#' @export
odds_prob <- function(x) {
  out <- x / (x + 1)
  return(out)
}