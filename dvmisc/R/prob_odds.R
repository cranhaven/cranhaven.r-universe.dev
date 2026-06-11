#' Convert Probability to Odds
#' 
#' Defined simply as \code{x / (1 - x)}.
#' 
#' @param x Numeric vector.
#' 
#' @return Numeric vector.
#'
#' @export
prob_odds <- function(x) {
  out <- x / (1 - x)
  return(out)
}