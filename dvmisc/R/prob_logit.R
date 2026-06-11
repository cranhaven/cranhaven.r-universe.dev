#' Convert Probability to Logit
#' 
#' Defined simply as \code{log(x / (1 - x))}.
#' 
#' @param x Numeric vector.
#' 
#' @return Numeric vector.
#'
#' @export
prob_logit <- function(x) {
  out <- log(x / (1 - x))
  return(out)
}