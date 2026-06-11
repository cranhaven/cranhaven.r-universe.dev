#' Convert Logit to Probability
#' 
#' Defined as: \code{exp_x <- exp(x); out <- exp_x / (1 + exp_x)}. This 2-step 
#' approach is faster than \code{exp(x) / (1 + exp(x))} because the exponentials 
#' only have to be calculated once.
#' 
#' @param x Numeric vector.
#' 
#' @return Numeric vector.
#' 
#' @export
logit_prob <- function(x) {
  exp_x <- exp(x)
  out <- exp_x / (1 + exp_x)
  return(out)
}