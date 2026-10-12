#' Penalty calculator
#'
#' Calcualtes the penalty terms for penalizing mean size and mean difference.
#' This function runs inside \code{sparse_mdc}.
#'
#' @param lambda1 Calculated penalty parameter for mean size.
#' @param lambda2 Calculated penalty parameter for mean difference.
#' @param nk Vector containing the number of samples in each dimension.
#' @param delta Small term to ensure existance of solution, default is
#'  0.0000001.
#' @return a list with two vectors containing the penalty terms for mean size
#' and mean difference respectively.
#' @export
pen_calculator <- function(lambda1, lambda2, nk, delta){
  pen1 <- rep(NA, length(nk))
  pen2 <- rep(NA, length(nk) - 1)
  pen1 <- lambda1 * (nk + delta)
  pen2[] <- lambda2 # Penalty on mean difference |mu_1 - mu_2|
  return(list(pen1 = pen1, pen2= pen2))
}
