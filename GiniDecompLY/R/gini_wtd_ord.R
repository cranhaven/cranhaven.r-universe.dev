#' Gini index calculation (option of enforcing order)
#'
#' This function calculates the Gini index for a distribution, with an option of sorting according to another distribution.
#' @param x a numeric vector containing at least non-negative elements.
#' @param ord a numeric vector containing the distribution to sort with.
#' @param weights an optional vector of weights of x to be used in the computation of the Gini coefficient. Should be NULL or a numeric vector.
#' @return The value of the Gini index.
#' @noRd
#'

gini_wtd_ord <- function (x, ord = NULL, weights = NULL)
{
  if (is.null(weights)) {
    weights <- rep(1, length(x))
  }
  missing <- !(is.na(x) | is.na(weights))
  x <- x[missing]
  weights <- weights[missing]
  if (!all(weights >= 0))
    stop("At least one weight is negative", call. = FALSE)
  if (all(weights == 0))
    stop("All weights are zero", call. = FALSE)
  weights <- weights/sum(weights)

  if (is.null(ord)) {
    ord <- x
  }
  order <- order(ord)
  x <- x[order]
  weights <- weights[order]
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu/nu[n]
  gini <- sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
  return(gini)
}
