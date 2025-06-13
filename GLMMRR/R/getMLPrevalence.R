#' Compute Estimated Population Prevalence
#'
#'
#' @param mu
#' observed mean response.
#' @param n
#' number of units.
#' @param c
#' randomized response parameter c.
#' @param d
#' randomized response parameter d.
#'
#' @return
#' maximum likelihood estimate of the population prevalence and its variance.
#' @export
getMLPrevalence <- function(mu, n, c, d)
{
  pi <- (mu - c) / d
  var.estimate <- ((c + d * pi) * (1 - (c + d * pi))) / (d^2 * n)

  return(list("ML estimate" = pi, "variance" = var.estimate))
}
