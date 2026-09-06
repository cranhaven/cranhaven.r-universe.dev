#' Generate a symmetric Q matrix
#'
#' Creates a K x K symmetric rate matrix (Q matrix) with equal transition rates between states.
#' The diagonal elements are set such that each row sums to zero.
#'
#' @param K Integer. The number of states.
#'
#' @return A K x K numeric matrix representing the symmetric Q matrix.
#' @examples
#' symmetric.Q.matrix(4)
#' @export
symmetric.Q.matrix <- function(K) {

  # Create an empty matrix of size K x K
  Q <- matrix(0, nrow = K, ncol = K)

  # transition rate for each state
  transition.rate <- 1 / (K)

  for (i in 1:K) {
    for (j in 1:K) {
      if (i != j) {
        Q[i, j] = transition.rate
      }
    }
    Q[i, i] = -(transition.rate*(K-1))
  }
  return(Q)
}


#' Get discrete gamma rates
#'
#' Computes a set of discrete gamma rates for rate variation across sites or characters.
#' This function is adapted from the \code{phangorn} package.
#'
#' @param alpha Numeric. The shape parameter of the gamma distribution.
#' @param k Integer. The number of rate categories.
#'
#' @return Numeric vector of length k representing the discrete gamma rates.
#' @examples
#' get_gamma_rates(alpha = 0.5, k = 4)
#' @export
get_gamma_rates <- function(alpha, k) {
  if (k == 1) return(1)
  # Compute quantiles to divide the gamma distribution into k categories
  quants <- qgamma((1:(k - 1)) / k, shape = alpha, rate = alpha)
  diff(c(0, pgamma(quants * alpha, alpha + 1), 1)) * k
}


#' Get discrete log-normal rates
#'
#' Computes a set of discrete log-normal rates for rate variation across sites or characters.
#' The rates are normalized so that the mean rate equals 1.
#'
#' @param meanlog Numeric. Mean on the log scale.
#' @param sdlog Numeric. Standard deviation on the log scale.
#' @param k Integer. Number of rate categories.
#'
#' @return Numeric vector of length k representing the discrete log-normal rates.
#' @examples
#' get_lognormal_rates(meanlog = 0, sdlog = 1, k = 4)
#' @export
get_lognormal_rates <- function(meanlog, sdlog, k) {
  if (k == 1) return(1)
  quants <- qlnorm((1:(k - 1)) / k, meanlog = meanlog, sdlog = sdlog)
  probs <- diff(c(0, plnorm(quants, meanlog = meanlog, sdlog = sdlog), 1))
  # normalize to k categories so mean rate = 1
  rates <- qlnorm(((1:k) - 0.5) / k, meanlog = meanlog, sdlog = sdlog)
  rates / weighted.mean(rates, probs)
}
