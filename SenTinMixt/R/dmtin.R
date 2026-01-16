#' Density of a MTIN distribution
#'
#' @param x A data matrix with \code{n} rows and \code{d} columns, being \code{n} the number of data points and \code{d} the data the dimensionality.
#' @param mu A vector of length \code{d} representing the mean value.
#' @param Sigma A symmetric positive-definite matrix representing the scale matrix of the distribution.
#' @param theta A number greater than 0 indicating the tailedness parameter.
#' @param formula Method used to calculate the density: "direct", "indirect", "series".
#'
#' @return
#' The value(s) of the density in x
#' @export
#' @references
#' Punzo A., and Bagnato L. (2021). The multivariate tail-inflated normal distribution and its application in finance.
#' *Journal of Statistical Computation and Simulation*, **91**(1), 1-36.
#'
#' @examples
#' d <- 3
#' x <- matrix(rnorm(d*2), 2, d)
#' dmtin(x, mu = rep(0,d), Sigma = diag(d), theta = 0.9, formula = "direct")
dmtin <- function(x, mu = rep(0, d), Sigma, theta = 0.01, formula = "direct") {
  if (missing(Sigma)) {
    stop("Sigma is missing")
  }
  if (theta <= 0 | theta > 1) {
    stop("theta must be in the interval (0,1)")
  }
  if (is.matrix(Sigma)) {
    d <- ncol(Sigma)
  }
  if (!is.matrix(Sigma)) {
    d <- 1
  }
  if (is.vector(x)) {
    x <- matrix(x, 1, d)
    Sigma <- matrix(Sigma, nrow = d, ncol = d)
  }
  if (formula == "direct") {
    delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
    delta <- replace(delta, delta == 0, 1 / (theta * (2 *
                                                        pi)^(d / 2) * (d / 2 + 1)) * (1 - (1 - theta)^(d / 2 +
                                                                                                         1)))
    pdfgamma <- (2 / delta)^(d / 2 + 1) * (zipfR::Igamma(a = (d / 2 +
                                                                1), x = delta / 2 * (1 - theta), lower = FALSE) -
                                             zipfR::Igamma(a = (d / 2 + 1), x = delta / 2, lower = FALSE))
    pdfconst <- 1 / theta * (2 * pi)^(-d / 2) * det(Sigma)^(-1 / 2)
    PDF <- pdfconst * pdfgamma
  }
  if (formula == "indirect") {
    delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
    intf <- function(w, del) {
      w^(d / 2) * exp(-w / 2 * del)
    }
    pdfinteg <- sapply(1:nrow(x), function(i) {
      stats::integrate(intf,
                       lower = (1 - theta), upper = 1, del = delta[i]
      )$value
    })
    pdfconst <- 1 / theta * (2 * pi)^(-d / 2) * det(Sigma)^(-1 / 2)
    PDF <- pdfconst * pdfinteg
  }
  if (formula == "series") {
    delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
    delta <- replace(delta, delta == 0, 1 / (theta * (2 *
                                                        pi)^(d / 2) * (d / 2 + 1)) * (1 - (1 - theta)^(d / 2 +
                                                                                                         1)))
    n <- d / 2
    term <- sapply(1:length(delta), function(j) {
      -exp(-delta[j] / 2) *
        (delta[j] / 2)^n + exp(-(1 - theta) * delta[j] / 2) *
        ((1 - theta) * delta[j] / 2)^n + sum(sapply(
          1:floor(n),
          function(i) {
            prod(seq(from = n, to = n - i + 1, by = -1)) *
              (delta[j] / 2)^(n - i) * (exp(-(1 - theta) * delta[j] / 2) *
                                          (1 - theta)^(n - i) - exp(-delta[j] / 2))
          }
        ))
    })
    if (d %% 2 == 1) {
      term <- term + sapply(1:length(delta), function(j) {
        prod(seq(
          from = n,
          to = 1.5, by = -1
        )) * sqrt(pi) * (stats::pnorm(sqrt(2 *
                                             delta[j] / 2)) - stats::pnorm(sqrt(2 * (1 - theta) *
                                                                                  delta[j] / 2)))
      })
    }
    PDF <- 1 / theta * (2 * pi)^(-d / 2) * det(Sigma)^(-1 / 2) *
      (2 / delta)^(d / 2 + 1) * term
  }
  return(PDF)
}
