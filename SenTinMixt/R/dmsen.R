#' Density of a MSEN distribution
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
#' Punzo A., and Bagnato L. (2020). Allometric analysis using the multivariate shifted exponential normal distribution.
#' *Biometrical Journal*, **62**(6), 1525-1543.
#'
#' @examples
#' d <- 3
#' x <- matrix(rnorm(d*2), 2, d)
#' dmsen(x, mu = rep(0,d), Sigma = diag(d), theta = 0.4, formula = "direct")
dmsen <- function(x, mu = rep(0, d), Sigma, theta = Inf, formula = "direct") {
  if (missing(Sigma)) {
    stop("Sigma is missing")
  }
  if (theta < 0) {
    stop("theta must be greater than, or equal to, 0")
  }
  if (is.matrix(Sigma)) {
    d <- ncol(Sigma)
  }
  if (!is.matrix(Sigma)) {
    d <- 1
  }
  if (is.vector(x)) {
    x <- matrix(x, length(x), 1)
    Sigma <- matrix(Sigma, nrow = d, ncol = d)
  }
  if (formula == "direct") {
    delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
    delta <- replace(delta, delta == 0, 1 / (theta * (2 *
                                                        pi)^(d / 2) * (d / 2 + 1)) * (1 - (1 - theta)^(d / 2 +
                                                                                                         1)))
    pdfgamma <- expint::gammainc(a = (d / 2 + 1), x = 1 / 2 *
                                   delta + theta) * (1 / 2 * delta + theta)^(-(d / 2 +
                                                                                 1))
    pdfconst <- (2 * pi)^(-d / 2) * theta * exp(theta) * det(Sigma)^(-1 / 2)
    PDF <- pdfconst * pdfgamma
  }
  if (formula == "indirect") {
    delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
    intf <- function(w, gamm) {
      w^(d / 2) * exp(-w * gamm)
    }
    pdfinteg <- sapply(1:nrow(x), function(i) {
      stats::integrate(intf,
                       lower = 1, upper = Inf, gamm = delta[i] / 2 + theta
      )$value
    })
    pdfconst <- (2 * pi)^(-d / 2) * theta * exp(theta) * det(Sigma)^(-1 / 2)
    PDF <- pdfconst * pdfinteg
  }
  if (formula == "series") {
    delta <- sapply(1:nrow(x), function(i) t(as.vector(t(x[i, ]) - mu)) %*% solve(Sigma) %*% as.vector(t(x[i, ]) - mu))
    delta <- replace(delta, delta == 0, 1 / (theta * (2 *
                                                        pi)^(d / 2) * (d / 2 + 1)) * (1 - (1 - theta)^(d / 2 +
                                                                                                         1)))
    n <- d / 2
    term <- sapply(1:length(delta), function(j) {
      exp(-delta[j] / 2 -
            theta) * (delta[j] / 2 + theta)^(-1) * (1 + sum(sapply(
              1:floor(n),
              function(i) {
                prod(seq(from = n, to = n - i + 1, by = -1)) *
                  (delta[j] / 2 + theta)^(-i)
              }
            )))
    })
    if (d %% 2 == 1) {
      term <- term + sapply(1:length(delta), function(j) {
        prod(seq(
          from = n,
          to = 0.5, by = -1
        )) * sqrt(pi) * 2 * 1 / (delta[j] / 2 +
                                   theta)^(floor(n) + 1 + 1 / 2) * (1 - stats::pnorm(sqrt(2) *
                                                                                       sqrt(delta[j] / 2 + theta)))
      })
    }
    PDF <- (2 * pi)^(-d / 2) * det(Sigma)^(-1 / 2) * theta *
      exp(theta) * term
  }
  return(PDF)
}
