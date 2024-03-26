#' Probability density function of Pearson type-3 Distribution
#'
#' @param x quantile/s
#' @param para parameters as c(mu, sigma, gamma) that is c(location, scale, shape).
#'
#' @return Probability density function
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom stats dnorm
#' @importFrom stats dgamma
#'
#' @examples
#' 
#' d <- dpe3(x = 12, para = c(10, 1, 1.5))
#' 
dpe3 <- function(x , para = c(10, 1, 1.5)) {
  e <- para[1]
  a <- para[2]
  k <- para[3]
  x.correct <- !is.na(x)
  if (k == 0) {
    pdf.correct = dnorm((x[x.correct] - e)/a)/a
  } else {
    alpha <- 4/k^2
    tmp <- gamma(alpha)
    beta <- 0.5 * a * abs(k)
    xi <- e - 2 * a/k
    if (k > 0) {
      Y <- x[x.correct] - xi
      pdf.correct = (dgamma((Y)/beta, alpha))/beta
    }  else {
      Y <- xi - x[x.correct]
      pdf.correct = (dgamma((Y)/beta, alpha))/beta
    }
  }
  pdf <- vector()
  pdf.correct[pdf.correct <= 0] <- .Machine$double.eps
  pdf[x.correct] <- pdf.correct
  pdf[is.na(pdf)] <- .Machine$double.eps
  pdf[is.na(x)] <- NA
  return(pdf)
}
