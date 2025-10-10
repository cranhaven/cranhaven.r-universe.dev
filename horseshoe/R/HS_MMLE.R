#' MMLE for the horseshoe prior for the sparse normal means problem.
#'
#' Compute the marginal maximum likelihood estimator (MMLE) of tau for the
#' horseshoe for the normal means problem (i.e. linear regression with the
#' design matrix equal to the identity matrix). The MMLE is explained and
#' studied in Van der Pas et al. (2016).
#'
#' The normal means model is:
#' \deqn{y_i=\beta_i+\epsilon_i, \epsilon_i \sim N(0,\sigma^2)}
#'
#' And the horseshoe prior:
#' \deqn{\beta_j \sim N(0,\sigma^2 \lambda_j^2 \tau^2)}
#' \deqn{\lambda_j \sim Half-Cauchy(0,1).}
#'
#' This function estimates \eqn{\tau}. A plug-in value of \eqn{\sigma^2} is used.
#'
#' @param y The data, a \eqn{n*1} vector.
#' @param Sigma2 The variance of the data.
#' @return The MMLE for the parameter tau of the horseshoe.
#'
#' @note Requires a minimum of 2 observations. May return an error for
#' vectors of length larger than 400 if the truth  is very sparse. In that
#' case, try \code{\link{HS.normal.means}}.
#'
#' @seealso The estimated value of \eqn{\tau} can be plugged into \code{\link{HS.post.mean}}
#' to obtain the posterior mean, and into \code{\link{HS.post.var}} to obtain the posterior
#' variance. These functions are all for empirical Bayes; if a full Bayes version with a hyperprior
#' on \eqn{\tau} is preferred, see \code{\link{HS.normal.means}} for the normal means problem, or
#' \code{\link{horseshoe}} for linear regression.
#'
#' @references van der Pas, S.L., Szabo, B., and van der Vaart, A. (2017), Uncertainty
#' quantification for the horseshoe (with discussion). Bayesian Analysis
#' 12(4), 1221-1274.
#'
#' van der Pas, S.L., Szabo, B., and van der Vaart A. (2017), Adaptive
#' posterior contraction rates for the horseshoe. Electronic Journal of
#' Statistics 10(1), 3196-3225.
#'
#' @examples
#' \dontrun{#Example with 5 signals, rest is noise
#' truth <- c(rep(0, 95), rep(8, 5))
#' y <-  truth + rnorm(100)
#' (tau.hat <- HS.MMLE(y, 1)) #returns estimate of tau
#' plot(y, HS.post.mean(y, tau.hat, 1)) #plot estimates against the data
#'}
#' \dontrun{#Example where the data variance is estimated first
#' truth <- c(rep(0, 950), rep(8, 50))
#' y <-  truth + rnorm(100, mean = 0, sd = sqrt(2))
#' sigma2.hat <- var(y)
#' (tau.hat <- HS.MMLE(y, sigma2.hat)) #returns estimate of tau
#' plot(y, HS.post.mean(y, tau.hat, sigma2.hat)) #plot estimates against the data
#'}
#'
#' @export

HS.MMLE <- function(y, Sigma2){
  stats::optimize(f = MMLE.M, data = y, data.var = Sigma2, lower = (1 / length(y)), upper = 1, maximum = TRUE)$maximum
}
