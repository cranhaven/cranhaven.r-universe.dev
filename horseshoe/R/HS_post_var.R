#' Posterior variance for the horseshoe for the normal means problem.
#'
#' Compute the posterior variance for the horseshoe for the normal means problem
#'  (i.e. linear regression with the design matrix equal to the identity matrix),
#'   for a fixed value of tau, without using MCMC. Details on computation are given
#' in Carvalho et al. (2010) and Van der Pas et al. (2014).
#'
#' The normal means model is:
#' \deqn{y_i=\beta_i+\epsilon_i, \epsilon_i \sim N(0,\sigma^2)}
#'
#' And the horseshoe prior:
#' \deqn{\beta_j \sim N(0,\sigma^2 \lambda_j^2 \tau^2)}
#' \deqn{\lambda_j \sim Half-Cauchy(0,1).}
#'
#' If \eqn{\tau} and \eqn{\sigma^2} are known, the posterior variance can be computed without
#' using MCMC.
#'
#' @param y The data. An \eqn{n*1} vector.
#' @param tau Value for tau. Tau should be greater than 1/450.
#' @param Sigma2 The variance of the data.
#' @return The posterior variance for each of the datapoints.
#'
#' @references Carvalho, C. M., Polson, N. G., and Scott, J. G. (2010), The horseshoe
#'  estimator for sparse signals. Biometrika 97(2), 465–480.
#'
#'  van der Pas, S. L., Kleijn, B. J. K., and van der Vaart, A. W. (2014), The horseshoe
#'   estimator: Posterior concentration around nearly black vectors. Electronic
#'    Journal of Statistics 8(2), 2585–2618.
#'
#'@seealso \code{\link{HS.post.mean}} to compute the posterior mean. See
#'  \code{\link{HS.normal.means}} for an implementation that does use MCMC, and
#'  returns credible intervals as well as the posterior mean (and other quantities).
#' See \code{\link{horseshoe}} for linear regression.
#'
#' @examples
#'
#'#Plot the posterior variance for a range of deterministic values
#' y <- seq(-8, 8, 0.05)
#' plot(y, HS.post.var(y, tau = 0.05, Sigma2 = 1))
#'
#' #Example with 20 signals, rest is noise
#' #Posterior variance for the signals is plotted in blue
#' #Posterior variance for the noise is plotted in black
#' truth <- c(rep(0, 80), rep(8, 20))
#' data <-  truth + rnorm(100)
#' tau.example <- HS.MMLE(data, 1)
#' plot(data, HS.post.var(data, tau.example, 1),
#'  col = c(rep("black", 80), rep("blue", 20)) )
#'
#' @export


HS.post.var <- function(y, tau, Sigma2){
  var <- Sigma2*HS.post.mean(y, tau, Sigma2)/y - (HS.post.mean(y, tau, Sigma2) - y)^2 + y^2*Basic.y.vec(y, tau, k = 2, Sigma2)/Basic.y.vec(y, tau, k = 0, Sigma2)
  return(var)
}



