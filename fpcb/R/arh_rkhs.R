#' Autoregressive Hilbertian Model using RKHS
#' 
#' Estimates an autoregresive Hilbertian model of order 1 for functional time
#' series. The temporal dependence is estimated in the Hilbert projection space
#' which has a reproducing kernel as proposed in Hernández et al (2021) <arXiv:2105.13627> and
#' Wang et al (2020) <arXiv:2011.13993>.
#' 
#' 
#' @param fdata an fdata object containing the functional objects and the
#' lambda coefficients of the d dimensional RKHS representation.
#' @return \item{fdata}{smoothed curves.} \item{lambda_cent}{centered
#' coefficients of the d dimensional RKHS representation.}
#' \item{lambda_ce}{average coefficients of the d dimensional RKHS
#' representation.} \item{rho}{autocorrelation operator computed as:
#' \eqn{Gamma_0}\eqn{Psi} = \eqn{Gamma_1}. \eqn{Gamma_0} correspond to the
#' Covariance and \eqn{Gamma_0} correspond to the Cross-Covariance (of lag 1)
#' operators, both estimated using the coefficients \eqn{lambda}.}
#' @author N. Hernández and J. Cugliari
#' @export
#' @references N. Hernández, J. Cugliari, J. Jacques. Simultaneous Predictive
#' Bands for Functional Time Series using Minimum Entropy Sets. arXiv:2105.13627 (2021).
#' D. Wang, Z. Zhao, R. Willett, C. Y. Yau, Functional
#' autoregressive processes in reproducing kernel hilbert spaces, arXiv
#' preprint arXiv:2011.13993 (2020).
arh_rkhs <-
function(fdata) { # this will be called as the 'model'
  n <- nrow(fdata$fdata)
  lambda.me   <- colMeans(fdata$lambda)
  lambda.cent <- sweep(fdata$lambda, 2, lambda.me)
  
  # operators
  Covariance = t(lambda.cent) %*% (lambda.cent) / n
  Cross.Cov  = t(lambda.cent[2:n,]) %*% (lambda.cent[1:(n-1),]) / (n - 1)
  rho = Cross.Cov %*% invgen(Covariance)
  return(list(fdata = fdata,
              lambda_cent = lambda.cent,
              lambda_me   = lambda.me,
              rho = rho))
}
