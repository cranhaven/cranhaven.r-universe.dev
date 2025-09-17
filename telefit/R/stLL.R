#' Compute log likelihood for model
#'
#'
#' @export
#' 
#' @importFrom fields rdist.earth
#' 
#' @useDynLib telefit, .registration = TRUE
#' 
#' 
#' @param stFit Object with class 'stFit' containing posterior parameter samples
#'  needed to composition sample the teleconnection effects and generate 
#'  posterior predictions. The data needed from stFit need only be manually 
#'  entered if not using a stData object.
#' @param stData Object with class 'stData' containing data needed to fit this 
#'  model. The data need only be manually entered if not using a stData object.
#' @param X [ns, p, nt] array of design matrices with local covariates
#' @param Y [ns, nt] matrix with response data
#' @param Z [nr, nt] matrix with remote covariates
#' @param coords.s matrix with coordinates where responses were 
#'  observed (lon, lat)
#' @param coords.r matrix with coordinates where remote covariates
#'  were observed (lon, lat)
#' @param coords.knots matrix with coordinates of knots for remote covariates
#'  (lon, lat)
#' @param miles TRUE if distances should be computed in miles (kilometers otherwise)
#' @param beta values of \eqn{\beta} at which to evaluate the likelihood
#' @param sigmasq_y values of \eqn{\sigma^2_w} at which to evaluate the
#'  likelihood
#' @param sigmasq_r values of \eqn{\sigma^2_\alpha} at which to evaluate the
#'  likelihood
#' @param sigmasq_eps values of \eqn{\sigma^2_\varepsilon} at which to evaluate 
#' the likelihood
#' @param sigmasq_r_eps values of \eqn{\sigma^2_{\alpha_\varepsilon}} at which 
#' to evaluate the likelihood
#' @param rho_y values of \eqn{\rho_w} at which to evaluate  the likelihood
#' @param rho_r values of \eqn{\rho_\alpha} at which to evaluate the likelihood
#' 
#' @example examples/stLL.R
  
stLL = function( stData, stFit, beta, sigmasq_y, sigmasq_r, sigmasq_eps, rho_y, 
                 rho_r, X = stData$X, Y = stData$Y, Z = stData$Z, 
                 coords.s = stData$coords.s, coords.r = stData$coords.r,
                 coords.knots = stFit$coords.knots, miles=TRUE, sigmasq_r_eps) {
  
  n = nrow(coords.s)
  r = nrow(coords.r)
  r_knots = nrow(coords.knots)
  p = dim(X)[2]
  t = dim(X)[3]
  
  Dy = rdist.earth(coords.s, miles=miles)
  Dz_knots = rdist.earth(coords.knots, miles=miles)
  Dz_to_knots = rdist.earth(coords.r, coords.knots, miles=miles)
  
  Z = as.matrix(Z)
  
  # format data
  Yl = matrix(as.numeric(Y), ncol=1)
  
  # format design matrix
  Xl = as.matrix(arrayToLong(X, coords.s, 1)[,-(1:3)])
  
  .Call(`r_ll`, matrix(Xl, ncol=p), Z, Yl, 
        Dy, Dz_knots, Dz_to_knots, p, n, r, r_knots, t, 
        stFit$priors$cov.s$smoothness, stFit$priors$cov.r$smoothness,
        matrix(beta, ncol=p), sigmasq_y, sigmasq_r, sigmasq_eps, rho_y, rho_r,
        sigmasq_r_eps)
}