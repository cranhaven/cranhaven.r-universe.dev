#' Computes variance inflation factors for fixed effects of the teleconnection model
#'
#' VIFs will be computed at the posterior mean of all covariance parameters.
#' 
#' @export
#'
#' @importFrom fields rdist.earth
#' @useDynLib telefit, .registration = TRUE
#'
#' @param burn number of posterior samples to burn before drawing composition
#'  samples
#' @param stFit Object with class 'stFit' containing posterior parameter samples
#'  needed to composition sample the teleconnection effects and generate 
#'  posterior predictions. 
#' @param stData Object with class 'stData' containing data needed to fit this 
#'  model. 
#'  
#' @examples
#' 
#' data("coprecip")
#' data("coprecip.fit")
#' 
#' stVIF(stData = coprecip, stFit = coprecip.fit, burn = 50)
#' 


stVIF = function( stData, stFit, burn ) {
  
  #
  # extract data from inputs
  #
  
  X = stData$X
  Z = stData$Z
  
  nt = ncol(Z)
  
  coords.s = stData$coords.s
  coords.r = stData$coords.r
  coords.knots = stFit$coords.knots
  
  miles = stFit$miles
  
  prior.precision = solve(stFit$priors$beta$Lambda)
  
  
  #
  # spatial covariances
  #
  
  R_knots = maternCov(
    d = rdist.earth(coords.knots, miles=miles),
    scale = mean(stFit$parameters$samples$sigmasq_r[-(1:burn)]),
    range = mean(stFit$parameters$samples$rho_r[-(1:burn)]),
    smoothness = stFit$priors$cov.r$smoothness,
    nugget = 0
  )
  
  R_knots.inv = solve(R_knots)
  
  cknots = maternCov(
    d = rdist.earth(coords.r, coords.knots, miles=miles),
    scale = mean(stFit$parameters$samples$sigmasq_r[-(1:burn)]),
    range = mean(stFit$parameters$samples$rho_r[-(1:burn)]),
    smoothness = stFit$priors$cov.r$smoothness,
    nugget = 0
  )
  
  Sigma = maternCov(
    d = rdist.earth(coords.s, miles=miles),
    scale = mean(stFit$parameters$samples$sigmasq_y[-(1:burn)]),
    range = mean(stFit$parameters$samples$rho_y[-(1:burn)]),
    smoothness = stFit$priors$cov.r$smoothness,
    nugget = mean(stFit$parameters$samples$sigmasq_y[-(1:burn)] *
                    stFit$parameters$samples$sigmasq_eps[-(1:burn)])
  )
  
  SigmaInv = solve(Sigma)
  
  cknotsZ = t(cknots) %*% Z
  C = solve( diag(nt) + t(cknotsZ) %*% R_knots.inv %*% cknotsZ )
  
  
  #
  # compute vifs for betas
  #
  
  # format design matrix
  if(stFit$remoteOnly) {
    Xl = matrix(1, nrow=length(stData$Y), ncol=1)
    beta.names = 'Intercept'
  } else {
    Xl = as.matrix(arrayToLong(X, coords.s, 1)[,-(1:3)])
    beta.names = colnames(stData$X)
  }

  # compute vifs  
  res = diag(solve(prior.precision + t(Xl) %*% dgemkmm(C, SigmaInv, Xl))) /
    diag(solve(prior.precision + t(Xl) %*% dgemkmm(diag(nt), SigmaInv, Xl)))
  
  # label vifs
  names(res) = beta.names
  
  
  
  #
  # compute vifs for alphas
  #
  
  # format variance components
  Zst = R_knots.inv %*% cknotsZ
  alpha.precision = 1/mean(stFit$parameters$samples$sigmasq_r[-(1:burn)])
  
  # compute vifs
  res.alpha = 
    diag(solve(R_knots.inv + Zst %*% t(Zst))) /
    sapply(1:nrow(Zst), function(i) {
      1 / (alpha.precision + t(Zst[i,]) %*% Zst[i,])
    })
  
  
  list(
    beta = res,
    alpha = res.alpha
  )
}