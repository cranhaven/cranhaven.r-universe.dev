#' Simulate responses from the spatio-temporal teleconnection model
#'
#' This function simulates spatio-temporal data. The intention is that data Y and latent 
#' parameters alpha will be generated using provided covariates X and Z; 
#' spatial domains coords.s, coords.r, and coords.knots; and model parameters.
#'
#'
#' @export
#' 
#' @import foreach
#' @importFrom fields rdist.earth
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats rnorm
#' 
#' @param dat.train stData object with training data to simulate new Y values for
#' @param dat.test stData object with test data to simulate new Y values for
#' @param params A list containing model parameters for use in simulation
#'    \describe{
#'      \item{beta}{ vector with fixed effect coefficients }
#'      \item{cov.s}{ list(smoothness=double, range=double, variance=double, nugget=double) }
#'      \item{cov.r}{ list(smoothness=double, range=double, variance=double, nugget=double) }
#'    }
#' @param coords.knots matrix with coordinates of knots for remote covariates
#'  (lon, lat)
#' @param miles TRUE to compute distances for evaluating covariance functions 
#'   in miles.  This is important since the interpretations of the cov.r and 
#'   cov.s parameters depend on the units with which distance is measured.
#' 
#' @example examples/stPredict.R
#' 

stSimulate = function( dat.train, dat.test, coords.knots, params, miles = T ) {
  
  # initialize simulated data objects
  
  dat.sim.train = dat.train
  dat.sim.test = dat.test
  
  
  # extract common parameters
  
  cov.s = params$cov.s
  cov.r = params$cov.r
  
  coords.r = dat.train$coords.r
  coords.s = dat.train$coords.s
  
  n = dim(dat.train$X)[1]
  p = dim(dat.train$X)[2]
  r = nrow(coords.r)
  r_knots = nrow(coords.knots)
  
  
  # build covariance matrices
  
  Dy = rdist.earth(coords.s, miles=miles)
  Dz_knots = rdist.earth(coords.knots, miles=miles)
  Dz_to_knots = rdist.earth(coords.r, coords.knots, miles=miles)
  
  Sigma = maternCov( Dy, scale = cov.s$var, range = cov.s$range, 
                     smoothness = cov.s$smoothness, 
                     nugget = cov.s$var * cov.s$nugget )
  
  Rknots = maternCov( Dz_knots, scale = cov.r$var, range = cov.r$range, 
                      smoothness = cov.r$smoothness, 
                      nugget = cov.r$var * cov.r$nugget )
  
  cknots = maternCov( Dz_to_knots, scale = cov.r$var, range = cov.r$range,
                      smoothness = cov.r$smoothness, 
                      nugget = cov.r$var * cov.r$nugget)
  
  Sigma.chol = chol(Sigma, pivot=T)
  Sigma.chol = t(Sigma.chol[, order(attr(Sigma.chol, 'pivot'))])
  
  
  # simulate latent parameters (and save in output)
  
  params$alphaKnots = rmatnorm(1, Rknots, Sigma)
  params$alpha = cknots %*% solve(Rknots) %*% params$alphaKnots
  
  
  # simulate response data
  
  response.sim = function(dat) {
    nt = dim(dat$X)[3]
    foreach(t = 1:nt, .combine='cbind') %do% {
      Zta = apply(params$alpha, 2, function(a) { t(dat$Z[,t]) %*% a })
      dat$X[,,t] %*% params$beta + Zta + Sigma.chol %*% rnorm(n)
    }
  }
  
  dat.sim.train$Y = response.sim(dat.train)
  dat.sim.test$Y = response.sim(dat.test)
  
  
  # return simulated datasets and true parameters
  list(
    dat.sim.train = dat.sim.train,
    dat.sim.test = dat.sim.test,
    params = params
  )
}