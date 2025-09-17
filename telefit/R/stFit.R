#' Fit the remote effects spatial process (RESP) model
#' 
#' @export
#' 
#' @importFrom fields rdist.earth
#' @useDynLib telefit, .registration = TRUE
#'
#' @param localOnly TRUE to fit the model without the teleconnection effects
#'  (typically for evaluating impact of teleconnection effects)
#' @param remoteOnly TRUE to fit the model without local effects.  This will 
#'  fit a local intercept, but will not incorporate local covariates.
#' @param stData Object with class 'stData' containing data needed to fit this 
#'  model. The data need only be manually entered if not using a stData object.
#' @param X [ns, p, nt] array of design matrices with local covariates
#' @param Y [ns, nt] matrix with response data
#' @param Z [nr, nt] matrix with remote covariates
#' @param coords.s matrix with coordinates where responses were 
#'  observed (lon, lat)
#' @param coords.r matrix with coordinates where remote covariates
#'  were observed (lon, lat)
#' @param coords.knots matrix with coordinates where remote teleconnections
#'  will be based (lon, lat)
#' @param priors A list containing parameters for the prior distributions. The
#'  list needs to contain the following values
#'    \describe{
#'      \item{beta}{ list(Lambda=matrix) specifying the prior covariance matrix
#'        for the local effects if varying==F, otherwise 
#'        list(Psi=matrix, nu=double) specifying the Inverse wishart prior 
#'        distribution for the spatially varying coefficient process if 
#'        varying==T. }
#'      
#'      \item{cov.s}{ list(smoothness=double, range=c(min, max), 
#'        variance=c(shape, rate), nugget=c(shape, rate)) }
#'        
#'      \item{cov.r}{ list(smoothness=double, range=c(min, max), 
#'        variance=c(shape, rate), nugget=c(shape, rate)) }
#'    }
#' @param rw.initsd A list containing initial standard deviation parameters for
#'  the MCMC parameters requiring random walk updates
#'    \describe{
#'      \item{cov.s}{ list(range=double, nugget=double) }
#'      \item{cov.r}{ list(range=double, variance=double, nugget=double) }
#'    }
#' @param maxIt number of iterations to run the MCMC chain for
#' @param returnll TRUE to compute the model log-likelihood at each iteration
#' @param miles TRUE if covariance matrix distances should be in miles, FALSE 
#'  for kilometers
#' @param C scaling factor used in adapting random walk proposal variances.
#' @param alpha target acceptance rate for random walk proposals.
#' @param varying (depreceated) TRUE to fit the model with spatially varying local coefficients
#' 
#' @example examples/stFit.R
#' 


stFit = function( stData = NULL, priors, maxIt, X = stData$X, Y = stData$Y, 
                  Z = stData$Z, coords.s = stData$coords.s, 
                  coords.r = stData$coords.r, rw.initsd = NULL, 
                  returnll = T, miles = T, C=1, alpha=.44, localOnly = F,
                  varying = F, remoteOnly = F, coords.knots ) {
  
  n = dim(X)[1]
  p = dim(X)[2]
  t = dim(X)[3]
  r = nrow(coords.r)
  r_knots = nrow(coords.knots)

  Dy = rdist.earth(coords.s, miles=miles)
  Dz_knots = rdist.earth(coords.knots, miles=miles)
  Dz_to_knots = rdist.earth(coords.r, coords.knots, miles=miles)
  
  # format data
  Yl = matrix(as.numeric(Y), ncol=1)
  
  # format design matrix
  if(remoteOnly) {
    # remoteOnly => intercept only model
    Xl = matrix(1, nrow = nrow(Yl), ncol = 1)
    p = 1
    priors$beta$Lambda = matrix(priors$beta$Lambda[1,1], ncol=1, nrow=1)
  } else {
    Xl = as.matrix(arrayToLong(X, coords.s, 1)[,-(1:3)])
  }
  
  # default random walk proposal standard deviations
  if(is.null(rw.initsd))
    rw.initsd = list(
      cov.s = list(range = .07, nugget = .09),
      cov.r = list(range = .15, variance = .1, nugget = .09)
    )
  
  # fit model
  res = .Call(`r_stpfit`, p, Xl, Z, Yl, priors$beta$Lambda,
              priors$cov.s$variance[1], priors$cov.s$variance[2],
              priors$cov.r$variance[1], priors$cov.r$variance[2],
              priors$cov.s$nugget[1], priors$cov.s$nugget[2],
              priors$cov.s$range[1], priors$cov.s$range[2],
              priors$cov.r$range[1], priors$cov.r$range[2], 
              Dy, Dz_knots, Dz_to_knots, n, r, r_knots, t, 
              priors$cov.s$smoothness, priors$cov.r$smoothness,
              maxIt, errDump, C, alpha, 
              rw.initsd$cov.s$range, rw.initsd$cov.r$range,
              rw.initsd$cov.s$nugget, rw.initsd$cov.r$variance, localOnly,
              priors$cov.r$nugget[1], priors$cov.r$nugget[2], rw.initsd$cov.r$nugget)
  
  
  reslist = list(
    parameters = list(samples = res,
                      beta.names = colnames(X)),
    priors = priors,
    miles = miles,
    localOnly = localOnly,
    remoteOnly = remoteOnly,
    varying = varying,
    coords.knots = coords.knots
  )
  
  class(reslist) = 'stFit'
  
  reslist
}