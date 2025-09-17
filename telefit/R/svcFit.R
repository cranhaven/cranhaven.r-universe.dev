#' Fit a spatially varying coefficient model
#'
#'
#' @export
#'
#' @importFrom fields rdist.earth
#' @useDynLib telefit, .registration = TRUE
#'
#' @param y vector containing responses for each timepoint.  vector is blocked
#'   by timepoint.
#' @param X matrix containing local covariates for each timepoint.  each row
#'   are the covariates for one location and timepoint.  matrix is blocked by
#'   timepoint.
#' @param z matrix containing remote covariates.  each column has remote 
#'   covariates for one timepoint.
#' @param coords n x 2 matrix containing lon-lat coordinates for locations.
#' @param miles T/F for whether to compute great circle distances in miles (T)
#'   or km (F)
#' @param priors A list containing parameters for the prior distributions. The
#'  list needs to contain the following values
#'    \describe{
#'      \item{T}{ list(Psi=matrix, nu=double) specifying the Inverse wishart 
#'                prior distribution for the spatially varying coefficient 
#'                process. }
#'      \item{beta}{ list(Linv=matrix) specifying the prior precision matrix 
#'                   for the fixed local covariates. }
#'      \item{sigmasq}{ list(a=double, b=double) specifying the prior shape and
#'                      scale parameters for the covariance scale and nugget 
#'                      parameters. }
#'      \item{rho}{ list(L=double, U=double) specifying the lower and upper 
#'                  bounds for the spatial range parameter. }
#'      \item{cov}{ list(nu=double) specifying the smoothness for the 
#'                  matern covariance.}
#'    }
#' @param nSamples number of MCMC iterations to run
#' @param thin MCMC thinning; defaults to no thinning (thin=1)
#' @param rw.initsd Initial proposal standard deviation for RW samplers
#' @param inits optional list containing starting parameters for MCMC sampler
#' @param C scaling factor used in adapting random walk proposal variances.
#' @param alpha target acceptance rate for random walk proposals.
#' 
#' @example examples/svcMod.R


svcFit = function(y, X, z, coords, miles=T, priors, nSamples, thin=1, 
                  rw.initsd=.1, inits=list(), C=1, alpha=.44) {
  
  D = rdist.earth(coords, miles=miles)
    
  if(is.null(inits)) {
    inits = list()
  }
  
  res = .Call(`r_svcfit`, y, X, z, D, priors$T$nu, 
              priors$T$Psi, priors$beta$Linv, priors$sigmasq$a, 
              priors$sigmasq$b, priors$rho$L, priors$rho$U, priors$cov$nu,
              nSamples, thin, rw.initsd, inits, C=1, alpha=.44)
  
  reslist = list(
    parameters = list(samples = res,
                      beta.names = colnames(X),
                      theta.names = rownames(z)),
    priors = priors,
    miles = miles,
    coords = coords
  )
  
  class(reslist) = 'svcFit'
  
  reslist
}