#' Compute point estimates for parameters from posterior samples
#'
#' @export
#'
#' @importFrom stats prcomp
#' 
#' @param object stPredict object containing posterior estimates of alphas
#' @param stFit stFit object containing posterior samples for model
#' @param stData stData object containing spatial information for dataset
#' @param burn number of posterior samples to reject before computing estimates
#' @param type One of the following options to specify what point estimates to return
#'    \describe{
#'      \item{eof-alpha_knots}{ Remote coefficient estimates (alpha_knots) 
#'        mapped onto the eof patterns of the remote covariates.   }
#'    }
#' @param ... S3 generic/method consistency
#' 
#' @examples
#' 
#' data("coprecip")
#' data("coprecip.fit")
#' data("coprecip.predict")
#' 
#' coef(coprecip.predict, stFit = coprecip.fit, stData = coprecip, burn = 50)
#'  

coef.stPredict = function(object, stFit, stData, burn = 1, 
                          type = 'eof-alpha_knots', ...) {
  stPredict = object
  # determine which type of coefficients are requested
  match.opts = c('eof-alpha_knots')
  type = match.opts[pmatch(type, match.opts)]
  
  # TODO: replace this code so that it reformats the output in stPredict objects
  
  # compute requested point estimate
  if( type=='eof-alpha_knots' ) {
    
    # compute eofs
    eof = prcomp(stData$Z, center = F)
    W = -eof$x
    Tmat = -eof$rotation
    
    # compute correlation matrices
    coefs = coef.stFit(stFit, burn)
    Dz_knots = rdist.earth(stFit$coords.knots, miles=stFit$miles)
    Dz_to_knots = rdist.earth(stData$coords.r, stFit$coords.knots, miles=stFit$miles)
    Rst = maternCov( Dz_knots, smoothness = stFit$priors$cov.r$smoothness,
                     scale = coefs$sigmasq_r, range = coefs$rho_r )
    cst = maternCov( Dz_to_knots, smoothness = stFit$priors$cov.r$smoothness,
                     scale = coefs$sigmasq_r, range = coefs$rho_r )
    
    # compute eof coefficients for all locations in local domain
    A=matrix(stPredict$alpha_knots$alpha, nrow = nrow(stFit$coords.knots))
    ret = t(t(W) %*% cst %*% solve(Rst) %*% A)
    dimnames(ret)=list(Location=1:nrow(ret), EOF=1:ncol(ret))
  }
  
  ret
}