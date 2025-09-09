#' Fucntion for estimating initial parameter values
#'
#' @param CFM A counter-factual model
#' @param DC_clean a cleaned dataset obsect obtained using dataComb.flexsurvreg
#' @param trt An optional additional vector denoting treatment allocations for multiple treatment comparisons.  Defaults to 'NULL'
#' @details
#' This function takes the liklihood for a 'flexsurvreg' model and uses 'optim'
#'   to fit the likelihood.
#' @return an 'optim' output giving the parameter values to be supplied as a
#'   starting value for the mcmc routine.
#' @export
#'
initParm.glm <- function(CFM,DC_clean,trt=trt){

  if(is.null(trt)){
    beta<- 0
    ip <- optim(beta, lik.glm, DC_clean=DC_clean, method = "Brent", lower = -10,
                upper = 10, hessian = T)
  }

  if(!is.null(trt)){
    beta <- rep(0,length(levels(factor(trt))))
    ip <- optim(beta, lik.glm.mtc, DC_clean=DC_clean, method = "BFGS", hessian = T)
  }

  return(ip)

  }

