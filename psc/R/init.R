#' Function for estimating initial parameter values
#'
#' @param pscOb a psc object
#' @details
#' This function takes the likelihood and data structures provided by the
#' pscData() strucutres and fits the likelihood to provide starting values for
#' MCMC estimation
#' @return Parameter Estimates and standard error for the efficacy parameter
#' @importFrom survival Surv
#' @export
init <- function(pscOb){

  if(is.null(pscOb$DC$trt)){
    beta <- 0
    op <- optim(par=c(beta=beta),fn=pscOb$lik,pscOb=pscOb,method="Brent",lower=-10,
                upper=10,hessian=T)
    pscOb$start.mu <- op$par
    pscOb$start.sd <- sqrt(1/diag(op$hessian))
  }


  if(!is.null(pscOb$DC$trt)){
    trt <- pscOb$DC$trt;trt
    trt <- factor(trt)
    lev <- levels(trt)
    beta<- rep(0,length(lev));beta
    op <- optim(par=c(beta=c(0,0)),fn=pscOb$lik,pscOb=pscOb,method="BFGS",hessian=T)
    pscOb$start.mu <- op$par
    pscOb$start.sd <- diag(sqrt(1/diag(op$hessian)))
  }

  return(pscOb)

}
