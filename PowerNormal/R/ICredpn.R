#' Credibility interval for alpha (PN)
#'
#' @description Credibility interval for alpha
#'




pn.ICred <- function(x, p, prior="Jeffreys", shape_0=NULL, rate_0 = NULL){

  if((prior != "Uniform") && (prior != "Gamma") && (prior != "Jeffreys") ) stop("Prior distribution not recognized.")

  n <- length(x)
  Tx <- -sum(log(pnorm(x)))

  if(prior == "Jeffreys"){
    return(list(L_Inf = qgamma((1-p)/2,n,Tx),
                L_Sup = qgamma(1-(1-p)/2,n,Tx)))}

  if(prior == "Uniform"){
    return(list(L_Inf = qgamma((1-p)/2,n+1,Tx),
                L_Sup = qgamma(1-(1-p)/2,n+1,Tx)))}

  if(prior == "Gamma"){
    if( (length(shape_0)==0) | (length(rate_0) == 0))stop("Unspecified hyperparameters")

    return(list(L_Inf = qgamma((1-p)/2,n + shape_0,Tx + rate_0),
                L_Sup = qgamma(1-(1-p)/2,n + shape_0,Tx + rate_0)))}
}


