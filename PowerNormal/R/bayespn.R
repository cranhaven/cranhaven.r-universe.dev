pn.bayes <- function(x,prior="Jeffreys", shape_0=NULL, rate_0 = NULL){

  if((prior != "Uniform") && (prior != "Gamma") && (prior != "Jeffreys") ) stop("Prior distribution not recognized.")

  n <- length(x)
  Tx <- -sum(log(pnorm(x)))

  if(prior == "Jeffreys"){
    return(list(posterior_mean = n/Tx,
                posterior_median = qgamma(0.5,n,Tx),
                posterior_variance = n/Tx^2 ))
  }

  if(prior == "Uniform"){
    return(list(posterior_mean = (n+1)/Tx,
                posterior_median = qgamma(0.5,n+1,Tx),
                posterior_variance = (n+1)/Tx^2 ))
  }

  if(prior == "Gamma"){
    if( (length(shape_0)==0) | (length(rate_0) == 0))stop("Unspecified hyperparameters")

    return(list(posterior_mean = (n+shape_0)/(Tx+rate_0) ,
                posterior_median = qgamma(0.5,n+shape_0,Tx+rate_0),
                posterior_variance = (n+shape_0)/(Tx+rate_0)^2 ))
  }
}


