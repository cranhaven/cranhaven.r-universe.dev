summary.LMbasiccont<-function(object,...){

  piv = cbind(est_piv = object$piv)
  k = length(piv)
  cat("Call:\n")
  print(object$call)
  cat("\nCoefficients:\n")
  cat("\nInitial probabilities:\n")
  print(round(piv,4))
  if(is.null(object$sepiv)==FALSE){
    cat("\nStandard errors for the initial probabilities:\n")	
    print(round(cbind(se_piv = object$sepiv),4))
  }
  TT = dim(object$Pi)[3]
  cat("\nTransition probabilities:\n")
  if(k==1){
    print(object$Pi)
  }else{ 
    if(!is.null(object$call$mod)){
      if(object$call$mod==1) print(round(object$Pi[,,2],4)) else print(round(object$Pi[,,2:TT],4))
    }else print(round(object$Pi[,,2:TT],4))
  }	
  if(is.null(object$sePi)==FALSE){
    cat("\nStandard errors for the transition probabilities:\n")
    if(k==1){
      print(object$Pi)
    }else{
      if(!is.null(object$call$mod)){
        if(object$call$mod==1) print(round(object$sePi[,,2],4)) else print(round(object$sePi[,,2:TT],4))
      }else print(round(object$sePi[,,2:TT],4))
    }	
  }	
  cat("\n Mu - Conditional response means:\n")
  print(round(object$Mu,4))
  if(is.null(object$seMu)==FALSE){
    cat("\n Standard errors for the conditional response means:\n")
    print(round(object$seMu,4))
  }

  cat("\n Si - Variance-covariance matrix:\n")
  print(round(object$Si,4))
  if(is.null(object$seSi)==FALSE){
    cat("\n Standard errors for the variance-covariance matrix:\n")
    print(round(object$seSi,4))
  }

}