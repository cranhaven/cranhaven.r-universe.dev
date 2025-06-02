summary.MCbasic<-function(object,...){ 
piv = cbind(est_piv = object$piv)
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
 
 if(!is.null(object$call$mod)){
	if(object$call$mod==1) print(round(object$Pi[,,2],4)) else print(round(object$Pi[,,2:TT],4))
 }else print(round(object$Pi[,,2:TT],4))
	
if(is.null(object$sePi)==FALSE){
	cat("\nStandard errors for the transition probabilities:\n")
	if(!is.null(object$call$mod)){
			if(object$call$mod==1) print(round(object$sePi[,,2],4)) else print(round(object$sePi[,,2:TT],4))
	}else print(round(object$sePi[,,2:TT],4))
		
}	
}