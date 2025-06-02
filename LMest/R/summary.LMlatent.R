summary.LMlatent<-function(object,...){ 
cat("Call:\n")
print(object$call)
cat("\nCoefficients:\n")
if(is.null(object$k)) object$k = dim(object$Psi)[2]
if(object$k>1){
  cat("\n Be - Parameters affecting the logit for the initial probabilities:\n")
	print(round(object$Be,4))
	if(is.null(object$seBe)==FALSE){
		cat("\n Standard errors for Be:\n")
		print(round(object$seBe,4))
		pvalueBe <- 2*pnorm(abs(object$Be/object$seBe),lower.tail=FALSE)
		cat("\n p-values for Be:\n")
		print(round(pvalueBe,3))
	}
	if(object$param=="multilogit"){
	cat("\n Ga - Parameters affecting the logit for the transition probabilities:\n")
		print(round(object$Ga,4))	
		if(is.null(object$seGa)==FALSE){
			cat("\n Standard errors for Ga:\n")
			print(round(object$seGa,4))	
			pvalueGa <- 2*pnorm(abs(object$Ga/object$seGa),lower.tail=FALSE)
			cat("\n p-values for Ga:\n")
			print(round(pvalueGa,3))
		}	
	}else if(object$param=="difflogit"){
		cat("\n Ga0 - Intercept affecting the logit for the transition probabilities:\n")
		print(round(object$Ga[[1]],4))
		cat("\n Ga1 - Regression parameters affecting the logit for the transition probabilities:\n")
		print(round(object$Ga[[2]],4))
		
		if(is.null(object$seGa)==FALSE){
			cat("\n Standard errors for Ga0:\n")
			print(round(object$seGa[[1]],4))
			
			cat("\n Standard errors for Ga1:\n")
			print(round(object$seGa[[2]],4))
			
			pvalueGa0 <- 2*pnorm(abs(object$Ga[[1]]/object$seGa[[1]]),lower.tail=FALSE)
			cat("\n p-values for Ga0:\n")
			print(round(pvalueGa0,3))
			
			pvalueGa1 <- 2*pnorm(abs(object$Ga[[2]]/object$seGa[[2]]),lower.tail=FALSE)
			cat("\n p-values for Ga1:\n")
			print(round(pvalueGa1,3))
		}
	}
	
}
	cat("\n Psi - Conditional response probabilities:\n")
	print(round(object$Psi,4))
	if(is.null(object$sePsi)==FALSE){
		cat("\n Standard errors for the conditional response probability matrix:\n")
		print(round(object$sePsi,4))
	}
}