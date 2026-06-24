
print.EM <- function(x, ...){
	if(!is.null(x$best.result[[1]])){
        	K <- length(x$best.result[[1]]$tau)
        	p <- dim(x$best.result[[1]]$Mu)[1]
        	T <- dim(x$best.result[[1]]$Mu)[2]
		cat("\nBest model: \n")
        	print(x$best.model[1])
		cat("\nK = ", K,
	   	 ", p = ", p,
	   	 ", T = ", T,
            	", logl = ", x$best.result[[1]]$ll,
            	", BIC = ", x$best.result[[1]]$bic,
            	"\n", sep = "")
        	cat("\nCluster sizes:")
        	print(table(x$best.result[[1]]$id))
        	cat("\nid: \n")
        	print(x$best.result[[1]]$id)
		if(x$trans != "Gaussian"){
        		cat("\nLAMBDA: \n")
        		print(x$best.result[[1]]$LA)
		}	
		cat("\nMu: \n")
        	print(x$best.result[[1]]$Mu)
	}
        cat("\nUse $ to access:\n\t-Other parsimonious models \n\t-Sigma and Psi matrices \n\t-posterior probabilities \n")
	invisible()
} # End of print.EM().


summary.EM <- function(object, ...){
	if(!is.null(object$best.result[[1]])){

       		K <- length(object$best.result[[1]]$tau)
        	p <- dim(object$best.result[[1]]$Mu)[1]
        	T <- dim(object$best.result[[1]]$Mu)[2]
		cat("\nBest model: \n")
        	print(object$best.model[1])
		cat("\nK = ", K,
	   	 ", p = ", p,
	   	 ", T = ", T,
            	", logl = ", object$best.result[[1]]$ll,
            	", BIC = ", object$best.result[[1]]$bic,
            	"\n", sep = "")
        	cat("\nCluster sizes:")
        	print(table(object$best.result[[1]]$id))
	}
	invisible()
} # End of summary.EM().


