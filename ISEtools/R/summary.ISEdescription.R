#' @title Summarise ISE parameters
#' @description summary.ISEdescription takes an object of class ISEddescription and prints a table of parameter values for
#'   y = a + b log(x + c) + error, with the erros following a Normal distribution with mean 0 and standard deviation sigma.
#'   Also calculates LOD using the conditional analytic method (alpha, beta, or S/N).
#' @param object object of class ISEdescription
#' @param ... Other objects passed through.
#' @author Peter Dillingham, \email{peter.dillingham@@otago.ac.nz}
#' @seealso \code{\link{describeISE}}	
#' @return table1: A matrix with parameter values for each ISE
summary.ISEdescription = function(object, ...) {
###
# returns a matrix of ISE parameter estimates and 95% credible intervals
###
	ISEdescription = object
	Num.ISEs = length(ISEdescription$ahat)
	i = 1
	table1 = cbind(c(ISEdescription$ahat[i], ISEdescription$bhat[i], ISEdescription$chat[i], ISEdescription$sigmahat[i], ISEdescription$LOD.hat[i]), 
			c(ISEdescription$ahat.lcl[i], ISEdescription$bhat.lcl[i], ISEdescription$chat.lcl[i], ISEdescription$sigmahat.lcl[i], ISEdescription$LOD.lcl[i]), 
			c(ISEdescription$ahat.ucl[i], ISEdescription$bhat.ucl[i], ISEdescription$chat.ucl[i], ISEdescription$sigmahat.ucl[i], ISEdescription$LOD.ucl[i]))
	if (Num.ISEs > 1) {
		for (i in 2:Num.ISEs) {
			tabletmp = cbind(c(ISEdescription$ahat[i], ISEdescription$bhat[i], ISEdescription$chat[i], 
					ISEdescription$sigmahat[i], ISEdescription$LOD.hat[i]), 
				c(ISEdescription$ahat.lcl[i], ISEdescription$bhat.lcl[i], ISEdescription$chat.lcl[i], 
					ISEdescription$sigmahat.lcl[i], ISEdescription$LOD.lcl[i]), 
				c(ISEdescription$ahat.ucl[i], ISEdescription$bhat.ucl[i], ISEdescription$chat.ucl[i], 
					ISEdescription$sigmahat.ucl[i], ISEdescription$LOD.ucl[i]))
			table1 = rbind(table1, tabletmp)
		}
	}
	colnames(table1)= c("Parameter estimate", "Lower limit", "Upper limit")
	rnames = paste(c("a", "b", "c", "sigma", "LOD"), "_ISE1", sep="")
	if (Num.ISEs > 1) {
		for (i in 2:Num.ISEs) {
			tmp = paste(c("a", "b", "c", "sigma", "LOD"), "_ISE", i, sep="")
			rnames = c(rnames, tmp)
		}
	}
	rownames(table1) = rnames

	class(table1) = "matrix"
	return(table1)
}
