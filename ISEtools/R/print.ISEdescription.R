#' @title Prints tables of ISE parameters.
#' @description Prints tables of ISE parameters for one or multiple ISEs.
#' @param x ISE analysis results (e.g. object of class analyseISE)
#' @param ... Other objects passed through.
#' @author Peter Dillingham, \email{peter.dillingham@@otago.ac.nz}
#' @seealso \code{\link{describeISE}}
print.ISEdescription = function(x, ...) {
###
# prints ISE parameter estimates and 95% credible intervals
###
	ISEdescription = x
	###
	# Tables to print
	###

	Num.ISEs = length(ISEdescription$ahat)

	## ISE parameter estimates
	cat("\n\nNon-linear parameter estimates and 95% CIs for\n\t y = a + b log(x + c)\n")
	for (i in 1:Num.ISEs) {
		cat("\nISE #",i,":\n", sep="")
		table1 = format(cbind(c(ISEdescription$ahat[i], ISEdescription$bhat[i], ISEdescription$chat[i], ISEdescription$sigmahat[i]), 
			c(ISEdescription$ahat.lcl[i], ISEdescription$bhat.lcl[i], ISEdescription$chat.lcl[i], ISEdescription$sigmahat.lcl[i]), 
			c(ISEdescription$ahat.ucl[i], ISEdescription$bhat.ucl[i], ISEdescription$chat.ucl[i], ISEdescription$sigmahat.ucl[i])), nsmall=2, digits=3)
		colnames(table1)= c("Parameter estimate", "Lower limit", "Upper limit")
		rownames(table1)= c("a", "b", "c", "sigma")
		print(table1, quote=F)
		if(ISEdescription$LOD.info$type == "alpha, beta") {
			cat("\nEstimated log LOD{alpha=", ISEdescription$LOD.info$alpha, ", beta=", ISEdescription$LOD.info$beta,"} (95% CI): ", 
				format(ISEdescription$LOD.hat[i], digits=3), " (" , format(ISEdescription$LOD.lcl[i], digits=3), ", ", format(ISEdescription$LOD.ucl[i], digits=3), ")", "\n\n", sep="")
		}
		if(ISEdescription$LOD.info$type == "S/N") {
			cat("\nEstimated log LOD{S/N=", ISEdescription$LOD.info$SN, "} (95% CI): ", 
				format(ISEdescription$LOD.hat[i], digits=3), " (" , format(ISEdescription$LOD.lcl[i], digits=3), ", ", format(ISEdescription$LOD.ucl[i], digits=3), ")", "\n\n", sep="")
		}
	}
}
