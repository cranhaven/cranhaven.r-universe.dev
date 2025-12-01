#' @title Prints tables of ISE parameters and estimated sample concentrations.
#' @description Prints tables of ISE parameters and estimated sample concentrations.
#' @param x ISE analysis results (e.g. object of class analyseISE)
#' @param ... Other objects passed through.
#' @author Peter Dillingham, \email{peter.dillingham@@otago.ac.nz}
#' @seealso \code{\link{analyseISE}}
print.analyseISE = function(x, ...) {
###
# Prints estimated parameter for the ISE(s) and estimated concentrations for the sample(s)
#	and 95% credible intervals for each
### 
	ISEanalysis = x
	## Print ISE parameters
	print.ISEdescription(ISEanalysis)

	## Print Sample concentrations
	M = max(ISEanalysis$SampleID)
	if (M > 1) {
		table2 = cbind(ISEanalysis$SampleID, format(ISEanalysis$log10x.exp, nsmall=2, digits=3))
		tmp = c("Sample ID", colnames(ISEanalysis$log10x.exp))
		cat("\nBayesian calibration of experimental samples:\n\n")
		rownames(table2)= rep("", M)
		colnames(table2) = tmp
		print(table2, quote=F)
	}
	if (M == 1) {
		tmp = c("Sample ID", names(ISEanalysis$log10x.exp))
		table2 = c(1, format(ISEanalysis$log10x.exp, nsmall=2, digits=3))
		cat("\nBayesian calibration of experimental sample:\n\n")
		table2 = t(table2)
		rownames(table2)= rep("", M)
		colnames(table2) = tmp
		print(table2, quote=F)
	}
}