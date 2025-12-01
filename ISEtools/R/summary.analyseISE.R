#' @title Summary of estimates for ISE parameter values and experimental sample concentrations.
#' @description summary.analyseISE takes an object of class analyseISE and produces summary tables.
#' @param object Data set of class ISEdata
#' @param ... Other objects passed through.
#' @author Peter Dillingham, \email{peter.dillingham@@otago.ac.nz}
#' @seealso \code{\link{analyseISE}} \code{\link{summary.ISEdescription}}
#' @return tables: Two tables (table1 and table2) are returned as a list.
#' @return \item{table1}{A table of ISE parameter values (see summary.describeISE for details)}
#' @return \item{table2}{A table of estimated analyte concentrations for experimental samples}
summary.analyseISE = function(object, ...) {
###
# Produces matrices with ISE parameter estimates (ISE_summary) and sample concentration estimates (sample_summary)
#	as well as 95% credible intervals
###
	ISEanalysis = object
	table1 = summary.ISEdescription(ISEanalysis)

	M = max(ISEanalysis$SampleID)
	if (M > 1) {
		table2 = cbind(ISEanalysis$SampleID, ISEanalysis$log10x.exp)
		tmp = c("Sample ID", colnames(ISEanalysis$log10x.exp))
		# cat("\nBayesian calibration of experimental samples:\n\n")
		rownames(table2)= rep("", M)
		colnames(table2) = tmp
	}
	if (M == 1) {
		tmp = c("Sample ID", names(ISEanalysis$log10x.exp))
		table2 = c(1, ISEanalysis$log10x.exp)
		# cat("\nBayesian calibration of experimental sample:\n\n")
		table2 = t(table2)
		rownames(table2)= rep("", M)
		colnames(table2) = tmp
	}
	class(table2) = "matrix"
	tables = list(ISE_summary=table1, sample_summary = table2)
	return(tables)
}