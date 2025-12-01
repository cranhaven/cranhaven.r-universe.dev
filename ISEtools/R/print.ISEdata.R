#' @title Prints ISE data
#' @description Prints tables of calibration data and experimental data (if present).
#' @param x ISE data (e.g. object of class ISEdata)
#' @param ... Other objects passed through.
#' @author Peter Dillingham, \email{peter.dillingham@@otago.ac.nz}
#' @seealso \code{\link{loadISEdata}}
#' @examples data(LeadStdAdd)
#' print(LeadStdAdd)
print.ISEdata = function(x, ...) {
###
# prints ISE calibration and (if loaded) sample experimental data
###
	if (!x$calibration.only) {
		print("Calibration data")
		print(x$data.calib)
		cat("\n")
		print("Experimental data")
		print(x$data.exp)
	}
	if (x$calibration.only) {
		print("Calibration data")
		print(x$data.calib)
		cat("\n")
	}
}
