#' @title Summarises ISE data
#' @description summary.ISE takes an object of class ISEdata (e.g. see loadISEdata) and produces metadata for it.
#' @param object Data set of class ISEdata
#' @param ... Other objects passed through.
#' @author Peter Dillingham, \email{peter.dillingham@@otago.ac.nz}
#' @seealso \code{\link{loadISEdata}}
#' @examples
#' data(LeadStdAdd)
#' summary(LeadStdAdd)	
#' @return metadata: Metadata for the ISEs, a list with N, R, calibration.only, M, and stdadd
#' @return \item{N}{Total number of calibration observations}
#' @return \item{R}{Number of ISEs}
#' @return \item{calibration.only}{Indicates calibration only data (T), or calibration and experimental data (F)}
#' @return \item{M}{Number of experimental samples (NA if no experimental data were loaded)}
#' @return \item{stdadd}{Indicates whether standard addition used for experimental samples (T) or the basic model was used (F), or no experimental data (NA)}
summary.ISEdata = function(object, ...) {
###
# summarises ISE data
# 	N: total number of calibration observations
#	R: number of ISEs
# 	calibration.only: calibration only data (T), or calibration and experimental data (F)
# 	M: Number of experimental samples (NA if no experimental data were loaded)
# 	stdadd: standard addition used for experimental samples (T) or no (F), the basic model was used
###
	data = object
	N = data$N	
	R = data$R	
	calibration.only = data$calibration.only 	
	if (data$calibration.only) { data$M = NA }
	M = data$M	
	stdadd = data$stdadd	

	metadata = list(N = N, R = R, calibration.only = calibration.only, M = M, stdadd = stdadd)
	class(metadata) = "summary.ISEdata"
	metadata
}
