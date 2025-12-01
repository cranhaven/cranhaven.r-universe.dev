#' @title Basic plot of ion selective electrode calibration data
#' @description Plots raw ISE calibration data; data should follow a hockey stick pattern coinciding with the equation y = a + b log(x + c) + error, where error follows a normal distribution with mean 0 and standard deviation sigma.
#' @param x ISE calibration data
#' @param xlab Label for the x-axis
#' @param ylab Label for the y-axis	
#' @param pch Plotting symbol for data	
#' @param ... Other arguments to be passed through to plotting functions.
#' @author Peter Dillingham, \email{peter.dillingham@@otago.ac.nz}
#' @seealso \code{\link{loadISEdata}}
#' @examples data(LeadStdAdd)
#' plot(LeadStdAdd)
#' @export
plot.ISEdata = function(x, xlab=expression(paste(log[10], " { ", italic(x), " }" )), ylab="emf", pch=20, ...) {
###
# plot ISE calibration data (should follow hockey-stick model
###
	# Plot the calibration data
	ISEID = x$data.calib$ISEID
	for (i in 1:x$R) {
		main=paste("ISE #", i, sep="")
		plot(x = x$log10x[ISEID==i], y = x$emf[ISEID==i], 
			xlab = xlab, ylab = ylab, main = main, pch = pch, ...)
	}
}
