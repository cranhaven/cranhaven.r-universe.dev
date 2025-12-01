#' @title Plot function for ion selective electrode characterisation and estimation of sample concentrations
#' @description Plots sample concentration estimates derived from Bayesian calibration.  E.g. analyseISE uses Bayesian calibration to estimate parameters for y = a + b log(x + c) + error, where error follows a normal distribution with mean 0 and standard deviation sigma.  These valus are combined with experimental data to estimate sample concentrations.
#' @param x Calibration and experimental sample results (of class 'analyseISE'; see analyseISE)
#' @param xlab Label for the x-axis
#' @param ylab Label for the y-axis
#' @param xlim Limits for the x-axis.  Automatically calculated if xlim = NA.
#' @param ylim Limits for the y-axis.
#' @param x.ticks Location of tickmarks for the x-axis.  Automatically calculated if x.ticks = NA.
#' @param y.ticks Location of tickmarks for the y-axis.  Automatically calculated if y.ticks = NA.
#' @param x.ticks.label Labels associated with x-axis tickmarks for the x-axis. Automatically caluclated labels (TRUE), no labels (FALSE), or a column of text specifying custom labels (e.g. x.ticks.label = c("A", "B", "C") or similar, of the same length as x.ticks).
#' @param y.ticks.label Labels associated with y-axis tickmarks for the y-axis. See x.ticks.label for details.
#' @param y.las Indicates whether y-axis labels be perpendicular to the y-axis (2) or parallel to it (0).
#' @param col Colour for the field of the plot.
#' @param x.shift Shifts the plots to the left (- values) or right (+ values); useful for overlaying figures.
#' @param xaxs The style of x-axis interval. See par for further details, but "r" adds 4 percent padding, "i" has no padding.
#' @param yaxs The style of y-axis interval.  See xaxs above.	
#' @param add.box Indicates whether a box should be drawn around the plot (TRUE) or not (FALSE).	
#' @param ... Other arguments to be passed through to plotting functions.
#' @author Peter Dillingham, \email{peter.dillingham@@otago.ac.nz}
#' @seealso \code{\link{analyseISE}}
plot.analyseISE  = function(x, xlab = "Sample ID", ylab = expression(paste(log[10], " { ", italic(x), " }" )),
	xlim=NA, ylim = c(-15, 0), x.ticks = NA, y.ticks = NA,  x.ticks.label = TRUE,  y.ticks.label = TRUE, 
	y.las = 2, col = 1, x.shift = 0, xaxs = "r", yaxs = "r", add.box = TRUE, ...) {

###
# Plots sample concentration estimates (use describeISE and plot.ISEdescription to plot ISE parameter estimates)
#	along with inter-quartile range and 95% credible intervals
#
# Input:		x, an object of class 'analyseISE' as produced by the function 'analyseISE'
#
# Graphical parameters that may be specified, if desired:
#  xlab:		label for the x-axis
#  ylab:		label for the y-axis
#  xlim:		The range of the x axis for the sample concentrations plot (use rarely)
#  ylim: 		The range of the y axis for the sample concentrations plot (use often to zoom in or out)
#  x.ticks:     	The location of tickmarks on the x axis (e.g. x.ticks = c(1, 3, 5) draws ticks at 1, 3, and 5
#  y.ticks:     	The location of tickmarks on the y axis
#  x.ticks.label	Should values be added to the tickmarks for the x-axis (TRUE) or not (FALSE). A column of text can also be added to specify the values.
#  y.ticks.label	Should values be added to the tickmarks for the x-axis (TRUE) or not (FALSE). A column of text can also be added to specify the values.
#  y.las:		y-axis labels are parallel (0) or perpendicular (2) to the y-axis
#  col:		colour for the field
#  x.shift:		shifts the plots to the left (- values) or right (+ values); useful for overlaying figures
#  xaxs:		The style of x-axis interval. see par for further details, but "r" adds 4 percent padding, "i" has no padding
#  yaxs:		The style of y-axis interval.  See xaxs above.	 
#  add.box: 	Should a box be drawn around the figure (TRUE) or not (FALSE)
###
	ISEanalysis = x

	M = max(ISEanalysis$SampleID)
	delta = 0.015*M
	S.low = ISEanalysis$SampleID - delta + x.shift
	S.high = ISEanalysis$SampleID + delta + x.shift
	S.llow = ISEanalysis$SampleID - 0.5*delta + x.shift
	S.lhigh = ISEanalysis$SampleID + 0.5*delta + x.shift

	if (M > 1) {
		if (is.na(xlim[1])) { xlim = c(min(ISEanalysis$SampleID), max(ISEanalysis$SampleID)) }
		plot(rep(ISEanalysis$SampleID[1] + x.shift,2), c(ISEanalysis$log10x.exp[1,2], ISEanalysis$log10x.exp[1,3]), type="l", axes=F,
			xlim = xlim, ylim = ylim, lend = 1, 
			xlab = xlab, ylab = ylab, col = col, xaxs = xaxs, yaxs = yaxs, ...
		)
		if(add.box) box()
		if(!is.na(x.ticks[1])) {
			axis(1, at = x.ticks, cex.axis= 0.8, labels = x.ticks.label)
		}
		if(is.na(x.ticks[1])) {
			axis(1, at = ISEanalysis$SampleID, cex.axis = 0.8, labels = x.ticks.label)
		}
		if(is.na(y.ticks[1])) {
			axis(2, cex.axis= 0.8, las=y.las, labels = y.ticks.label)
		}
		if(!is.na(y.ticks[1])) {
			axis(2, at = y.ticks, cex.axis= 0.8, las=y.las, labels = y.ticks.label)
		}	

		for (i in 1:M) {
			lines(rep(ISEanalysis$SampleID[i] + x.shift,2), c(ISEanalysis$log10x.exp[i,2], ISEanalysis$log10x.exp[i,3]), col=col, ...)
			lines(c(S.llow[i], S.lhigh[i]), rep(ISEanalysis$log10x.exp[i,2], 2) ,  col=col, ...)
			lines(c(S.llow[i], S.lhigh[i]), rep(ISEanalysis$log10x.exp[i,3], 2), col=col, ...)
			lines(c(S.low[i], S.high[i]), rep(ISEanalysis$log10x.exp[i,1], 2), lwd=1, col=col, ...)
			lines(rep(ISEanalysis$SampleID[i] + x.shift,2), ISEanalysis$log10x.exp.IQ[i,], lwd=3, lend = 1, col=col, ...)
		}
	}

	if (M == 1) {
		if (is.na(xlim[1])) { xlim = c(0.8, 1.2) }
		plot(rep(ISEanalysis$SampleID[1] + x.shift,2), c(ISEanalysis$log10x.exp[2], ISEanalysis$log10x.exp[3]), type="l", axes=F,
			xlim = xlim, ylim = ylim, lend = 1, 
			xlab = xlab, ylab = ylab, col = col, ...
		)
		if(add.box) box()
		if(!is.na(x.ticks[1])) {
			axis(1, at = x.ticks, cex.axis= 0.8, labels = x.ticks.label)
		}
		if(is.na(x.ticks[1])) {
			axis(1, at = ISEanalysis$SampleID, cex.axis = 0.8, labels = x.ticks.label)
		}
		if(is.na(y.ticks[1])) {
			axis(2, cex.axis= 0.8, las=y.las, labels = y.ticks.label)
		}
		if(!is.na(y.ticks[1])) {
			axis(2, at = y.ticks, cex.axis= 0.8, las=y.las, labels = y.ticks.label)
		}

		lines(rep(ISEanalysis$SampleID[1] + x.shift,2), c(ISEanalysis$log10x.exp[2], ISEanalysis$log10x.exp[3]), col=col, ...)
		lines(c(S.llow, S.lhigh), rep(ISEanalysis$log10x.exp[2], 2) ,  col=col, ...)
		lines(c(S.llow, S.lhigh), rep(ISEanalysis$log10x.exp[3], 2), col=col, ...)
		lines(c(S.low, S.high), rep(ISEanalysis$log10x.exp[1], 2), lwd=1, col=col, ...)
		lines(rep(ISEanalysis$SampleID[1] + x.shift,2), ISEanalysis$log10x.exp.IQ, lwd=3, lend = 1, col=col, ...)

	}

}