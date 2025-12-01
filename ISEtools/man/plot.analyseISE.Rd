\name{plot.analyseISE}
\alias{plot.analyseISE}
\title{Plot function for ion selective electrode characterisation and estimation of sample concentrations}
\usage{
\method{plot}{analyseISE}(x, xlab = "Sample ID",
  ylab = expression(paste(log[10], " { ", italic(x), " }")), xlim = NA,
  ylim = c(-15, 0), x.ticks = NA, y.ticks = NA, x.ticks.label = TRUE,
  y.ticks.label = TRUE, y.las = 2, col = 1, x.shift = 0, xaxs = "r",
  yaxs = "r", add.box = TRUE, ...)
}
\arguments{
\item{x}{Calibration and experimental sample results (of class 'analyseISE'; see analyseISE)}

\item{xlab}{Label for the x-axis}

\item{ylab}{Label for the y-axis}

\item{xlim}{Limits for the x-axis.  Automatically calculated if xlim = NA.}

\item{ylim}{Limits for the y-axis.}

\item{x.ticks}{Location of tickmarks for the x-axis.  Automatically calculated if x.ticks = NA.}

\item{y.ticks}{Location of tickmarks for the y-axis.  Automatically calculated if y.ticks = NA.}

\item{x.ticks.label}{Labels associated with x-axis tickmarks for the x-axis. Automatically calculated labels (TRUE), no labels (FALSE), or a column of text specifying custom labels (e.g. x.ticks.label = c("A", "B", "C") or similar, of the same length as x.ticks).}

\item{y.ticks.label}{Labels associated with y-axis tickmarks for the y-axis. See x.ticks.label for details.}

\item{y.las}{Indicates whether y-axis labels be perpendicular to the y-axis (2) or parallel to it (0).}

\item{col}{Colour for the field of the plot.}

\item{x.shift}{Shifts the plots to the left (- values) or right (+ values); useful for overlaying figures.}

\item{xaxs}{The style of x-axis interval. See par for further details, but "r" adds 4 percent padding, "i" has no padding.}

\item{yaxs}{The style of y-axis interval.  See xaxs above.}

\item{add.box}{Indicates whether a box should be drawn around the plot (TRUE) or not (FALSE).}

\item{...}{Other arguments to be passed through to plotting functions.}
}
\value{No return value, creates plot.} 
\description{
Plots sample concentration estimates derived from Bayesian calibration.  E.g. analyseISE uses Bayesian calibration to estimate parameters for y = a + b log(x + c) + error, where error follows a normal distribution with mean 0 and standard deviation sigma.  These valus are combined with experimental data to estimate sample concentrations.
}
\seealso{
\code{\link{analyseISE}}
}
\author{
Peter Dillingham, \email{peter.dillingham@otago.ac.nz}
}
