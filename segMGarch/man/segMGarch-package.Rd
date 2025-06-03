\name{segMGarch-package}
\alias{segMGarch-package}
\alias{segMGarch}
\docType{package}
\title{
  Multiple Change-Point Detection for High-Dimensional GARCH Processes
}
\description{
Implements a segmentation algorithm for multiple change-point detection in high-dimensional GARCH processes described in Cho and Korkas (2018) ("High-dimensional GARCH process segmentation with an application to Value-at-Risk." arXiv preprint arXiv:1706.01155). It simultaneously segments GARCH processes by identifying 'common' change-points, each of which can be shared by a subset or all of the component time series as a change-point in their within-series and/or cross-sectional correlation structure. We adopt the Double CUSUM Binary Segmentation procedure Cho (2016), which achieves consistency in estimating both the total number and locations of the multiple change-points
while permitting within-series and cross-sectional correlations, for simultaneous segmentation of the panel data of transformed time series.

It also provides additional functions and methods that relate to risk management measures and backtests.
}
\details{
We develop a segmentation algorithm for
multiple change-point detection in high-dimensional GARCH processes.
It simultaneously segments GARCH processes 
by identifying 'common' change-points,
each of which can be shared by a subset or all of the component time series
as a change-point in their within-series and/or cross-sectional correlation structure.
The methodology first transforms the \eqn{d}-dimensional time series
into \eqn{d(d+1)/2}-dimensional panel data consisting of 
empirical residual series and their cross-products,
whereby change-points in the complex ((un)conditional variance and covariance) structure
are made detectable as change-points in the simpler (mean) structure of the panel data
at the price of the increased dimensionality. The main routine is \code{garch.seg}.
}
\author{
Haeran Cho and  Karolos Korkas

Maintainer: Karolos Korkas <kkorkas@yahoo.co.uk>
}
\references{
Cho, Haeran, and Karolos Korkas. "High-dimensional GARCH process segmentation with an application to Value-at-Risk." arXiv preprint arXiv:1706.01155 (2018).

Cho, Haeran. "Change-point detection in panel data via double CUSUM statistic." Electronic Journal of Statistics 10, no. 2 (2016): 2000-2038.
 
}
\keyword{ multiple change-point detection, multivariate GARCH, stress period selection, 
Double CUSUM Binary Segmentation, high dimensionality, nonstationarity }
\examples{
  \dontrun{
#pw.CCC.obj <- new("simMGarch")
#pw.CCC.obj <- pc_cccsim(pw.CCC.obj)
#pw.CCC.obj@d=10
#pw.CCC.obj@n=1000
#pw.CCC.obj@changepoints=c(250,750)
#pw.CCC.obj <- pc_cccsim(pw.CCC.obj)
#dcs.obj=garch.seg(pw.CCC.obj@y)
#dcs.obj$est.cps
#ts.plot(t(pw.CCC.obj@y),col="grey");grid()
#abline(v=dcs.obj$est.cps,col="red" )
#abline(v=pw.CCC.obj@changepoints,col="blue" )
#legend("bottom", legend=c("Estimated change-points", "Real change-points"), 
#col=c("red", "blue"), lty=1:2, cex=0.8)
  }
}
