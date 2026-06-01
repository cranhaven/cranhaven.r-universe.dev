\name{SOPIE-package}
\alias{SOPIE-package}
\docType{package}
\title{Package 'SOPIE' : Summary Information}
\description{The package 'SOPIE' provides functions to non-parametrically estimate the off-pulse interval of a source function originating from a pulsar.
This technique is based on a sequential application of P-values 
obtained from goodness-of-fit tests for the uniform distribution. The well-known Kolmogorov-Smirnov, Cramer-von Mises, 
Anderson-Darling and Rayleigh test statistics are applied sequentially on subintervals of \eqn{[0 ; 1]}. \cr The most important functions in the package 
are combined in a wrapper function called \code{\link{SOPIE}}. Users should start by looking at the documentation of the function \code{\link{findh}}, \code{\link{circ.kernel}} and \code{\link{SOPIE}}.}
\details{
\tabular{ll}{
Package: \tab SOPIE\cr
Type: \tab Package\cr
Version: \tab 1.6\cr
Date: \tab 2022-02-23\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes \cr
}
The SOPIE package consists of 4 main functions. Each of these functions are 
discussed in terms of its functioning, structure, arguments and output in the help documentation of each function.\cr
\enumerate{
\item \code{\link{findh}} is the function used to obtain the estimated smoothing parameter \eqn{\hat h} that will be
used in the circular kernel density estimator.
\item \code{\link{circ.kernel}} is the function used to perform circular kernel density estimation on the
sample data set in order to obtain the minimum points of the kernel density estimator.
This is essentially the first step of the suggested procedure, as described in the second reference listed below. The output can also
be used to draw a graph of the circular kernel density estimator.
\item \code{\link{a.estimate}} and \code{\link{b.estimate}} is almost identical function. \code{a.estimate} is the function
used to obtain the estimated values of \eqn{a}, i.e. \eqn{\hat a}, for the off-pulse interval of a pulsar light curve. \code{b.estimate} is the function
used to obtain the estimated values of \eqn{b}, i.e. \eqn{\hat b}, for the off-pulse interval of a pulsar light curve.
\item \code{\link{SOPIE}} is a wrapper-function in the sense that it utilises all of the above function to produce
the estimated off-pulse intervals in an easy readable matrix format, together with
a graph consisting of the histogram estimate of the sample data, the kernel density
estimator and an indication of the estimated median off-pulse interval.
}
}
\author{
Willem Daniel Schutte \cr
Maintainer: Willem Daniel Schutte 
}
\references{
Jammalamadaka, S. Rao and SenGupta, A. (2001). Topics in Circular Statistics, World Scientific Publishing Co. Pte. Ltd. \cr
Schutte WD (2014). Nonparametric estimation of the off-pulse interval(s) of a pulsar light
curve. Ph.D. thesis, North-West University. URL \url{http://hdl.handle.net/10394/12199} \cr
Schutte WD, Swanepoel JWH (2016). SOPIE: an R package for the non-parametric estimation of the off-pulse interval of a pulsar light curve. Monthly Notices of the Royal Astronomical Society, 461, 627-640.\cr
}
\keyword{ package }
%%\seealso{}
\examples{
set.seed(777)
simdata<-von_mises_sim(n=5000,k=1,c=0.3,noise=0.2)
SOPIE(simdata,h=1,to=1,alpha=0.05,g=5,r=10,m=1,grid=100)

}
