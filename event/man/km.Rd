\name{km}
\title{Kaplan-Meier Survivor Curves}
\usage{
km(times, censor, group=1, freq=1, cdf=FALSE)
\method{plot}{km}(x, add=FALSE, xlim=NULL, ylim=c(0,1), 
main=NULL, xlab="Time", ylab=NULL, lty=NULL, ...)
\method{plot.intensity}{km}(x, add=FALSE, xlab="Time", ylab="Hazard", type="l", lty=NULL, ...)
\method{plot.dist}{km}(x, ...)
}
\alias{km}
\alias{plot.surv}
\alias{plot.km}
\alias{print.km}
\alias{plot.intensity.km}
\alias{plot.dist}
\alias{plot.dist.km}
\description{
\code{km} calculates the Kaplan-Meier estimates for survival.

To plot the survivor curve, use \code{plot()}; for the empirical
intensity curve, use \code{plot.intensity()}; for diagnostic curves to
choose a distribution to which the data might belong, use \code{plot.dist()}.
}
\arguments{
\item{times}{Vector of times to events or a list of vectors of such
times for different individuals.}
\item{censor}{Vector of censoring indicators corresponding to the
vector of times or to the last time in each vector of a list.}
\item{group}{Vector indicating to which group each individual belongs.}
\item{freq}{Vector of frequencies for grouped data.}
\item{cdf}{If TRUE, calculate the cdf instead of the survivor curve.}
\item{x}{An object produced by \code{km}.}
\item{add}{Plotting control options.}
\item{main}{Plotting control options.}
\item{type}{Plotting control options.}
\item{ylab}{Plotting control options.}
\item{xlab}{Plotting control options.}
\item{xlim}{Plotting control options.}
\item{ylim}{Plotting control options.}
\item{lty}{Plotting control options.}
\item{...}{Plotting control options.}
}
\value{
A matrix with class, \code{km}, containing the Kaplan-Meier estimates
is returned.
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{plot.intensity}}, \code{\link[event]{plot.surv}}
}
\examples{
surv <- rgamma(40,2,scale=5)
cens <- rbinom(40,1,0.9)
treat <- gl(2,20)
plot(km(surv, cens, group=treat), main="",xlab="Months",
	ylab="Probability of deterioration")
plot.dist(km(surv, cens, group=treat))
plot.intensity(km(surv, cens, group=treat),ylab="Risk of deterioration")
}
\keyword{hplot}
