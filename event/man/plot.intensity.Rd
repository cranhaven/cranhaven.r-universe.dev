\name{plot.intensity}
\title{Plot Intensity Functions}
\alias{plot.intensity}
\alias{plot.intensity.default}
\usage{
\method{plot}{intensity}(x, ...)
\method{plot.intensity}{default}(x, censor=1, group=1, colour=TRUE, mix=1,
	ylim=c(0,1), ylab="p", xlab="Time",
	main="Empirical Hazard Function(s)", ...)
}
\description{
Plot the empirical intensity curve for given times between events.
}
\arguments{
\item{censor}{Vector of censoring indicators corresponding to the
vector of times or to the last time in each vector of a list.}
\item{group}{Vector indicating to which group each individual
belongs.}
\item{colour}{Use a different colour for each curve.}
\item{x}{An object produced by \code{km} for \code{plot.intensity}; for \code{plot.intensity.default} it is \code{times} (Vector of times to events or a list of vectors of such
times for different individuals.)  These changes were made for S3 methods compatability.}
\item{mix}{...}
\item{main}{Plotting control options.}
\item{ylab}{Plotting control options.}
\item{xlab}{Plotting control options.}
\item{ylim}{Plotting control options.}
\item{...}{Plotting control options.}
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{km}}, \code{\link[event]{plot.surv}}
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
