\name{autointensity}
\title{Plot Autointensity Function of a Point Process}
\usage{
autointensity(times, window=NULL, maxlag=max(times),
	ylab="Conditional probability", xlab="Lag",
	main="Autointensity function", xlim=c(0,max(times)),
	ylim=c(0,if(plotse)max(se1)else max(z$density)),
	lty=1, plot=TRUE, plotse=TRUE, add=FALSE, ...)
}
\alias{autointensity}
\description{
\code{autointensity} plots the autointensity function of a point
process, including a solid horizontal line indicating the constant
intensity of a Poisson process.
}
\arguments{
\item{times}{Vector of times between events.}
\item{window}{Width of grouping interval.}
\item{maxlag}{Maximum lag to be calculated, by default the maximum
interevent time.}
\item{plot}{If FALSE, values are returned but the function is not
plotted.}
\item{plotse}{If TRUE, plots pointwise two-standard error bands around
the curve.}
\item{add}{If TRUE, add curve to an existing plot.}
\item{ylab}{Plotting control options.}
\item{xlab}{Plotting control options.}
\item{main}{Plotting control options.}
\item{xlim}{Plotting control options.}
\item{ylim}{Plotting control options.}
\item{lty}{Plotting control options.}
\item{...}{Plotting control options.}
}
\value{
A list containing the coordinates of the plotted function and the
standard error bands.
}
\references{
Guttorp, P. (1995) Stochastic Modeling of Scientific Data. Chapman &
Hall, pp. 229, 238-240.
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{cprocess}}.
}
\examples{
times <- rgamma(100,2,scale=4)
autointensity(times, window=3)
}
\keyword{hplot}
