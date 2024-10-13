\name{plot.EPoC.validation}
\alias{plot.EPoC.validation}
\alias{plot.EPoC.validation.W}
\alias{plot.EPoC.validation.pred}
\title{Plot model validation criteria}
\encoding{utf-8}
\description{Plot model validation criteria}
\usage{
  \method{plot}{EPoC.validation.W}(x, ...)
  \method{plot}{EPoC.validation.pred}(x, ...)
}
\arguments{
  \item{x}{An object of type \code{EPoC.validation.W} or \code{EPoC.validation.W} respectively.}
  \item{...}{Parameters passed down to underlying functions, \code{plot}, \code{lines}, \code{points}, \code{abline}.}
}
\details{
  Plot Kendall W or prediction error, respectively on the y-axis, network size on the upper x-axis and \eqn{\lambda} on the lower x-axis. 
  The plot fit a loess model of degree 1 to the points from the input object and finds the largest network size and corresponding \eqn{\lambda} such that W is maximized or prediction error is minimized, respectively.
}
\seealso{
  \code{\link{epoc}}, \code{\link{epoc.validation}}, \code{\link{plot.default}}
}
\references{
   Rebecka Jörnsten, Tobias Abenius, Teresia Kling, Linnéa Schmidt, Erik Johansson, Torbjörn Nordling, Bodil Nordlander, Chris Sander, Peter Gennemark, Keiko Funa, Björn Nilsson, Linda Lindahl, Sven Nelander. (2011)  Network modeling of the transcriptional effects of copy number aberrations in glioblastoma.   Molecular Systems Biology 7 (to appear)
}
\keyword{cancer}
\keyword{loess}
\keyword{plot}
\keyword{multivariate}
\keyword{selection}
