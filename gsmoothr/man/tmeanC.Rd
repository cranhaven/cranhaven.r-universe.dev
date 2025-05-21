\name{tmeanC}
\alias{tmeanC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Trimmed Mean Smoother}
\description{
A fast trimmed mean smoother (using C code) of data at discrete points (e.g. probe-level data).
}
\usage{
tmeanC(sp, x, spout = NULL, nProbes = 10, probeWindow = 600, trim = 0.1) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{numeric vector of positions (x-values)}
  \item{x}{numeric vector of data (corresponding to \code{sp}}
  \item{spout}{optional vector of output values to calculate trimmed mean at, default: NULL}
  \item{nProbes}{minimum number of observations required within window}
  \item{probeWindow}{distance (in x) in each direction to look for observations to be used in the trimmed mean}
  \item{trim}{proportion of trim to use in trimmed mean}
}
\details{
Using the specified probe window, this procedure uses all values within the window and calculates a trimmed mean with the specified amount of trim.  If there are not enough observations within the window at a given position (as given by \code{nProbes}), a zero is returned.
}
\value{
  vector (of the same length as \code{sp} (or \code{spout})) giving the trimmed mean smoothed values
}
\author{Mark Robinson}
\seealso{\code{\link{trimmedMean}}}
\examples{

sp <- seq(100, 1000, by=100)
ss <- seq(100,1000, by=50)
set.seed(14)
x <- rnorm(length(sp))

tmC <- tmeanC(sp, x, probeWindow=300, nProbes=5)
tmC1 <- tmeanC(sp, x, spout=sp, probeWindow=300, nProbes=5)
tmC2 <- tmeanC(sp, x, spout=ss, probeWindow=300, nProbes=5)

cbind(tmC,tmC1)

plot(sp, x, type="h", ylim=c(-2,2))
lines(sp, tmC1, col="blue")
lines(ss, tmC2, col="red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
