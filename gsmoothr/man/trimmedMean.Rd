\name{trimmedMean}
\alias{trimmedMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Trimmed Mean Smoother}
\description{
A slow trimmed mean smoother (using R code) of data at discrete points (e.g. probe-level data).
}
\usage{
trimmedMean(pos, score, probeWindow=600, meanTrim=.1, nProbes=10) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pos}{numeric vector of positions (x-values)}
  \item{score}{numeric vector of data (corresponding to \code{sp}}
  \item{probeWindow}{distance (in x) in each direction to look for observations to be used in the trimmed mean}
  \item{meanTrim}{proportion of trim to use in trimmed mean}
  \item{nProbes}{minimum number of observations required within window}
}
\details{
Using the specified probe window, this procedure uses all values within the window and calculates a trimmed mean with the specified amount of trim.  If there are not enough observations within the window at a given position (as given by \code{nProbes}), a zero is returned.
}
\value{
  vector (of the same length as \code{sp} giving the trimmed mean smoothed values
}
\author{Mark Robinson}
\seealso{\code{\link{tmeanC}}}
\examples{

sp <- seq(100, 1000, by=100)
ss <- seq(100,1000, by=50)
set.seed(14)
x <- rnorm(length(sp))

tmC <- trimmedMean(sp, x, probeWindow=300, nProbes=5)
}
