\name{pcapture}
\alias{pcapture}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Posterior probabilities for the multiple stage capture-recapture model%%  ~~function to do ... ~~
}
\description{
This function computes the posterior expectation of the population size for a 
multiple stage capture-recapture experiment (Chapter 5) under a uniform prior on 
the range (0,400).
}
\usage{
pcapture(T, nplus, nc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T}{
number of experiments%%     ~~Describe \code{T} here~~
}
  \item{nplus}{
total number of captured animals%%     ~~Describe \code{nplus} here~~
}
  \item{nc}{
total number of captures%%     ~~Describe \code{nc} here~~
}
}
\details{
This analysis is based on the restrictive assumption that all dippers captured in the
second year were already present in the population during the first year.
}
\value{
%%  ~Describe the value returned
numerical value of the posterior expectation
}
\seealso{
\code{\link{pdarroch}}
}
\examples{
sum((1:400)*pcapture(2,70,81))
}
\keyword{capture-recapture model}
\keyword{posterior expectation}
\keyword{uniform prior}
