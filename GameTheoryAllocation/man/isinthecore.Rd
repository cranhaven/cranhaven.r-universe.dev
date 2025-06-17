\name{isinthecore}
\alias{isinthecore}
\title{
Isinthecore (core allocations)}
\description{
This function checks if the allocation belongs to the core of the TU game.}
\usage{
isinthecore(characteristic_function, allocation, game = c("profit", "cost"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{characteristic_function}{
A vector with the characteristic function \eqn{v(S)} (or \eqn{c(S)}, if we work with a cost game), according to the order of coalitions shown in \code{coalitions} function.
}
  \item{allocation}{
A vector with the allocation where each component indicates the part of each agent.
}
  \item{game}{
Characters to indicate if the game is a cost or profit game. The possibles values are \code{"cost"} or \code{"profit"}.
}
}

\references{
Gillies, D.B.  (1953). Some theorems on n-person games. PhD thesis, Princeton
University.}
\author{
A. Saavedra-Nieves
}
\examples{
characteristic_function<-c(0,0.538, 0.761, 1.742, 0.554, 0.137, 0.293, 0.343)
isinthecore(characteristic_function,allocation=c(0.1,0.2,0.043),game="cost")
#[1] "The allocation is not in the core"
#NULL

isinthecore(characteristic_function,allocation=c(0.05,0.206,0.087),game="cost")
#[1] "The allocation is in the core"
#NULL
}
\keyword{core_allocations}% use one of  RShowDoc("KEYWORDS")
\keyword{core}% __ONLY ONE__ keyword per line
