\name{ragweed}
\alias{ragweed}
\non_function{}
\title{Ragweed data}
\usage{data(ragweed)}
\description{
The \code{ragweed} data frame has data on ragweed levels
and meteorological variables for 335 days in Kalamazoo,
Michigan, U.S.A.
}
\format{
  This data frame contains the following columns:
  \describe{
       \item{ragweed}{ragweed level (grains per cubic metre).}
       \item{year}{one of 1991, 1992, 1993 or 1994.}
       \item{day.in.seas}{day number in the current ragweed pollen season.}
       \item{temperature}{temperature of following day (degrees Fahrenheit).}
       \item{rain}{indicator of significant rain the following day:
                   1=at least 3 hours of steady or brief but intense rain,
                   0=otherwise.}
       \item{wind.speed}{wind speed forecast for following day (knots).}

  }
}
\source{
Stark, P. C., Ryan, L. M., McDonald, J. L. and Burge, H. A. (1997).
Using meteorologic data to model and predict daily ragweed pollen levels.
\emph{Aerobiologia}, \bold{13}, 177-184.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(ragweed)
pairs(ragweed,pch=".")
}
\keyword{datasets}
