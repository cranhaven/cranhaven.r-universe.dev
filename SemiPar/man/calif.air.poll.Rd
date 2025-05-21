\name{calif.air.poll}
\alias{calif.air.poll}
\non_function{}
\title{California air polution data}
\usage{data(calif.air.poll)}
\description{
The \code{calif.air.poll} data frame has 345 sets
of observations ozone level and meteorological variables
in Upland, California, U.S.A., in 1976.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{ozone.level}{Ozone concentration (ppm) at Sandburg Air Force Base.}
    \item{daggett.pressure.gradient}{Pressure gradient at Daggett, California.}
    \item{inversion.base.height}{Inversion base height, feet.}
    \item{inversion.base.temp}{Inversion base temperature, degrees Fahrenheit.}
  }
}
\source{Brieman, L. and Friedman, J. (1985). Estimating optimal
transformations for multiple regression and correlation
(with discussion).
\emph{Journal of the American Statistical Association},
\bold{80}, 580--619.
}
\examples{
library(SemiPar)
data(calif.air.poll)
pairs(calif.air.poll)
}
\keyword{datasets}
