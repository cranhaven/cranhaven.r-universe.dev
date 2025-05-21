\name{salinity}
\alias{salinity}
\non_function{}
\title{Salinity data}
\usage{data(salinity)}
\description{
The \code{salinity} data frame has 28 observations
on hydrological measurements from Pamlico Sound,
North Carolina, USA.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{salinity}{salinity in Pamlico Sound.}
    \item{lagged.salinity}{salinity in Pamlico Sound during the previous
six weeks.}
    \item{trend}{trend=1 if the data is the first six-week period of the
spring, and so forth.  Used to detect possible effects of the seasonal
warming trend.}
    \item{discharge}{discharge of fresh water from rivers into the sound.}
}
}
\source{
Ruppert, D, and Carroll, R.J. (1980), Trimmed least squares estimation in
the linear model, \emph{Journal of the American Statistical Association},
\bold{75},  828-838.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http//stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(salinity)
pairs(salinity)
}
\keyword{datasets}
