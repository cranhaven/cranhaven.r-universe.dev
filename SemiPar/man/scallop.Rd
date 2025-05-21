\name{scallop}
\alias{scallop}
\non_function{}
\title{Scallop abundance data}
\usage{data(scallop)}
\description{
The \code{scallop} data frame has 148 triplets
concerning scallop abundance; based
on a 1990 survey cruise in the Atlantic continental
shelf off Long Island, New York, U.S.A.}
\format{
  This data frame contains the following columns:
  \describe{
     \item{latitude}{degrees latitude (north of the Equator).}
     \item{longitude}{degrees longitude (west of Greenwich).}
     \item{tot.catch}{size of scallop catch at location specified
by "latitude" and "longitude".}
  }
}
\source{Ecker, M.D. and Heltshe, J.F. (1994).
Geostatistical estimates of scallop abundance.
In \emph{Case Studies in Biometry.} 
Lange, N., Ryan, L., Billard, L., Brillinger, D.,
Conquest, L. and Greenhouse, J. (eds.)
New York: John Wiley & Sons, 107-124.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(scallop)
pairs(scallop)
}
\keyword{datasets}
