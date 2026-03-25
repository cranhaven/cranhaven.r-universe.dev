\name{eurodip}
\alias{eurodip}
\docType{data}
\title{European Dipper dataset}
\description{
This capture-recapture dataset on the \emph{European dipper} bird
covers 7 years (1981-1987 inclusive) of observations of captures
within one of three zones. It is used in Chapter 5.
}
\usage{data(eurodip)}
\format{
  A data frame with 294 observations on the following 7 variables.
  \describe{
    \item{\code{t1}}{non-capture/location on year 1981}
    \item{\code{t2}}{non-capture/location on year 1982}
    \item{\code{t3}}{non-capture/location on year 1983}
    \item{\code{t4}}{non-capture/location on year 1984}
    \item{\code{t5}}{non-capture/location on year 1985}
    \item{\code{t6}}{non-capture/location on year 1986}
    \item{\code{t7}}{non-capture/location on year 1987}
  }
}
\details{
The data consists of markings and recaptures of breeding adults 
each year during the breeding period from early March to early June. Birds
were at least one year old when initially banded. In \code{eurodip},  
each row gof seven digits corresponds to a capture-recapture story for a given dipper,
0 indicating an absence of capture that year and, in the case of a capture,
1, 2, or 3 representing the zone where the dipper is captured. This dataset corresponds to
three geographical zones covering 200 square kilometers in eastern France.
It was kindly provided to us by J.D. Lebreton.
}
\references{
Lebreton, J.-D., K. P. Burnham, J. Clobert, and D. R. Anderson. (1992)
Modeling survival and testing biological hypotheses using marked animals:
case studies and recent advances. \emph{Ecol. Monogr.} \bold{62}, 67-118.  
}
\examples{
data(eurodip)
summary(eurodip)
}
\keyword{datasets}
\keyword{capture-recapture}
\keyword{European dipper}
