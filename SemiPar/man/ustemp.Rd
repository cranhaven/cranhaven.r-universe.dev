\name{ustemp}
\alias{ustemp}
\non_function{}
\title{U.S. temperature data}
\usage{data(ustemp)}
\description{
The \code{ustemp} data frame has 56 observations on the temperature
and location of 56 U.S. cities.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{city}{character string giving name of city and state 
               (two-letter abbreviation).}
    \item{min.temp}{average minimum January temperature.}
    \item{latitude}{degrees latitude (north of Equator).}
    \item{longitude}{degrees longitude (west of Greenwich).}
  }
}
\source{
Peixoto, J.L. (1990). A property of well-formulated polynomial
regression models. \emph{American Statistician}, \bold{44},
26-30.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(ustemp)
attach(ustemp)
grey.levs <- min.temp+20
col.vec <- paste("grey",as.character(grey.levs),sep="")
plot(-longitude,latitude,col=col.vec,pch=16,cex=3,xlim=c(-130,-60))
text(-longitude,latitude,as.character(city))
}
\keyword{datasets}
