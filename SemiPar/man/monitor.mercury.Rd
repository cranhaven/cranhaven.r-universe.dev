\name{monitor.mercury}
\alias{monitor.mercury}
\non_function{}
\title{Mercury biomonintoring data}
\usage{data(monitor.mercury)}
\description{
The \code{monitor.mercury} data frame has 22 observations 
from sampling locations around a solid waste 
incinerator in Warren County, New Jersey, USA}
\format{
  This data frame contains the following columns:
  \describe{
    \item{UTM.North}{longitude of sampling location.}
    \item{UTM.East}{latitude of sampling location.}
    \item{mercury.concentration}{mercury concentration in dry sphagnum 
moss grown at the sampling location.} 
}
}
\source{
Opsomer, J.D., Agras, J., Carpi, A. and Rodrigues, G. (1995), 
An application of locally weighted regression to airborne mercury 
deposition around an incinerator site, \emph{Environmetrics}, 
\bold{6}, 205-221.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http//stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(monitor.mercury)
pairs(monitor.mercury)
}
\keyword{datasets} 
