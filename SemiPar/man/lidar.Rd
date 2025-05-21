\name{lidar}
\alias{lidar}
\non_function{}
\title{LIDAR data}
\usage{data(lidar)}
\description{
The \code{lidar} data frame has 221 observations
from a light detection and ranging (LIDAR) experiment.
}
\format{
This data frame contains the following columns:
\describe{
    \item{range}{distance travelled before the light is reflected
                 back to its source.}
    \item{logratio}{logarithm of the ratio of received light
                    from two laser sources.}
  }
}
\source{Sigrist, M. (Ed.) (1994). \emph{Air Monitoring by 
  Spectroscopic Techniques} (Chemical Analysis Series, vol. 197).
  New York: Wiley.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(lidar)
attach(lidar)
plot(range,logratio)
}
\keyword{datasets}
