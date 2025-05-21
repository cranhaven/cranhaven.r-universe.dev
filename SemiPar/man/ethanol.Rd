\name{ethanol}
\alias{ethanol}
\non_function{}
\title{Ethanol data}
\usage{data(ethanol)}
\description{
The ethanol data frame contains 88 sets of measurements for variables
from an experiment in which ethanol was burned in a
single cylinder automobile test engine.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{NOx}{the concentration of nitric oxide (NO) and 
               nitrogen dioxide (NO2) in engine exhaust, 
               normalized by the work done by the engine.}
    \item{C}{the compression ratio of the engine}
    \item{E}{the equivalence ratio at which the engine was run --
             a measure of the richness of the air/ethanol mix.}
  }
}
\source{Brinkman, N.D. (1981). Ethanol fuel -- a single-cylinder 
     engine study of efficiency and exhaust emissions.
   \emph{SAE transactions} Vol. 90, 
      No 810345, 1410--1424.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(ethanol)
pairs(ethanol)
}
\keyword{datasets}
