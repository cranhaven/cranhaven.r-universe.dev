\name{OBsMD.es5}
\alias{OBsMD.es5}
\docType{data}
\title{OBsMD.es5}
\description{
Data of the Reactor Experiment from Box, Hunter and Hunter (1978).
}
\usage{data(OBsMD.es5)}
\format{
  A data frame with 8 observations on the following 6 variables.
  \describe{
    \item{A}{numeric vector. Contrast factor.}
    \item{B}{numeric vector. Contrast factor.}
    \item{C}{numeric vector. Contrast factor.}
    \item{D}{numeric vector. Contrast factor.}
    \item{E}{numeric vector. Contrast factor.}
    \item{y}{numeric vector. Response.}
  }
}
\references{
Box G. E. P., Hunter, W. C. and Hunter, J. S. (1978)
    \emph{Statistics for Experimenters}. Wiley.

Box G. E. P., Hunter, W. C. and Hunter, J. S. (2004)
    \emph{Statistics for Experimenters II}. Wiley.

}
\examples{
library(OBsMD)
data(OBsMD.es5,package="OBsMD")
print(OBsMD.es5)
}
\keyword{datasets}
