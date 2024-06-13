\name{Reactor.data}
\alias{Reactor.data}
\docType{data}
\title{Reactor Experiment Data}
\description{
Data of the Reactor Experiment from Box, Hunter and Hunter (1978).
}
\usage{data(Reactor.data)}
\format{
  A data frame with 32 observations on the following 6 variables.
  \describe{
    \item{A}{numeric vector. Feed rate factor.}
    \item{B}{numeric vector. Catalyst factor.}
    \item{C}{numeric vector. Agitation rate factor.}
    \item{D}{numeric vector. Temperature factor.}
    \item{E}{numeric vector. Concentration factor.}
    \item{y}{numeric vector. Percentage reacted response.}
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
data(Reactor.data,package="OBsMD")
print(Reactor.data)
}
\keyword{datasets}
