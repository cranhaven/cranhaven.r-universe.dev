\name{MetalCutting}
\alias{MetalCutting}
\docType{data}
\title{Data sets in Edwards, Weese and Palmer (2014)}
\description{Design factors and responses used in the examples of Edwards, Weese and Palmer (2014)}
\usage{data(MetalCutting)}
\format{
  A data frame with 64 observations on the following 8 variables.
  \describe{
    \item{blk}{block}
    \item{A}{numeric vector. Tool speed.}
    \item{B}{numeric vector. Workpiece speed.}
    \item{C}{numeric vector. Depth of cut.}
    \item{D}{numeric vector. Coolant.}
    \item{E}{numeric vector. Direction of cut.}
    \item{F}{numeric vector. Number of cut.}
    \item{Ytransformed}{numeric vector. Response.}
  }
}
\references{
Edwards, D. J. P., Weese, M. L. and Palmer, G. A. (2014)
Comparing methods for design
follow-uprevisiting a metal-cutting case study.,
\emph{Applied Stochastic Models in Business and Industry} \bold{30}(4), 464--478.
  \doi{10.1002/asmb.1988}
}
\examples{
library(OBsMD)
data(MetalCutting,package="OBsMD")
print(MetalCutting)
}
\keyword{datasets}
