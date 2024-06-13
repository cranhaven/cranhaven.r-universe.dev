\name{BM93.e3.data}
\alias{BM93.e3.data}
\docType{data}
\title{Example 3 data in Box and Meyer (1993)}
\description{
\eqn{2^{8-4}} Fractional factorial design in the injection molding example from
Box, Hunter and Hunter (1978).
}
\usage{data(BM93.e3.data)}
\format{
  A data frame with 20 observations on the following 10 variables.
  \describe{
    \item{blk}{a numeric vector}
    \item{A}{a numeric vector. Mold temperature factor.}
    \item{B}{a numeric vector. Moisture content factor.}
    \item{C}{a numeric vector. Holding Pressure factor.}
    \item{D}{a numeric vector. Cavity thickness factor.}
    \item{E}{a numeric vector. Booster pressure factor.}
    \item{F}{a numeric vector. Cycle time factor.}
    \item{G}{a numeric vector. Gate size factor.}
    \item{H}{a numeric vector. Screw speed factor.}
    \item{y}{a numeric vector. Shrinkage response.}
  }
}
\references{
  Box G. E. P., Hunter, W. C. and Hunter, J. S. (1978)
  \emph{Statistics for Experimenters}. Wiley.

  Box G. E. P., Hunter, W. C. and Hunter, J. S. (2004)
  \emph{Statistics for Experimenters II}. Wiley.

  Box, G. E. P. and Meyer, R. D. (1993)
  Finding the Active Factors in Fractionated Screening Experiments.,
  \emph{Journal of Quality Technology} \bold{25}(2), 94--105.
  \doi{10.1080/00224065.1993.11979432}.

}
\examples{
library(OBsMD)
data(BM93.e3.data,package="OBsMD")
print(BM93.e3.data)
}
\keyword{datasets}
