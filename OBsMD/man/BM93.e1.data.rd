\name{BM93.e1.data}
\alias{BM93.e1.data}
\docType{data}
\title{Example 1 data in Box and Meyer (1993)}
\description{
12-run Plackett-Burman design from the $2^5$ reactor
    example from Box, Hunter and Hunter (1977).
}
\usage{data(BM93.e1.data)}
\format{
  A data frame with 12 observations on the following 7 variables.
  \describe{
    \item{Run}{a numeric vector. Run number from a $2^5$ factorial design in standard order.}
    \item{A}{a numeric vector. Feed rate factor.}
    \item{B}{a numeric vector. Catalyst factor.}
    \item{C}{a numeric vector. Agitation factor.}
    \item{D}{a numeric vector. Temperature factor.}
    \item{E}{a numeric vector. Concentration factor.}
    \item{y}{a numeric vector. Percent reacted response.}
  }
}
\references{
  Box, G. E. P., Hunter, W. C. and Hunter, J. S. (1978)
  \emph{Statistics for Experimenters}. Wiley.

  Box, G. E. P. and Meyer, R. D. (1993)
  Finding the Active Factors in Fractionated Screening Experiments.,
  \emph{Journal of Quality Technology} \bold{25}(2), 94--105.
  \doi{10.1080/00224065.1993.11979432}.

}
\examples{
library(OBsMD)
data(BM93.e1.data,package="OBsMD")
print(BM93.e1.data)
}
\keyword{datasets}
