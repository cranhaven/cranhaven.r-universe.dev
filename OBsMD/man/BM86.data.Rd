\name{BM86.data}
\alias{BM86.data}
\docType{data}
\title{Data sets in Box and Meyer (1986)}
\description{ Design factors and responses used in the examples of Box and
Meyer (1986) } \usage{data(BM86.data)} \format{
  A data frame with 16 observations on the following 19 variables.
  \describe{
    \item{X1}{numeric vector. Contrast factor.}
    \item{X2}{numeric vector. Contrast factor.}
    \item{X3}{numeric vector. Contrast factor.}
    \item{X4}{numeric vector. Contrast factor.}
    \item{X5}{numeric vector. Contrast factor.}
    \item{X6}{numeric vector. Contrast factor.}
    \item{X7}{numeric vector. Contrast factor.}
    \item{X8}{numeric vector. Contrast factor.}
    \item{X9}{numeric vector. Contrast factor.}
    \item{X10}{numeric vector. Contrast factor.}
    \item{X11}{numeric vector. Contrast factor.}
    \item{X12}{numeric vector. Contrast factor.}
    \item{X13}{numeric vector. Contrast factor.}
    \item{X14}{numeric vector. Contrast factor.}
    \item{X15}{numeric vector. Contrast factor.}
    \item{y1}{numeric vector. Log drill advance response.}
    \item{y2}{numeric vector. Tensile strength response.}
    \item{y3}{numeric vector. Shrinkage response.}
    \item{y4}{numeric vector.  Yield of isatin response.}
  }
}
\references{
  Box, G. E. P. and Meyer, R. D. (1986)
  An Analysis of Unreplicated Fractional Factorials.,
  \emph{Technometrics} \bold{28}(1), 11--18.
  \doi{10.1080/00401706.1986.10488093}.
}
\examples{
library(OBsMD)
data(BM86.data,package="OBsMD")
print(BM86.data)
}
\keyword{datasets}
