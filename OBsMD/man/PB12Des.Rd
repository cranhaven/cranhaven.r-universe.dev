\name{PB12Des}
\alias{PB12Des}
\docType{data}
\title{ 12-run Plackett-Burman Design Matrix}
\description{
12-run Plackett-Burman design matrix.
}
\usage{data(PB12Des)}
\format{
  A data frame with 12 observations on the following 11 variables.
  \describe{
    \item{x1}{numeric vectors. Contrast factor.}
    \item{x2}{numeric vectors. Contrast factor.}
    \item{x3}{numeric vectors. Contrast factor.}
    \item{x4}{numeric vectors. Contrast factor.}
    \item{x5}{numeric vectors. Contrast factor.}
    \item{x6}{numeric vectors. Contrast factor.}
    \item{x7}{numeric vectors. Contrast factor.}
    \item{x8}{numeric vectors. Contrast factor.}
    \item{x9}{numeric vectors. Contrast factor.}
    \item{x10}{numeric vectors. Contrast factor.}
    \item{x11}{numeric vectors. Contrast factor.}
  }
}
\references{
    Box G. E. P., Hunter, W. C. and Hunter, J. S. (2004)
    \emph{Statistics for Experimenters II}. Wiley.
}
\examples{
library(OBsMD)
data(PB12Des,package="OBsMD")
str(PB12Des)
X <- as.matrix(PB12Des)
print(t(X)\%*\%X)
}
\keyword{datasets}
