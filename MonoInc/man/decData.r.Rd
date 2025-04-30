\name{decData.r}
\alias{decData.r}
\docType{data}
\title{
Data range(decreasing)
}
\description{
Chart of measurements of  children aged 0 to 120 months
}
\usage{data("decData.r")}
\format{
  A data frame with 121 observations on the following 3 variables.
  \describe{
    \item{\code{Age}}{a numeric vector}
    \item{\code{L.bound}}{a numeric vector}
    \item{\code{U.bound}}{a numeric vector}
  }
}
\details{
Range data needed for the simulated decreasing data.
}

\examples{
data(decData.r)

## plot Range boundary lines
tol <- 3
plot(decData.r[,1], decData.r[,2], type="l", lty=2, col=2)
lines(decData.r[,1], decData.r[,3], type="l", lty=2, col=2)
lines(decData.r[,1], decData.r[,2] - tol, type="l", lty=2, col=4)
lines(decData.r[,1], decData.r[,3] + tol, type="l", lty=2, col=4)
}
\keyword{datasets}
