\name{data.r}
\alias{data.r}
\docType{data}
\title{
Data range
}
\description{
CDC growth chart of heights of female children aged 0 to 120 months.
}
\usage{data("data.r")}
\format{
  A data frame with 121 observations on the following 3 variables.
  \describe{
    \item{\code{Age}}{a numeric vector}
    \item{\code{Per_5}}{a numeric vector}
    \item{\code{Per_95}}{a numeric vector}
  }
}
\details{
Range data needed for the simulated data.
}
\source{
http://www.cdc.gov/growthcharts/clinical_charts.htm
}

\examples{
data(data.r)
## plot Range boundary lines
tol <- 3
plot(data.r$Age, data.r$Per_5, type="l", lty=2, col=2)
lines(data.r$Age, data.r$Per_95, type="l", lty=2, col=2)
lines(data.r$Age, data.r$Per_5 - tol, type="l", lty=2, col=4)
lines(data.r$Age, data.r$Per_95 + tol, type="l", lty=2, col=4)
}
\keyword{datasets}
\keyword{CDC}
