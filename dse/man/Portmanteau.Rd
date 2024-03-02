\name{Portmanteau}
\alias{Portmanteau}
\title{Calculate Portmanteau statistic}
\description{Calculate Portmanteau statistic.}
\usage{
    Portmanteau(res)}
\arguments{
    \item{res}{A matrix with time-series residuals in columns.}
}
\seealso{
\code{\link{informationTests}}
}
\examples{
require("stats")
Portmanteau(matrix(rnorm(200), 100,2)) # but typically with a residual
}
\concept{DSE}
\keyword{ts}

