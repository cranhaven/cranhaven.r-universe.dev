\name{preci}
\alias{preci}
\docType{data}
\title{
Empirical data related to rainfall
}
\description{
Empirically generated data in 10 arbitrary locations associated with
the rainfall variable
}
\usage{data(preci)}
\format{
  A data frame with 10 observations on the following 4 variables:
  \describe{
    \item{\code{Obs}}{a numeric vector; observation number}
    \item{\code{x}}{a numeric vector; x-coordinate; unknown reference }
    \item{\code{y}}{a numeric vector; y-coordinate; unknown reference }
    \item{\code{prec}}{a numeric vector; the target variable}
  }
}
\examples{
data(preci)
summary(preci)
}
\keyword{datasets}
