\name{data_long_cont}
\alias{data_long_cont}

\docType{data}

\title{Multivariate Longitudinal Continuous (Gaussian) Data}


\description{Simulated multivariate longitudinal continuous dataset assuming that there are 500 subjects in the study whose data are collected at 5 equally-spaced time points.}

\usage{data(data_long_cont)}

\format{
 A data frame with 2500 observations on the following 7 variables.
  \describe{
   \item{\code{id}}{subject id.}
   \item{\code{time}}{occasion of observation.}
   \item{\code{Y1}}{a numeric vector for the first longitudinal response.}
   \item{\code{Y2}}{a numeric vector for the second longitudinal  response.}
   \item{\code{Y3}}{a numeric vector for the third longitudinal  response.}
   \item{\code{X1}}{a numeric vector for the first covariate.}
   \item{\code{X2}}{a numeric vector for the second covariate.}}
}

\examples{
data(data_long_cont)
}

\keyword{datasets}
