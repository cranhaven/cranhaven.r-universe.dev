\name{bodytemp}
\alias{bodytemp}
\docType{data}
\title{ Data on body temperature, heart rate, and gender of 130 human subjects. }
\description{
  These data were collected by Mackowiak, Wasserman, and Levine(1992),
  and have been used, among others, by Ntzoufras(2009).
}
\usage{data(bodytemp)}
\format{
  A data frame with 130 observations on the following 3 variables:
  \describe{
    \item{\code{temperature}}{body temperature in degrees Fahrenheit}
    \item{\code{gender}}{a factor with levels 'female' and 'male'}
    \item{\code{heart.rate}}{heart rate in beats per minute}
  }
}
\source{
  Mackowiak, P.A., Wasserman, S.S., and Levine, M.M.(1992) A critical
  appraisal of 98.6 degrees F, the upper limit of the normal body
  temperature, and other legacies of Carl Reinhold August
  Wunderlich. JASA 268, 1578--1580.
  
  Ntzoufras, I.(2009) Bayesian Modeling Using Winbugs. Wiley, Hoboken,
  N.J.
  
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\examples{
data(bodytemp)
B2Nsir(temperature ~ gender, bodytemp)
}
\keyword{datasets}
