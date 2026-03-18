\name{contingency}
\alias{contingency}
\docType{data}
\title{ Contingency Table from a sample Survey  }
\description{
  Contingency table describing a survey of sources and quality of information
  about cancer for 1729 people.  
}
\usage{data(contingency)}
\format{
  A data frame with 32 observations on the following 6 variables.
  \describe{
    \item{\code{source.1}}{a factor with levels \code{N} \code{Y}}
    \item{\code{source.2}}{a factor with levels \code{N} \code{Y}}
    \item{\code{source.3}}{a factor with levels \code{N} \code{Y}}
    \item{\code{source.4}}{a factor with levels \code{N} \code{Y}}
    \item{\code{knowledge}}{a factor with levels \code{Good} \code{Poor}}
    \item{\code{count}}{number of respondents with this pattern}
  }
}
\details{
  The sources of information are: (1) news media, (2) light reading, 
  (3)  solid reading, (4)  lectures.  The book (page 437) is fitting
  Bayesian loglinear models.
}
\examples{
data(contingency)

}
\keyword{datasets}
\concept{loglinear model}

