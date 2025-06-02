\name{NLSYlong}
\alias{NLSYlong}

\docType{data}

\title{National Longitudinal Survey of Youth data}


\description{Longitudinal dataset in long format deriving  from the National Longitudinal Survey of Youth with information about 581 individuals  followed from 1990 to 1994.}

\usage{data(NLSYlong)}

\format{
  A data frame with 1743 observations on the following 12 variables.
  \describe{
   \item{\code{momage}}{mother's age at birth.}
   \item{\code{gender}}{0 if male, 1 if female.}
   \item{\code{childage}}{child's age at first interview.}
   \item{\code{hispanic}}{1 if child is Hispanic, 0 if not.}
    \item{\code{black}}{1 if child is black, 0 if not.}
    \item{\code{momwork}}{1 if mother works, 0 if not.}
    \item{\code{married}}{1 if parents are married, 0 if not.}
    \item{\code{time}}{occasion of observation.}
    \item{\code{anti}}{a measure of antisocial behavior measured on a scale from 0 to 6.}
    \item{\code{self}}{a measure of self-esteem measured on a scale from 6 to 24.}
    \item{\code{pov}}{a time varying variable assuming value 1 if family is in poverty, 0 if not.}
    \item{\code{id}}{subject id.}
  
  }
}

\source{\url{https://www.nlsinfo.org/content/cohorts/nlsy79}}

\references{The wide format of this dataset is downloadable from the package `panelr`.}

\examples{
data(NLSYlong)
}

\keyword{datasets}
