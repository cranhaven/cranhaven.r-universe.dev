\name{RLMSdat}

\alias{RLMSdat}

\docType{data}

\title{Dataset about job satisfaction}

\description{Longitudinal dataset deriving from the Russia Longitudinal Monitoring Survey (RLMS) about job satisfaction measured by an ordinal variable at seven different occasions with five categories, \code{1} for “absolutely satisfied”, \code{2} for  “mostly satisfied”, \code{3} for “neutral”, \code{4} for “not very satisfied”, and \code{5} for “absolutely unsatisfied”.}

\usage{data(RLMSdat)}

\format{
  A data frame with 1718 observations on the following 7 variables.
  \describe{
    \item{\code{IKSJQ}}{reported job satisfaction at the 1st occasion}
    \item{\code{IKSJR}}{reported job satisfaction at the 2nd occasion}
    \item{\code{IKSJS}}{reported job satisfaction at the 3rd occasion}
    \item{\code{IKSJT}}{reported job satisfaction at the 4th occasion}
    \item{\code{IKSJU}}{reported job satisfaction at the 5th occasion}
    \item{\code{IKSJV}}{reported job satisfaction at the 6th occasion}
    \item{\code{IKSJW}}{reported job satisfaction at the 7th occasion}

  }
}

\source{\url{http://www.cpc.unc.edu/projects/rlms-hse}, \url{https://www.hse.ru/org/hse/rlms}}

\references{Russia Longitudinal Monitoring survey, RLMS-HSE, conducted by Higher School of Economics and ZAO "Demoscope" together with Carolina Population Center, University of North Carolina at Chapel Hill and the Institute of Sociology RAS}

\examples{
data(RLMSdat)
}

\keyword{datasets}
