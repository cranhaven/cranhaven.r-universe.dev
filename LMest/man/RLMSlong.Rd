\name{RLMSlong}
\alias{RLMSlong}

\docType{data}

\title{Dataset about job satisfaction}

%\description{Longitudinal dataset deriving from the Russia Longitudinal Monitoring Survey (RLMS) about job satisfaction measured by an ordinal variable with 5 categories.}

\description{Longitudinal dataset in long format deriving from the Russia Longitudinal Monitoring Survey (RLMS, from Round XVII to Round XXIII, collected from 2008 to 2014)  about job satisfaction measured by an ordinal variable at seven different occasions with five categories, \code{1} for “absolutely satisfied”, \code{2} for  “mostly satisfied”, \code{3} for “neutral”, \code{4} for “not very satisfied”, and \code{5} for “absolutely unsatisfied”.}

\usage{data(RLMSlong)}

\format{
  A data frame with 1718 observations on the following 7 variables.
  \describe{
   \item{\code{time}}{occasion of observation.}
   \item{\code{id}}{subject id.}
   \item{\code{rlms}}{see \code{\link{RLMSdat}}.}
   \item{\code{value}}{reported job satisfaction  at different time occasions coded as 1 for “absolutely satisfied”, 2 for  “mostly satisfied”, 3 for “neutral”, 4 for “not very satisfied”, 5 for “absolutely unsatisfied”.}
  }
}

\source{\url{http://www.cpc.unc.edu/projects/rlms-hse}, \url{https://www.hse.ru/org/hse/rlms}}

\references{Russia Longitudinal Monitoring survey, RLMS-HSE, conducted by Higher School of Economics and ZAO "Demoscope" together with Carolina Population Center, University of North Carolina at Chapel Hill and the Institute of Sociology RAS}

\examples{
data(RLMSlong)
}

\keyword{datasets}
