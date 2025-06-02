\name{data_employment_sim}

\alias{data_employment_sim}

\docType{data}

\title{Employment dataset}


\description{Simulated dataset related to a survey on the employment status of a cohort of graduates.}

\usage{data(data_employment_sim)}

\format{
  A data frame with 585  observations on the following variables:
  \describe{
   \item{\code{id}}{subject id.}
    \item{\code{time}}{occasion of observation.}
   \item{\code{emp}}{0 if unemployed, 1 if employed.}
   \item{\code{area}}{1  if graduated in the South area, 2 if  graduated in the North area.}
   \item{\code{grade}}{1 if grade at graduation is low, 2 if it is medium, 3 if it is high.}
   \item{\code{edu}}{1 if parents hold a university degree, 0 if not.}
  }
}


\references{Pennoni, F., Pandolfi, S. and Bartolucci, F. (2024), LMest: An R Package for Estimating Generalized Latent Markov Models, \emph{Submitted to the R Journal}, pp. 1-30.}

\examples{
data(data_employment_sim)
}

\keyword{datasets}
