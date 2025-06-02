\name{data_heart_sim}

\alias{data_heart_sim}

\docType{data}

\title{Health dataset}


\description{Simulated longitudinal dataset coming from a medical study to assess the health state progression of patients after a certain treatment.}

\usage{data(data_heart_sim)}

\format{
  A data frame referred to 125 units observed at 6 time occasions on the following variables:
  \describe{
   \item{\code{id}}{subject id}
   \item{\code{time}}{occasion of observation}
   \item{\code{sap}}{systolic arterial pressure in mmgh}
   \item{\code{dap}}{diastolic arterial pressure in mmgh}
   \item{\code{hr}}{heart rate in bpm}
   \item{\code{fluid}}{fluid administration in ml/kg/h}
   \item{\code{gender}}{1 for male, 2 for females}
   \item{\code{age}}{age in years}
  }
}


\references{Pennoni, F., Pandolfi, S. and Bartolucci, F. (2024), LMest: An R Package for Estimating Generalized Latent Markov Models, \emph{Submitted to the R Journal}, pp. 1-30.}

\examples{
data(data_heart_sim)
}

\keyword{datasets}
