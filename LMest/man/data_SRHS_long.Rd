\name{data_SRHS_long}

\alias{data_SRHS_long}

\docType{data}

\title{Self-reported health status dataset}

\description{Dataset about self-reported health status derived from the Health and Retirement Study conducted by the University of Michigan.}

\usage{data(data_SRHS_long)}

\format{
  A data frame with 56592 observations on the following 6 variables.
  \describe{
    \item{\code{t}}{occasion of observation}
    \item{\code{id}}{subject id}
    \item{\code{gender}}{sex of the subject coded as 1 for "male", 2 for "female"}
    \item{\code{race}}{race coded as 1 for "white", 2 for "black", 3 for "others"}
    \item{\code{education}}{educational level coded as 1 for "high school", 2 for "general educational diploma", 3 for "high school graduate", 4 for "some college", 5 for "college and above"}
    \item{\code{age}}{age at the different time occasions}
    \item{\code{srhs}}{self-reported health status at the different time occasions coded as 1 for "excellent", 2 for "very good", 3 for "good", 4 for "fair", 5 for "poor"}
  }
}

\references{Bartolucci, F., Bacci, S. and Pennoni, F. (2014) Longitudinal analysis of the self-reported health status by mixture latent autoregressive models, \emph{Journal of the Royal Statistical Society - series C}, \bold{63}, pp. 267-288}

\examples{data(data_SRHS_long)}

\keyword{datasets}
