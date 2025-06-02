\name{data_drug}

\alias{data_drug}

\docType{data}

\title{Dataset about marijuana consumption}

\description{Longitudinal dataset derived from the National Youth Survey about marijuana consumption measured  by ordinal variables with 3 categories with increasing levels of consumption (1  "never in the past year", 2 "no more than once in a month in the past year", 3 "more than once a month in the past year").}

\usage{data(data_drug)}

\format{
  A data frame with 51 observations on the following 6 variables.
  \describe{
    \item{\code{V1}}{reported drug use at the 1st occasion}
    \item{\code{V2}}{reported drug use at the 2nd occasion}
    \item{\code{V3}}{reported drug use at the 3rd occasion}
    \item{\code{V4}}{reported drug use at the 4th occasion}
    \item{\code{V5}}{reported drug use at the 5th occasion}
    \item{\code{V6}}{frequency of the response configuration}
  }
}

\source{Elliot, D. S., Huizinga, D. and Menard, S. (1989) \emph{Multiple Problem Youth: Delinquency, Substance Use, and Mental
Health Problems.} New York: Springer.}

\references{Bartolucci, F. (2006) Likelihood inference for a class of latent Markov models under
linear hypotheses on the transition probabilities. \emph{Journal of the Royal Statistical Society,
series B}, \bold{68}, 155-178.}

\examples{
data(data_drug)
}

\keyword{datasets}
