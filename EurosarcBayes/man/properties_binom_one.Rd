\name{properties_binom_one}
\alias{properties_binom_one}
\title{
Properties for single-arm single binomial endpoint trial design
}
\description{
Get frequentist and Bayesian properties for a single-arm single binomial endpoint trial design.
}
\usage{
properties_binom_one(failure = NULL, success = NULL, reviews = NULL, 
	p0, p1, prior.a = 0, prior.b = 0, round = TRUE, cutpoints = NULL)
}
\arguments{
  \item{failure}{Vector of failures needed to stop the trial}
  \item{success}{Vector of successes needed to stop the trial}
  \item{reviews}{Vector of the number of patients at each analysis}
  \item{p0}{probability of success under H0}
  \item{p1}{probability of success under H1}
  \item{prior.a,prior.b}{beta prior parameters}
  \item{round}{Option whether to round results or not}
  \item{cutpoints}{Alternative usage, this replaces failure, success and reviews with a data.frame with columns of the same names}
}
\value{
Returns an object of class \code{\linkS4class{trialDesign_binom_one}}.
}
