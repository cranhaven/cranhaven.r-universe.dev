\name{Bayesian binomial analysis}
\alias{Bbinom}
\alias{Bbinomial}


\title{
Bayesian analysis of binomial data
}
\description{
Draws random values from the posterior for a binomial likelihood and beta prior. \code{Bbinom} is deprecated and will be removed in the next version; use \code{Bbinomial} instead.
}
\usage{
Bbinom(...)
Bbinomial(y, n, priors=NULL, draws=100000, ...)
}
\arguments{
  \item{y}{
the number of successes
}
  \item{n}{
the number of trials
}
  \item{priors}{
an optional list with elements specifying the priors for the mode and concentration of the beta prior distribution; see Details.
}
  \item{draws}{
the number of MCMC draws to be returned.
}
  \item{...}{
other arguments to pass to the function.
}
}

\details{
The function generates a vector of random draws from the posterior distribution of the probability of success. It uses conjugacy to determine the parameters of the posterior beta distribution, and draws independent values from this.

A prior can be specified with the \code{priors} argument. A beta prior is used, specified by mode, \code{mode}, and concentration, \code{conc}.

When \code{priors = NULL} (the default), a uniform prior corresponding to beta(1, 1) is used.
}

\value{
Returns an object of class \code{mcmcOutput}.
}

\author{
Mike Meredith.
}

\examples{
# Generate a sample from a binomial distribution, maybe the number of sites
#   where a species was detected:
n <- 10  # number of trials (sites visited)
( y <- rbinom(1, n, 0.75) ) # number of successes (sites where species detected)
Bbinomial(y, n)  # with uniform prior
Bbinomial(y, n, priors=list(mode=0.4, conc=5))  # with informative prior
}

