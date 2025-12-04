\name{Bayesian Poisson analysis}
\alias{Bpoisson}


\title{
Bayesian analysis of count data
}
\description{
Generates random draws from the posterior for a Poisson likelihood and gamma prior.
}
\usage{
Bpoisson(y, n, priors=NULL, draws=10000, ...)
}
\arguments{
  \item{y}{
the count
}
  \item{n}{
the sample size
}
  \item{priors}{
an optional list with elements specifying the priors for the mode and SD of the gamma prior distribution; see Details.
}
  \item{draws}{
the number of MCMC draws to be returned.
}
  \item{...}{
additional arguments to pass to the function.
}
}

\details{
The function generates a vector of random draws from the posterior distribution of the probability of the observed count. It uses conjugacy to determine the parameters of the posterior gamma distribution, and draws independent values from this.

A prior can be specified with the \code{priors} argument. A gamma prior is used, specified by mode, \code{mode}, and SD, \code{sd}.

When \code{priors = NULL} (the default), a uniform prior corresponding to gamma(1, 0) is used.
}

\value{
Returns an object of class \code{mcmcOutput}.
}

\author{
Mike Meredith.
}

\examples{
# Generate a sample from a Poisson distribution, maybe the number of ticks
#   observed on a sample of rodents:
n <- 10  # number of trials (rodents examined)
( y <- rpois(n, 1.2) ) # number of ticks on each rodent
Bpoisson(sum(y), n)  # with uniform prior
plot(Bpoisson(sum(y), n))
Bpoisson(sum(y), n, priors=list(mode=1, sd=3))  # with informative prior
plot(Bpoisson(sum(y), n, priors=list(mode=1, sd=3)))
}
