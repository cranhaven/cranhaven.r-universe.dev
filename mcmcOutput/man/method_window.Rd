\name{window.mcmcOutput}
\alias{window.mcmcOutput}
\title{
Subsetting chains for \code{mcmcOutput} objects
}
\description{
The \code{window} method extracts the subset of the draws between \code{start} and \code{end}. Setting \code{thin = k} selects every \code{k}th observation starting from \code{start}.

Use \code{window} to discard additional draws at the start of the chain if extra burn-in is required, or to reduce the size of the object by thinning. See the examples.

Any previous burn-in or thinning is ignored (unlike the \code{coda::window.mcmc} method).
}
\usage{
\method{window}{mcmcOutput}(x, start=1, end=NULL, thin=1, ...)

}
\arguments{
  \item{x}{
an object of class \code{mcmcOutput}.
}
  \item{start}{
the first observation to retain.
}
  \item{end}{
the last observation to retain; if NULL, this is set to the end of the chain.
}
  \item{thin}{
the interval between retained observations.
}
  \item{\dots}{
further arguments for other methods (not used).
}
}

\value{
Returns an object of class \code{mcmcOutput} with the subset of observations.
}
\author{
Mike Meredith.
}

\examples{
data(mcmcListExample)
mco <- mcmcOutput(mcmcListExample)
mco

new1 <- window(mco, start=201) # Discard the first 200 draws in each chain
new1  # Now only 800 per chain.

new2 <- window(mco, thin=3) # Retain only 1/3 of the draws
new2  # new2 is smaller; each chain reduced from 1000 to 333, total draws 999.

}
\keyword{methods}
