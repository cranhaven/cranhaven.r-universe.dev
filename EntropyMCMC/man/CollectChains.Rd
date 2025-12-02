\name{CollectChains}
\alias{CollectChains}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Collect MCMC chains in a single object}
\description{Utility function for the package \pkg{EntropyMCMC}.}

\usage{
CollectChains(s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{ An object of class \code{plMCMC}, such as the one 
  returned by \code{\link{MCMCcopies}}, containing in particular an array of dimension 
  \eqn{(n,d,nmc)}
  holding the simulation of \eqn{n} steps of \eqn{nmc} parallel chains in dimension \eqn{d}.}
%%     ~~Describe \code{s} here~~
}

\details{
Concatenates all simulated copies together in one matrix of dimension \code{(n*nmc,d)}.}

\value{
Returns a matrix of dimension \code{(n*nmc,d)}.
}

\author{Didier Chauveau.}

\keyword{file}
