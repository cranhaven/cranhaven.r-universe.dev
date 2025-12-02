\name{DrawInit}
\alias{DrawInit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random draws for initialization}

\description{Utility function for the package \pkg{EntropyMCMC}, for generating random 
starting positions for the parallel Markov chains, used by, e.g., 
\code{\link{MCMCcopies}} or \code{\link{EntropyParallel}}.}
\usage{
DrawInit(nmc, d, initpdf="rnorm", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nmc}{Number of parallel chains = initial points.
}
  \item{d}{Space dimension.
}
  \item{initpdf}{Random generator. Generators currently implemented are: "rnorm" as the Normal distribution and "runif" as the uniform distribution.

}
  \item{...}{Parameters passed to initpdf}
}

\value{
\code{DrawInit} returns a matrix of dimension (nmc,d) where each row is a
\eqn{d}-dimensional point.
}

\author{
Didier Chauveau.}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{\code{\link{MCMCcopies}} and 
		\code{\link{MCMCcopies.mc}} for iid MCMC simulations,
		 \code{\link{EntropyParallel}} and \code{\link{EntropyParallel.cl}}
		 for simultaneous simulation and entropy estimation.}

\note{It is better for mixing properties to use diffuse initial distributions, such as the one 
proposed here. However Dirac initial points can also be used, precisely to evaluate the 
efficiency of a MCMC to escape from a wrong initial position (e.g., in the tails of the target density).}

\examples{
Ptheta0 <- DrawInit(10, 5, initpdf="rnorm", mean=0, sd=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file} % use one of  RShowDoc("KEYWORDS")
