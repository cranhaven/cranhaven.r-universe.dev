\name{gaussian_pdf}
\alias{gaussian_pdf}
\alias{gaussian_proposal}


\title{Proposal density evaluation and simulation}
\description{Functions for proposal density evaluation and random generation in MCMC algorithms,
in the case where these are Gaussian.}

\usage{
gaussian_pdf(y, x, param)

gaussian_proposal(x, param)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{Candidate for next move, a vector of dimension \eqn{d}}
  \item{x}{Current position of a chain, a vector of dimension \eqn{d}}
  \item{param}{The proposal parameters, that must contains the 
  \eqn{d\times d}{d x d} variance matrix
  in \code{param$v}.}
}
\details{
The Gaussian proposal density \eqn{q(y|x)} used in, e.g.,
random walk Hastings-Metropolis algorithm \code{RWHM}
is the multivariate Gaussian \eqn{N(x,v)} density evaluated at point \eqn{y}. 
Similarly, the Gaussian proposal (next move) is a random draw 
\eqn{y \sim N(x,v)}{y ~ N(x,v)} when the chain is at position \eqn{x}.
}

\note{These functions are calling multivariate 
Gaussian density and random generation functions imported from the 
\pkg{mixtools} package (chosen for efficiency) and wrapped
in the format required by the \pkg{EntropyMCMC} package.
}


\value{The value of the density, or the random draw, both in dimension \eqn{d}}

\author{Didier Chauveau.}



%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ gaussian }
