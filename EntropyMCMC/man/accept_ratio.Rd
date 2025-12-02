\name{accept_ratio}
\alias{accept_ratio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Acceptance ratio for Hastings-Metropolis simulated MCMC chains}

\description{Internal function for the package \code{EntropyMCMC}, computes the acceptance ratio required in the definition of any Hastings-Metropolis algorithm.}


\usage{
accept_ratio(x, y, target, q_pdf, f_param, q_param, symmetric = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The current position.
}
  \item{y}{The next (proposal) position.
}
  \item{target}{
The target density for which the MCMC algorithm is defined; 
  		may be given only up to a multiplicative constant for most MCMC. 
  		Target must be a function such as the multidimensional gaussian
  		\code{target_norm(x,param)}.
  		}
  \item{q_pdf}{The  density of the proposal.
}
  \item{f_param}{
A list holding all the necessary target parameters, 
  		consistent with the target definition.
  		}
  \item{q_param}{
A list holding all the necessary parameters 
  		for the proposal density of the MCMC algorithm \code{mcmc_algo}.
  		}
  \item{symmetric}{
If \code{TRUE}, the proposal \code{q_pdf} is symmetric which simplifies the acceptance ratio compuatation}
}
\details{
The \code{accept_ratio} is used to decide whether to accept or reject a candidate 
\eqn{y}. The acceptance ratio indicates how probable the new proposed candidate is with respect to the current candidate \eqn{x}, according to the distribution \code{target}.
}
\value{
%%  ~Describe the value returned
\code{accept_ratio} returns a real value \code{alpha}, which indicates the computed value of the current \code{accept_ratio}.
}

\author{Didier Chauveau, Houssam Alrachid.}


\keyword{file}
