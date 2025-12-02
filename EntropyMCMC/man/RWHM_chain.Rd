\name{RWHM_chain}
\alias{RWHM_chain}
\alias{HMIS_norm_chain}
\alias{AMHaario_chain}
\alias{IID_chain}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulating MCMC single chains using MCMC algorithms}
\description{
These functions are used to define the elements \code{$chain} of the MCMC algorithms
that are (and must be) implemented as lists in \pkg{EntropyMCMC}.
These functions are usually only called by higher-level functions, see details below. 
}

\usage{
RWHM_chain(theta0, it = 100, target, f_param, q_param, q_pdf = gaussian_pdf, 
                q_proposal = gaussian_proposal) 
HMIS_norm_chain(theta0, it = 100, target, f_param, q_param, q_pdf = q_pdf_ISnorm, 
                q_proposal = q_proposal_ISnorm)
AMHaario_chain(theta0, it = 100, target, f_param, q_param, q_pdf = gaussian_pdf,
                q_proposal = gaussian_proposal)
IID_chain(theta0 = NULL, it = 100, target, f_param, q_param = NULL, q_pdf = NULL,
                q_proposal = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{it}{the number of iterations to simulate}
  \item{theta0}{the initial position of the chain, a \eqn{d}-dim vector}
  \item{target}{the user-defined target density}
  \item{f_param}{the parameters (hyperparameters, data) of the user-defined target density}
  \item{q_param}{the parameters of the proposal density, which structure depends on the algorithm and the proposal density chosen by the user. Defaults are
  for \code{RWHM}: a list with the mean and covariance matrix of the proposal.
  For \code{AMHaario}: a list that must contain three elements:
  \code{v} the initial covariance matrix, \code{t0} the iteration of the end of initial stage 
  with that matrix, and \code{epsi} the epsilon parameter (for the nondegenerate matrix part),
  see Haario et. al.(2001).}
  \item{q_pdf}{the proposal density}
  \item{q_proposal}{the function simulating the proposal for the next move}
}

\details{Each MCMC algorithm is defined as a list with five elements, see 
the object \code{RWHM} for an example. The element \code{$chain} must provide
the name of the function performing simulation of a single chain and returning that chain,
with arguments that must follow the definition above.
Each user can define its own MCMC
starting with the algorithms provided (see also section below).
These functions are thus usually called by higher-level functions like
\code{MCMCcopies}, \code{EntropyParallel}, or their multicore versions,
for simulating copies of MCMC chains in an automatic manner.

\itemize{
\item \code{RWHM_chain} is used in \code{RWHM}, a standard Randow-Walk Hastings-Metropolis algorithm.

\item \code{HMIS_norm_chain} is used in \code{HMIS_norm},
an Independence Sampler HM with gaussian proposal

\item \code{AMHaario_chain} is used in \code{AMHaario}, the
Haario Adaptive Hastings-Metropolis algorithm (Haario 2001), and is provided as an example 
of a benchmark AMCMC.

\item \code{IID_chain} is used in \code{IID_norm}, a \dQuote{fake} MCMC that is just a gaussian IID sampler.
}
}

\value{
A list with elements:
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{theta}{the simulated chain in an array of \eqn{it} rows and \eqn{d} columns (the dimension)} 
\item{paccept}{the empirical acceptance rate}
\item{finalcov}{the last covariance matrix}
\item{algo}{the name of the algorithm (for plot methods)}

}

\references{
H. Haario, E. Saksman, and J. Tamminen (2001), 
An adaptive Metropolis algorithm. 
Bernoulli 7, 223--242.
}

\author{Didier Chauveau.}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
The algorithm already implemented, listed in \code{\link{EntropyMCMC-package}}.

The higher level functions that use these functions for simulation:
\code{\link{MCMCcopies}}, \code{\link{EntropyParallel}} and their multicore versions.
}

\keyword{file}
