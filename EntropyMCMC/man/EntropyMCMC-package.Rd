\name{EntropyMCMC-package}
\alias{EntropyMCMC-package}
%\alias{EntropyMCMC}
\docType{package}
\title{(A)MCMC Simulation and Convergence Evaluation using Entropy and
Kullback-Leibler Divergence  Estimation}
\description{Contains functions to analyse (Adaptive) Markov Chain Monte Carlo (MCMC) algorithms, evaluate their convergence rate, and compare candidate MCMC algorithms for a same target density, based on entropy and Kullback-Leibler divergence criteria. MCMC algorithms can be simulated using provided functions, or imported from external codes.
The diagnostics are based on consistent estimates of entropy and Kulback distance 
between the density at
iteration \eqn{t} and the target density \eqn{f}, based on iid (parallel) chains.}
\details{
\tabular{ll}{
Package: \tab EntropyMCMC\cr
Type: \tab Package\cr
Version: \tab 1.0.4\cr
Date: \tab 2019-03-08\cr
License: \tab GPL (>= 3)\cr
LazyLoad: \tab yes\cr
}

\bold{Statistical background:}

 

This package allows for simulation of standard or adaptive MCMC samplers for a user-defined
target density, and provides statistical tools to evaluate convergence of MCMC's 
and compare performance of algorithms for the same target density
(typically against benchmark samplers).

The criteria are graphical and based on plots against iterations (time) \eqn{t},
of the \emph{Kullback divergence} 
\eqn{K(p^t,f)}{K(pt,f)}
between the density \eqn{p^t}{pt} of the MCMC algorithm at time \eqn{t}, 
and the target density \eqn{f}, for \eqn{t=1} up to the number of iterations 
that have been simulated.
This requires estimation of the  entropy of  \eqn{p^t}{pt},
\deqn{E_{p^t} [\log(p^t)],}{E_pt [log(pt)],}
and of the external entropy  
\deqn{E_{p^t} [\log(f)].}{E_pt [log(f)].}

Consistent estimates are computed based on \eqn{N} iid (parallel) chains, 
since the \eqn{N} positions of the chains at iterations \eqn{t} 
forms a \eqn{N}-iid sample from the density \eqn{p^t}{pt}.

\bold{Computational considerations:}


The simulation of iid chains can be performed in this package, which provides a mechanism 
for defining (A)MCMC algorithms and building the iid chains required for convergence evaluation.
Each MCMC algorithm is defined by a list with five elements.
Each user can define its own MCMC, starting from the standard MCMC algorithms
that are already defined:

\itemize{

\item \code{RWHM}: a standard Randow-Walk Hastings-Metropolis (HM) algorithm.

\item \code{HMIS_norm}: an Independence Sampler HM with gaussian proposal

\item \code{AMHaario}: the
Haario (2001) Adaptive Hastings-Metropolis algorithm, provided as an example 
of a standard AMCMC.

\item \code{IID_norm}: a \dQuote{fake} MCMC that is just a gaussian IID sampler, used mostly
for testing purpose. Simulation of \eqn{N} iid chains for \eqn{n} iterations using this algorithm just returns \eqn{N\times n}{N.n} gaussian \eqn{d}-dimensional vectors.
}
Functions for doing the simulations and the convergence evaluation
automatically using these algorithms in their first argument are provided.
Two strategies are available:

\itemize{

\item \emph{Simulation and Kullback estimation separately:}
A \dQuote{cube} of \eqn{N} chains for \eqn{n} iterations in a space of dimension \eqn{d}
is first simulated and stored using \code{\link{MCMCcopies}} or its multicore or cluser versions, 
then the entropy and Kullback divergence 
are estimated from that object using \code{\link{EntropyMCMC}} or its multicore version.

\item \emph{Simulation and Kullback estimation simultaneously:}
  For each iteration \eqn{t}, the next step of all the  \code{N}
  chains are generated,
	then the Entropy and Kullback divergence \eqn{K(p^t,f)}{K(pt,f)}
	are estimated, and the past of the parallel chains 
	is discarded so that the amount of memory requirement is kept small, and
	only entropy-related estimates are stored and returned. Functions for this strategy are
 \code{\link{EntropyParallel}} and its multicore and cluster version.
}
See the Examples section of \code{\link{plot_Kblist}} for an illustration of these two methods.


\bold{Doing the simulations outside from this package}

A third hybrid strategy is also available:
the simulation of iid chains can be done using an external code
(in \R, \code{C} or any language) and imported 
in the \pkg{EntropyMCMC} package (defining an object of the appropriate class 
\code{"plMCMC"} and structure, see \code{\link{MCMCcopies}}).

Then the Kullback divergence criterion can be computed using \code{\link{EntropyMCMC}} 
or its multicore version, and convergence/comparison diagnostics can be displayed 
using the associated \code{plot} method.




\bold{About High Performance Computing}

The required simulations can be done using singlecore or
HCP (multicore computers, snow or clusters using the \pkg{\link{parallel}} or
\pkg{Rmpi} pakages). Note that the \pkg{\link{parallel}} package using socket cluster is not 
available on Windows machines.


}


\author{
Didier Chauveau,
Institut Denis Poisson, 
  University of Orleans, CNRS, Orleans France.
  \url{https://www.idpoisson.fr/chauveau/}

Maintainer: Didier Chauveau \email{didier.chauveau@univ-orleans.fr}

Contributor: Houssam Alrachid
}

\references{
\itemize{
   \item Chauveau, D. and Vandekerkhove, P. (2013), 
        Smoothness of Metropolis-Hastings algorithm and application to entropy estimation.
        \emph{ESAIM: Probability and Statistics},  \bold{17}, 419--431.
        DOI: \url{http://dx.doi.org/10.1051/ps/2012004}

  \item Chauveau D. and Vandekerkhove, P. (2014),
        Simulation Based Nearest Neighbor Entropy Estimation for (Adaptive) MCMC Evaluation,
    In \emph{JSM Proceedings, Statistical Computing Section}.
    Alexandria, VA: American Statistical Association. 2816--2827.

  \item Chauveau D. and Vandekerkhove, P. (2014),
        The Nearest Neighbor entropy estimate: an adequate tool for adaptive MCMC evaluation.
        \emph{Preprint HAL} \url{http://hal.archives-ouvertes.fr/hal-01068081}.
   }
}

\keyword{package}
