\name{plot_Kblist}
\alias{plot_Kblist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot sequences of Kullback distance estimates for comparison of several MCMC algorithms for a same target density
}
\description{
	This  function draws on a same plot several sequences of estimates of 
	Kullback distances \eqn{K(p^t,f)}{K(pt,f)}, i.e. the convergence criterion vs. time (iteration \eqn{t}), 
	for each MCMC algorithm for which the convergence criterion has been computed.
	}
\usage{
plot_Kblist(Kb, which = 1, lim = NULL, ylim = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Kb}{A list of objects of class \code{"KbMCMC"}, such as the ones returned by
  			\code{\link{EntropyMCMC}} or \code{\link{EntropyParallel}}, or their HPC versions. }
  \item{which}{Controls the level of details in the legend  added to the plot (see details)}
  \item{lim}{for zooming over \code{1:lim} iterations only. }
  \item{ylim}{limits on the \eqn{y} axis for zooming, passed to \code{plot}. }
}

\details{
  The purpose of this plot if to compare \eqn{K} MCMC algorithms (typically based on \eqn{K} different 
  simulation strategies or kernels) for convergence or efficiency in estimating a same target density \eqn{f}. 
  For the \eqn{k}th algorithm, the user has to generate the convergence criterion,
  i.e. the sequence  \eqn{K(p^t(_k)k), f)}{K(pt(k), f)} for \eqn{t=1} up to the number of iterations 
  that has been chosen, and where \eqn{p^t(k)}{pt(k)} is  the estimated pdf of the algorithm at time \eqn{t}.
	
	For the legend, \code{which=1} displays the MCMC's names together with some technical information depending on the algorithms definition (e.g. the proposal variance for the \code{\link{RWHM}} algorithm) and the 
	method used for entropy estimation. The legend for
  \code{which=2} is shorter, only displaying the MCMC's names together with the number of parallel chains used for each, 
  typically to compare the effect of that number for a single MCMC algorithm.
}



\value{The graphic to plot.}

\references{
\itemize{
   \item Chauveau, D. and Vandekerkhove, P. (2012), 
        Smoothness of Metropolis-Hastings algorithm and application to entropy estimation.
        \emph{ESAIM: Probability and Statistics},  \bold{17}, (2013) 419--431.
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

\author{Didier Chauveau.}


\seealso{\code{\link{EntropyMCMC}}, \code{\link{EntropyMCMC.mc}}}

\examples{
\donttest{
## Toy example using the bivariate centered gaussian target
## with default parameters value, see target_norm_param
d = 2           # state space dimension
n=300; nmc=100  # number of iterations and iid Markov chains
## initial distribution, located in (2,2), "far" from target center (0,0)
Ptheta0 <- DrawInit(nmc, d, initpdf = "rnorm", mean = 2, sd = 1) 

## MCMC 1: Random-Walk Hasting-Metropolis
varq=0.05 # variance of the proposal (chosen too small)
q_param=list(mean=rep(0,d),v=varq*diag(d))

## using Method 1: simulation with storage, and *then* entropy estimation
# simulation of the nmc iid chains, single core here
s1 <- MCMCcopies(RWHM, n, nmc, Ptheta0, target_norm,
                 target_norm_param, q_param)
summary(s1) # method for "plMCMC" object
e1 <- EntropyMCMC(s1) # computes Entropy and Kullback divergence

## MCMC 2: Independence Sampler with large enough gaussian proposal
varq=1; q_param <- list(mean=rep(0,d),v=varq*diag(d))

## using Method 2: simulation & estimation for each t, forgetting the past
## HPC with 2 cores here (using parallel socket cluser, not available on Windows machines)
e2 <- EntropyParallel.cl(HMIS_norm, n, nmc, Ptheta0, target_norm,
                      target_norm_param, q_param, 
                      cltype="PAR_SOCK", nbnodes=2)

## Compare these two MCMC algorithms
plot_Kblist(list(e1,e2)) # MCMC 2 (HMIS, red plot) converges faster.
  }
}

\keyword{file}




