\name{EntropyParallel}
\alias{EntropyParallel}
\alias{EntropyParallel.cl}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parallel simulation and Entropy estimation of MCMC's
	- single core and cluster versions
}
\description{This function simulates \dQuote{parallel chains} (iid copies) 
	of a MCMC algorithm, i.e.
	for each \dQuote{time} iteration \eqn{t} the next step of all the  \code{nmc}
  chains are generated,
	then the Entropy of the density \eqn{p^t}{pt} of the algorithm at iteration
	\eqn{t}, 
	\eqn{E_{p^t}[\log(p^t)]}{E_pt[log(pt)]},
	and the Kullback divergence between \eqn{p^t}{pt} and the target density
	are estimated, based on these \code{nmc} steps iid from \eqn{p^t}{pt}.
	By default \code{keep.all = FALSE} i.e. the past of the parallel chains 
	is discarded so that the amount of memory requirement is kept small, and
	only entropy-related estimates are returned. 
	If \code{keep.all = TRUE}, the entire set of chains trajectories
	is saved in an array of dimensions \code{(n,d,nmc)}, such as the one
	returned by \code{\link{MCMCcopies}} or \code{\link{MCMCcopies.cl}}.

	A version of this function implementing several HPC (parallel) computing
	strategies is available (see details).
}
\usage{
EntropyParallel(mcmc_algo, n = 100, nmc = 10, Ptheta0, target, f_param, q_param,
          method = "A.Nearest.Neighbor",k=1, trim = 0.02, keep.all = FALSE,
          verb = TRUE, EntVect = FALSE)

EntropyParallel.cl(mcmc_algo, n = 100, nmc = 10, Ptheta0, target, f_param, q_param,
          method = "A.Nearest.Neighbor",k=1, eps = 0, trim=0.02,
          verb=TRUE, EntVect = FALSE, cltype="PAR_SOCK", nbnodes = 4,
          par.logf = FALSE, uselogtarget = FALSE, logtarget = NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mcmc_algo}{a list defining an MCMC algorithm in terms of the 
  	    functions it uses, such as \code{RWHM}, see details below.}

 \item{n}{The number of (time) iterations of each single chain to run.}

 \item{nmc}{The number of iid copies of each single chain.}

  \item{Ptheta0}{A \code{(nmc,d)} matrix, with the ith row giving a d-dimensional 
  	initial theta values for the ith chain.}

  \item{target}{The target density for which the MCMC algorithm is defined; 
  		may be given only up to a multiplicative constant for most MCMC. 
  		target must be a function such as the multidimensional gaussian
  		\code{target_norm(x,param)} with argument and parameters passed 
  		like in the example below.}

  \item{f_param}{A list holding all the necessary target parameters,
  including the data in an actual Bayesian model, and
  		consistent with the target definition.}

  \item{q_param}{A list holding all the necessary parameters 
  		for the proposal density of the MCMC algorithm \code{mcmc_algo}.}

  \item{method}{The method for estimating the entropy 
  \eqn{E_{p^t}[\log(p^t)]}{E_pt[log(pt)]}. 
  	The methods currently  implemented for this function are
  	\code{"Nearest.Neighbor"} as in Kozachenko and Leonenko (1987), 
  	\code{"k.Nearest.Neighbor"} as in      
  	Leonenko et al. (2005) (the default in the single core version),	and 
  	\code{"A.Nearest.Neighbor"} which is as \code{"k.NearestNeighbor"} using the
  	\pkg{RANN} package for (Approximate) fast computation of nearest neighbors, 
  	instead of R code (the default for the cluster version).
  	Other methods such as  \code{"Gyorfi.trim"} subsampling method as defined in 
  	Gyorfi and Vander Mulen (1989) are available as well
  	(see Chauveau and Vandekerkhove (2012)).}

\item{k}{The k-nearest neighbor index, the default is \eqn{k=1}.}

\item{eps}{Error bound: default of 0.0 implies exact nearest neighbour search, see
the \pkg{RANN} package.}

  \item{trim}{not used in this implementation, only for \code{method="Gyorfi.trim"}}
  
  \item{keep.all}{If \code{TRUE}, all the simulated chains are stored in a 3-dimensional
  	array of dimensions \code{(n,d,nmc)}, such as the one returned by
  	\code{\link{MCMCcopies}}}
  
  \item{verb}{Verbose mode for summarizing output during the simulation.}
  
    \item{EntVect}{If \code{FALSE} (the default), the entropy is computed only on the kth-nearest neighbor. If \code{TRUE},  the entropy is computed for all j-NN's for \eqn{j=1} to \eqn{k} (the latter being mostly for testing purposes).}


  \item{cltype}{Character string specifying the type of cluster; 
  	currently implemented 
  	types are: "PAR_SOCK" for socket cluster with \code{parallel} library, the default;
				"SNOW_SOCK" for socket cluster with \code{snow} library, and
				"SNOW_RMPI" for \code{snow} MPI cluster with \code{Rmpi} library.}
 
  \item{nbnodes}{The number of nodes or virtual cores requested to run the \code{nmc}
  	simulations in parallel. For the snow version, defaults to all; 
  	for the cluster version, defaults to 4.}

  \item{par.logf}{if \code{TRUE}, then the computation of the log of the target density
  	 at each of the  \code{nmc} chain locations, needed for the NN procedure is also
  	executed in parallel using \code{parRapply()}. 
  	This may speed up the process if the target is complicated
  	i.e. takes some time to evaluate. If the target is simple enough
  	(like \code{target_norm}), then communications between nodes are slower than
  	computations, in which case \code{par.logf = FALSE} (the default) should be preferred.}
  	
  \item{uselogtarget}{Set to \code{FALSE} by default; 
    useful in some cases where \eqn{log(f(\theta))} returns \code{-Inf} values in 
    Kullback computations because   
    \eqn{f(\theta)} itself returns too small values for some \eqn{\theta} far from modal regions.
    In these case using a function computing the logarithm of the target can remove the infinity values.}

    \item{logtarget}{The function defining \eqn{log(f(theta))}, \code{NULL} by default, 
    required if \code{uselogtarget} equals \code{TRUE}.
    This option and \code{uselogtarget} are currently implemented only for the "A.Nearest.Neighbor" method,
    and for the default \code{EntVect = FALSE} option.}
}



\details{
\bold{About parallel computing:}

  For the HPC (parallel) version, the computation of the \code{nmc} chains next step
	are done by the cluster nodes:
	 \code{EntropyParallel.cl} is a generic \emph{cluster} version implementing 
	 several types of cluster for running on a single, multicore computer
	 or on a true cluster using MPI communications. It is under development and may not 
	 work on all platform/OS. For instance the parallel socket cluster version 
	 does not work on Windows machines (see the \pkg{parallel} package documentation).
	 Currently tested under LINUX, Mac OSX, and a cluster
	 using OpenMPI and Sun Grid Engine.
	 
	 Note that the parallel computing for this approach is less efficient 
	 than the two-steps procedure consisting in 
	 (i) parallel simulation of the iid chains using \code{\link{MCMCcopies.cl}} to generate the 
	 \dQuote{cube} of simulated values,
	 and then (ii) entropy and Kullback estimation using \code{\link{EntropyMCMC.mc}}.
	 This is because  each node computes only one iteration at a time for the \code{nmc} chains
	 here, whereas it computes all the \eqn{n} iterations once for the \code{nmc} chains
	 when the entire cube is saved first. This is a trade-off between memory and speed.
	 
	 Note also that the \code{Rmpi} option is less efficient than the default option
	 using \pkg{parallel} if you are running on a single computer. 
	 MPI communication are required only for running on a true cluster/grid.


\bold{About passing your MCMC algorithm:}

 The list \code{mcmc_algo} must contain the named elements:
 
 \itemize{
 	\item \code{name}, the name of the MCMC, such as "RWHM"
 
 \item \code{chain}, the function for simulation of n steps of a single chain
 
 \item \code{step}, the function for simulation of 1 step of that algorithm
 
 \item \code{q_pdf}, the density of the proposal
 
 \item \code{q_proposal}, the function that simulates a proposal
 }
For examples, see the algorithms currently implemented:
\code{RWHM}, the Random Walk Hasting-Metropolis with gaussian proposal;
\code{HMIS_norm}, an Independence Sampler HM with gaussian proposal;
\code{IID_norm}, a gaussian iid sampler which is merely 
				a "fake" MCMC for testing purposes. 

Currently only non-adaptive Markov chains or adaptive chains for which
the past can be summarized by some sufficient statistics are eligible for this
computation forgetting the past of the \code{nmc} chains.
Adaptive chains such as \code{AMHaario}, the Adaptive-Metropolis (AM) from Haario (2001) are 
not yet tested for this function.
}

\value{An object of class \code{"KbMCMC"}, containing
%%  ~Describe the value returned
%%  If it is a LIST, use
	\item{Kullback}{A vector of estimated \eqn{K(p^t,f)}{K(pt,f)}, 
		for \eqn{t=1} up to the number of iterations \code{n}. This is the 
		convergence/comparison criterion.}
		
	\item{Entp}{A vector of estimated \eqn{E_{p^t}[\log(p^t)]}{E_pt[log(pt)]}, 
		for \eqn{t=1} up to the number of iterations that have been simulated.}
		
	\item{nmc}{The number of iid copies of each single chain.}
	
	\item{dim}{The state space dimension of the MCMC algorithm.}
	
	\item{algo}{The name of the MCMC algorithm that have been used to simulate
		the copies of chains, see \code{\link{MCMCcopies}}.}
	
	\item{target}{The target density for which the MCMC algorithm is defined; 
  		may be given only up to a multiplicative constant for most MCMC. 
  		target must be a function such as the multidimensional gaussian
  		\code{target_norm(x,param)} with argument and parameters passed 
  		like in this example.}
	\item{method}{The \code{method} input parameter (see above).}
	
	\item{f_param}{A list holding all the necessary target parameters, 
  		consistent with the target definition.}
  		
	\item{q_param}{A list holding all the necessary parameters 
  		for the proposal density of the MCMC algorithm that have been used.}
	
	\item{prob.accept}{Estimated rate of acceptation 
					(meaningful for accept/reject-type algorithms).}

  \item{Ptheta}{The \code{nmc} copies of chains in an array(n,d,nmc) 
  	of simulated values, where 1st value (1,d,nmc) is \code{Ptheta0}.}
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


\author{Didier Chauveau, Houssam Alrachid.}

%%\note{%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{MCMCcopies}}, \code{\link{MCMCcopies.mc}} and 
\code{\link{MCMCcopies.cl}} for just simulating the iid chains, and 
		\code{\link{EntropyMCMC}} or \code{\link{EntropyMCMC.mc}}
		for computing entropy and Kullback estimates from an already simulated
		set of iid chains (internally or from external code).
}

\examples{
## Toy example using the bivariate gaussian target
## same as for MCMCcopies
n = 150; nmc = 50; d=2 # bivariate example
varq=0.1 # variance of the proposal (chosen too small)
q_param=list(mean=rep(0,d),v=varq*diag(d))
## initial distribution, located in (2,2), "far" from target center (0,0)
Ptheta0 <- DrawInit(nmc, d, initpdf = "rnorm", mean = 2, sd = 1) 
# simulations and entropy + Kullback using the singlecore version
e1 <- EntropyParallel(RWHM, n, nmc, Ptheta0, target_norm,
                target_norm_param, q_param, verb = FALSE)
par(mfrow=c(1,2))
plot(e1) # default plot.plMCMC method, convergence after about 80 iterations
plot(e1, Kullback = FALSE) # Plot Entropy estimates over time
abline(normEntropy(target_norm_param), 0, col=8, lty=2) # true E_f[log(f)]
\donttest{
# Another example using multicore version, (not available on Windows)
varq=0.05 # variance of the proposal, even smaller
q_param=list(mean=rep(0,d),v=varq*diag(d))
n=300 # requires more iterations to show convergence
e1 <- EntropyParallel.cl(RWHM, n, nmc, Ptheta0, target_norm,
                         target_norm_param, q_param, cltype="PAR_SOCK",
                         verb = FALSE, nbnodes = 2)
plot(e1) # convergence after about 150 iterations
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file}
