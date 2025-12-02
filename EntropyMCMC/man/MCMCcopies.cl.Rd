\name{MCMCcopies.cl}
\alias{MCMCcopies.cl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parallel simulation of iid copies of a MCMC algorithm - cluster versions}

\description{ This function simulates \dQuote{parallel chains} (iid copies)	of a MCMC algorithm 
for \code{n} (time) iterations, i.e. for each chain \eqn{k}, the whole trajectory of the chain is generated. It returns an object of class \code{plMCMC} (for parallel MCMC) 
holding  an array of the trajectories and running information.  
This functions is similar to \code{\link{MCMCcopies}} and \code{MCMCcopies.mc} except that it uses 
HPC in a more generic way, implementing several types of HPC for running on a single, multicore computer or on a true cluster using MPI communications.
}

\usage{
MCMCcopies.cl(mcmc_algo, n=100, nmc=10, Ptheta0, target, f_param, q_param,
              cltype="PAR_SOCK", nbnodes=4) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mcmc_algo}{a list defining an MCMC algorithm in terms of the 
  	    functions it uses, such as \code{RWHM}, see details below.}
  
  \item{n}{The number of (time) iterations of each single chain to run.}

  \item{nmc}{The number of iid copies of each single chain.}

  \item{Ptheta0}{A \eqn{(nmc x d)} matrix, with the ith row giving a 
  	d-dimensional initial theta values for the ith chain.}

  \item{target}{The target density for which the MCMC algorithm is defined; 
  		may be given only up to a multiplicative constant for most MCMC. 
  		target must be a function such as the multidimensional gaussian
  		\code{target_norm(x,param)} with argument and parameters passed 
  		like in this example.}

  \item{f_param}{A list holding all the necessary target parameters, 
  		consistent with the target definition.}

  \item{q_param}{A list holding all the necessary parameters 
  		for the proposal density of the MCMC algorithm \code{mcmc_algo}.}

  \item{cltype}{Character string specifying the type of cluster; 
  	currently implemented 
  	types are: "PAR_SOCK" for socket cluster with \code{parallel} library, the default;
				"SNOW_SOCK" for socket cluster with \code{snow} library, and
				"SNOW_RMPI" for \code{snow} MPI cluster with \code{Rmpi} library.}
 
  \item{nbnodes}{The number of nodes or virtual cores requested to run the \code{nmc}
  	simulations in parallel. For the snow version, defaults to all; 
  	for the cluster version, defaults to 4.}
 
}

\details{
\code{MCMCcopies.cl} simulates in parallel
\code{nmc} iid copies of the MCMC algorithm passed in the list \code{mcmc_algo}, 
for \code{n} (time) iterations, and returns an object of class \code{plMCMC} holding an array of the trajectories and running information.

\bold{About parallel computing:}

The \code{Rmpi} option is less efficient than the default option
	 using \pkg{parallel} if you are running on a single computer. 
	 MPI communication are required only for running on a true cluster/grid.
  
This generic \emph{cluster} version implementing 
	 several types of cluster for running on a single, multicore computer
	 or on a true cluster using MPI communications may not 
	 work on all platform/OS. For instance the parallel socket cluster version 
	 does not work on Windows machines (see the \pkg{parallel} package documentation).
	 
 
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
\code{AMHaario}, the Adaptive-Metropolis (AM) from Haario (2001); 
\code{IID_norm}, a gaussian iid sampler which is merely 
				a "fake" MCMC for testing purposes. 
}
\value{
  \code{MCMCcopies.cl} returns a list of class \code{plMCMC} with items:

  \item{Ptheta}{The \code{nmc} copies of chains in an array(n,d,nmc) 
  	of simulated values, where 1st value (1,d,nmc) is \code{Ptheta0}.}

  \item{prob.accept}{The estimated rate of acceptation over all simulations.}

  \item{algo}{The MCMC algorithm name i.e. \code{mcmc_algo$name}.}
  
  \item{target}{The target density.}

  \item{f_param}{The list holding all the target parameters.}

  \item{q_param}{The list holding all the proposal density parameters.}
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





\author{Houssam Alrachid and Didier Chauveau.}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
A simpler cluster version \code{\link{MCMCcopies.mc}},
a single core version \code{\link{MCMCcopies}}, 
and functions doing simulation and entropy and Kullback  estimation simultaneously:
\code{\link{EntropyParallel}} and \code{\link{EntropyParallel.cl}} 
}


\examples{
## Toy example using the bivariate gaussian target
\donttest{
n = 150; nmc = 20; d=2 # bivariate example
varq=0.1 # variance of the proposal (chosen too small)
q_param=list(mean=rep(0,d),v=varq*diag(d))
## initial distribution, located in (2,2), "far" from target center (0,0)
Ptheta0 <- DrawInit(nmc, d, initpdf = "rnorm", mean = 2, sd = 1) 
# simulations (may be compared with the singlecore version using system.time)
s1 <- MCMCcopies.cl(RWHM, n, nmc, Ptheta0, target_norm,
		              target_norm_param, q_param, nbnodes = 2)
summary(s1) # method for "plMCMC" object
  }
## see MCMCcopies example for plots
}
\keyword{file}% use one of  RShowDoc("KEYWORDS")
