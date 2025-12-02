\name{MCMCcopies.mc}
\alias{MCMCcopies.mc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulates iid copies of a MCMC algorithm - multicore version}

\description{
  Simulates \code{nmc} iid copies of a MCMC algorithm \code{mcmc_algo}
  for \code{n}
  (time) iterations and returns an object of class \code{plMCMC} 
  (for parallel MCMC) holding
  an array of the trajectories and running information.
  This functions is similar to \code{\link{MCMCcopies}} except that it uses the
  \pkg{parallel} package (available in the main distribution, but not for Windows machines) 
  to split the task between the available
  virtual cores on the computer. 
}

\usage{
MCMCcopies.mc(mcmc_algo, n = 100, nmc = 10, Ptheta0, target, f_param, q_param, 
			verb = TRUE, nbcores=detectCores())
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

  \item{verb}{Verbose mode for summarizing output during the simulation.}
  
  \item{nbcores}{Number of required (virtual) cores, defaults to all as returned
  	            by \code{detectCores()}.}  
}

\details{
%%  ~~ If necessary, more details than the description above ~~
\code{MCMCcopies.mc}, like \code{\link{MCMCcopies}}, sequentially simulates 
\code{nmc} iid copies of the
	 MCMC algorithm passed in the list \code{mcmc_algo}, 
  for \code{n} (time) iterations, and returns an object of class \code{plMCMC} holding an array of the trajectories and running information.
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
  \code{MCMCcopies} returns a list of class \code{plMCMC} with items:

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



\author{Didier Chauveau.}

%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
A more general cluster version \code{\link{MCMCcopies.cl}},
a single core version \code{\link{MCMCcopies}}, 
and functions doing simulation and entropy and Kullback  estimation simultaneously:
\code{\link{EntropyParallel}} and \code{\link{EntropyParallel.cl}} 
}

\examples{
## Toy example using the bivariate gaussian target
\donttest{
## not working on Windows since socket cluster not implemented
n = 150; nmc = 20; d=2 # bivariate example
varq=0.1 # variance of the proposal (chosen too small)
q_param=list(mean=rep(0,d),v=varq*diag(d))
## initial distribution, located in (2,2), "far" from target center (0,0)
Ptheta0 <- DrawInit(nmc, d, initpdf = "rnorm", mean = 2, sd = 1) 
# simulations (may be compared with the singlecore version using system.time)
s1 <- MCMCcopies.mc(RWHM, n, nmc, Ptheta0, target_norm,
                    target_norm_param, q_param, nbcores = 2)
summary(s1) # method for "plMCMC" object
  }
## see MCMCcopies example for plots
}

\keyword{file}
