\name{MCMCcopies}
\alias{MCMCcopies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulates iid copies of a MCMC algorithm}

\description{
  Simulates \code{nmc} iid copies of a MCMC algorithm \code{mcmc_algo}
  for \code{n}
  (time) iterations and returns an object of class \code{plMCMC} 
  (for parallel MCMC) holding an array of the trajectories and running information.
}

\usage{
MCMCcopies(mcmc_algo, n = 100, nmc = 10, Ptheta0, target, f_param, q_param, verb = TRUE)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{mcmc_algo}{a list defining an MCMC algorithm in terms of the 
  	    functions it uses, such as \code{RWHM}, see details below.}
  
  \item{n}{The number of (time) iterations of each single chain to run.}

  \item{nmc}{The number of iid copies of each single chain.}

  \item{Ptheta0}{A (nmc x d) matrix, with the ith row giving a d-dimensional initial theta values for the ith chain.}

  \item{target}{The target density for which the MCMC algorithm is defined; 
  		may be given only up to a multiplicative constant for most MCMC. 
  		Target must be a function such as the multidimensional gaussian
  		\code{target_norm(x,param)} with argument and parameters passed 
  		like in this example.}

  \item{f_param}{A list holding all the necessary target parameters, 
  		consistent with the target definition.}

  \item{q_param}{A list holding all the necessary parameters 
  		for the proposal density of the MCMC algorithm \code{mcmc_algo}.}

  \item{verb}{Verbose mode for summarizing output during the simulation.}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
\code{MCMCcopies} sequentially simulates \code{nmc} iid copies of the
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
   }
}

\author{Didier Chauveau.}

%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~



\seealso{
Two multicore and cluster version 
 \code{\link{MCMCcopies.mc}} and
\code{\link{MCMCcopies.cl}},
and functions doing simulation and entropy and Kullback  estimation simultaneously:
\code{\link{EntropyParallel}} and \code{\link{EntropyParallel.cl}} 
}


\examples{
## Toy example using the bivariate gaussian target
## with default parameters value, see target_norm_param
n = 150; nmc = 20; d=2 # bivariate example
varq=0.1 # variance of the proposal (chosen too small)
q_param=list(mean=rep(0,d),v=varq*diag(d))
## initial distribution, located in (2,2), "far" from target center (0,0)
Ptheta0 <- DrawInit(nmc, d, initpdf = "rnorm", mean = 2, sd = 1) 
# simulation
s1 <- MCMCcopies(RWHM, n, nmc, Ptheta0, target_norm,
                 target_norm_param, q_param, verb = FALSE)
summary(s1) # method for "plMCMC" object
par(mfrow=c(1,2))
plot(s1) # just a path of the iid chains, method for "plMCMC" object
hist(s1$Ptheta[,1,], col=8) # marginal 1
}

\keyword{file}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
