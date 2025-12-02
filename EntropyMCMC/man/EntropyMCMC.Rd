\name{EntropyMCMC}
\alias{EntropyMCMC}
\alias{EntropyMCMC.mc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kullback and entropy estimation from MCMC simulation output - 
	single and multicore versions}

\description{These functions return estimates of the entropy 
of the density \eqn{p^t}{pt} of a MCMC algorithm at time \eqn{t}, 
\eqn{E_{p^t}[\log(p^t)]}{E_pt[log(pt)]},
and of the Kullback divergence between \eqn{p^t}{pt} and the target density,
for \eqn{t=1} up to the number of iterations that have been simulated. 
The MCMC simulations must be computed before or externally, 
and passed as a "\code{plMCMC}" object 
in the first argument (see details).
The target may be known only up to a multiplicative constant (see details).

 \code{\link{EntropyMCMC.mc}} is a parallel computing
	version  that uses the
  \pkg{\link{parallel}} package to split the task between the available (virtual) cores on the computer. This version using socket cluster is not available for Windows computers.
}

\usage{
EntropyMCMC(plmc1, method = "A.Nearest.Neighbor", k=1, trim = 0.02, eps=0, 
        all.f = TRUE, verb = FALSE, EntVect = FALSE,
        uselogtarget = FALSE, logtarget = NULL)

EntropyMCMC.mc(plmc1, method = "A.Nearest.Neighbor", k = 1, trim = 0.02, eps=0,
        all.f = TRUE, verb = FALSE, EntVect = FALSE, nbcores=detectCores(), 
		    uselogtarget = FALSE, logtarget = NULL)

}

\arguments{
  \item{plmc1}{an objects of class \code{plMCMC} 
  (for parallel MCMC), like the output of \code{MCMCcopies},
  	which contains all the simulations plus target \eqn{f} definition and parameters.}
  	
  \item{method}{The method for estimating the entropy \eqn{E_{p^t}[\log(p^t)]}{E_pt[log(pt)]}. 
  	Methods currently  implemented are :
  	\code{"NearestNeighbor"} as in Kozachenko and Leonenko (1987),  
  	\code{"k.NearestNeighbor"} as in Leonenko et al. (2005), 
  	\code{"A.Nearest.Neighbor"} (the default) which is as 
  	\code{"k.NearestNeighbor"} but uses the \pkg{RANN} package for (Approximate) fast computation of nearest neighbors,
  	\code{"Gyorfi.trim"} subsampling method as defined in Gyorfi and Vander Mulen (1989), 
  	plus a tuning parameter \code{trim} for trimming the data 
  	(see Chauveau and Vandekerkhove (2011)).}

\item{k}{The k-nearest neighbor index, the default is \eqn{k=1}.}

  \item{trim}{Parameter controlling the percentage of smallest data from one subsample
  	that is removed, only for \code{method = "Gyorfi.trim"}.}
  	
    \item{eps}{A parameter controlling precision in the \code{"A.Nearest.Neighbor"}" method, 
    the default means no approximation, see the \pkg{RANN} package.}
    
  \item{all.f}{If \code{TRUE} (the default) relative entropy is computed
  	 over the whole sample. Should be removed in next version.}  	 
  \item{verb}{Verbose mode}
  
  \item{EntVect}{If \code{FALSE} (the default), the entropy is computed only on the kth-nearest neighbor. If \code{TRUE},  the entropy is computed for all j-NN's for \eqn{j=1} to \eqn{k} (the latter being mostly for testing purposes).}
  
  \item{nbcores}{Number of required (virtual) cores, defaults to all as returned
  	   by \code{detectCores()}.}
  	 
  \item{uselogtarget}{Set to \code{FALSE} by default; 
    useful in some cases where \eqn{log(f(\theta))} returns \code{-Inf} values in 
    Kullback computations because   
    \eqn{f(\theta)} itself returns too small values for some \eqn{\theta} far from modal regions.
    In these case using a function computing the logarithm of the target 
    can remove the infinity values.}

    \item{logtarget}{The function defining \eqn{log(f(theta))}, \code{NULL} by default, 
    required if \code{uselogtarget} equals \code{TRUE}.
    This option and \code{uselogtarget} are currently implemented only for the 
    \code{"A.Nearest.Neighbor"} method,
    and for the default \code{EntVect = FALSE} option.}
    }
	
  	


\details{Methods based on  Nearest Neighbors (NN) should be preferred since these require less tuning parameters. 
Some options, as \code{uselogtarget} are in testing phase and are not implemented in all the available methods (see Arguments). 
}

\value{An object of class \code{KbMCMC} (for Kullback MCMC), containing:
%%  ~Describe the value returned
%%  If it is a LIST, use
	\item{Kullback}{A vector of estimated divergences \eqn{K(p^t,f)}{K(pt,f)}, 
		for \eqn{t=1} up to the number of iterations that have been simulated. 
		This is the convergence/comparison criterion.}
		
	\item{Entp}{A vector of estimated entropies \eqn{E_{p^t}[\log(p^t)]}{E_pt[log(pt)]}, 
		for \eqn{t=1} up to the number of iterations that have been simulated.}
		
	\item{nmc}{The number of iid copies of each single chain.}
	
	\item{dim}{The state space dimension of the MCMC algorithm.}
	
	\item{algo}{The name of the MCMC algorithm that have been used to simulate
		the copies of chains, see \code{\link{MCMCcopies}}.}
	
	\item{target}{The target density for which the MCMC algorithm is defined; 
  	ususally given only up to a multiplicative constant for MCMC in Bayesian models. 
  		target must be a function such as the multidimensional gaussian
  		\code{target_norm(x,param)} with argument and parameters passed 
  		like in the example below.}
	\item{method}{The \code{method} input parameter (see above).}
	
	\item{f_param}{A list holding all the necessary target parameters, 
  		consistent with the target definition.}
  		
	\item{q_param}{A list holding all the necessary parameters 
  		for the proposal density of the MCMC algorithm that have been used.}
%% ...
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

\note{The method \code{"Resubst"} is implemented for testing, without theoretical guarantee of convergence.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{MCMCcopies}} and 
		\code{\link{MCMCcopies.mc}} for iid MCMC simulations (single core and multicore),
		 \code{\link{EntropyParallel}} and \code{\link{EntropyParallel.cl}}
		 for simultaneous simulation and entropy estimation (single core and multicore).}

\examples{
## Toy example using the bivariate gaussian target
## with default parameters value, see target_norm_param
n = 150; nmc = 50; d=2 # bivariate example
varq=0.1 # variance of the proposal (chosen too small)
q_param=list(mean=rep(0,d),v=varq*diag(d))
## initial distribution, located in (2,2), "far" from target center (0,0)
Ptheta0 <- DrawInit(nmc, d, initpdf = "rnorm", mean = 2, sd = 1) 
# simulation of the nmc iid chains, singlecore 
s1 <- MCMCcopies(RWHM, n, nmc, Ptheta0, target_norm,
                 target_norm_param, q_param, verb = FALSE)
summary(s1) # method for "plMCMC" object
e1 <- EntropyMCMC(s1) # computes Entropy and Kullback divergence estimates
par(mfrow=c(1,2))
plot(e1) # default plot.plMCMC method, convergence after about 80 iterations
plot(e1, Kullback = FALSE) # Plot Entropy estimates over time
abline(normEntropy(target_norm_param), 0, col=8, lty=2) # true E_f[log(f)]
}

\keyword{file}

