\name{e_dist}
\alias{e_dist}
\docType{package}
\title{
  Computes the energy distance of a point set
}
\description{
  \code{e_dist} computes the energy distance between points \code{D} and a target distribution (or big dataset) \eqn{F}. The cross-term \eqn{E[||X-X'||]}, \eqn{X,X'~F} is \emph{NOT} computed in \code{e_dist} for computational efficiency, since this is not needed for optimizing \code{D}. The target distribution or big dataset can be set using \code{dist.str} or \code{dist.samp}, respectively.
}
\usage{
  e_dist(D, dist.str=NA, dist.param=vector("list",ncol(D)),
       nsamp=1e6, dist.samp=NA)
}
\arguments{
  \item{D}{An \eqn{n} x \eqn{p} point set.}
  \item{dist.str}{A \eqn{p}-length string vector for target distribution (assuming independence). Current options include uniform, normal, exponential, gamma, lognormal, student-t, weibull, cauchy and beta. Exactly one of \code{dist.str} or \code{dist.samp} should be \code{NA}.}
  \item{dist.param}{A \eqn{p}-length list for desired distribution parameters in \code{dist.str}. The following parameters are supported:
  \itemize{
  \item \bold{Uniform}: Minimum, maximum;
  \item \bold{Normal}: Mean, standard deviation;
  \item \bold{Exponential}: Rate parameter;
  \item \bold{Gamma}: Shape parameter, scale parameter;
  \item \bold{Lognormal}: Log-mean, Log-standard deviation;
  \item \bold{Student-t}: Degree-of-freedom;
  \item \bold{Weibull}: Shape parameter, scale parameter;
  \item \bold{Cauchy}: Location parameter, scale parameter;
  \item \bold{Beta}: Shape parameter 1, shape parameter 2.
  }
  }
  \item{nsamp}{Number of samples to draw from \code{dist.str} for comparison.}
  \item{dist.samp}{An \eqn{N} x \eqn{p} matrix for the target big dataset (e.g., MCMC chain). Exactly one of \code{dist.str} or \code{dist.samp} should be \code{NA}.}
}
\references{
Szekely, G. J. and Rizzo, M. L. (2013). Energy statistics: A class of statistics based on distances. \emph{Journal of Statistical Planning and Inference}, 143(8):1249-1272.
}
\examples{
    \dontrun{
    #############################################################
    # Generate 25 SPs for the 2-d i.i.d. N(0,1) distribution
    #############################################################
    n <- 25 #number of points
    p <- 2 #dimension
    D <- sp(n,p,dist.str=rep("normal",p))
    Drnd <- matrix(rnorm(n*p),ncol=p)
    e_dist(D$sp,dist.str=rep("normal",p)) #smaller
    e_dist(Drnd,dist.str=rep("normal",p))
  
  
    #############################################################
    # Support points for big data reduction: Franke's function
    #############################################################
  #  library(MHadaptive) # Package archived, but you can use your favorite MCMC sampler
    
  #  #Generate MCMC samples
  #  li_func <- franke2d #Desired log-posterior
  #  ini <- c(0.5,0.5) #Initial point for MCMc
  #  NN <- 1e5 #Number of MCMC samples desired
  #  burnin <- NN/2 #Number of burn-in runs
  #  mcmc_franke <- Metro_Hastings(li_func, pars=ini, prop_sigma=0.05*diag(2),
  #                           iterations=NN, burn_in=burnin)
    data(mcmc_franke) # Loading MCMC sample from data
    
    #Use modified Franke's function as posterior
    franke2d <- function(xx){
      if ((xx[1]>1)||(xx[1]<0)||(xx[2]>1)||(xx[2]<0)){
        return(-Inf)
      }
      else{
        x1 <- xx[1]
        x2 <- xx[2]
        
        term1 <- 0.75 * exp(-(9*x1-2)^2/4 - (9*x2-2)^2/4)
        term2 <- 0.75 * exp(-(9*x1+1)^2/49 - (9*x2+1)/10)
        term3 <- 0.5 * exp(-(9*x1-7)^2/4 - (9*x2-3)^2/4)
        term4 <- -0.2 * exp(-(9*x1-4)^2 - (9*x2-7)^2)
        
        y <- term1 + term2 + term3 + term4
        return(2*log(y))
      }
    }
    
    #Generate ncur SPs
    ncur <- 50
    D <- sp(ncur,2,dist.samp=mcmc_franke$trace)$sp
    Drnd <- mcmc_franke$trace[sample(1:nrow(mcmc_franke$trace),n,F),]
    e_dist(D,dist.samp=mcmc_franke$trace) #smaller
    e_dist(Drnd,dist.samp=mcmc_franke$trace)
    }
}
