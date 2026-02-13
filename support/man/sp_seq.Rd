\name{sp_seq}
\alias{sp_seq}
\docType{package}
\title{
  Computing (batch) sequential support points using difference-of-convex programming
}
\description{
  \code{sp_seq} computes (batch) sequential support points to add onto a current point set \code{D}. Current options include sequential support points on standard distributions (specified via \code{dist.str}) or sequential support points for reducing big data (specified via \code{dist.samp}).
}
\usage{
  sp_seq(D, nseq, ini=NA, num.rep=1,
        dist.str=NA, dist.param=vector("list",p),
        dist.samp=NA, scale.flg=TRUE, bd=NA, 
        num.subsamp=ifelse(any(is.na(dist.samp)),
        max(10000,10*(nseq+nrow(D))),
        min(10000,nrow(dist.samp))),
        iter.max=max(200,iter.min), iter.min=50,
        tol=1e-10, par.flg=TRUE)
}
\arguments{
  \item{D}{An \eqn{n} x \eqn{p} matrix for the current point set.}
  \item{nseq}{Number of support points to add to \code{D}.}
  \item{ini}{An \eqn{nseq} x \eqn{p} matrix for initialization.}
  \item{num.rep}{Number of random restarts for optimization.}
  \item{dist.str}{A \eqn{p}-length string vector for desired distribution (assuming independence). Current options include uniform, normal, exponential, gamma, lognormal, student-t, weibull, cauchy and beta. Exactly one of \code{dist.str} or \code{dist.samp} should be \code{NA}.}
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
  \item{dist.samp}{An \eqn{N} x \eqn{p} matrix for the big dataset (e.g., MCMC chain) to be reduced. Exactly one of \code{dist.str} or \code{dist.samp} should be \code{NA}.}
  \item{scale.flg}{Should the big data \code{dist.samp} be normalized to unit variance before processing?}
  \item{bd}{A \eqn{p} x \eqn{2} matrix for the lower and upper bounds of each variable.}
  \item{num.subsamp}{Batch size for resampling. For distributions, the default is \code{max(10000,10*n)}. For data reduction, the default is \code{min(10000,nrow(dist.samp))}.}
  \item{iter.max}{Maximum iterations for optimization.}
  \item{iter.min}{Minimum iterations for optimization.}
  \item{tol}{Error tolerance for optimization.}
  \item{par.flg}{Should parallelization be used?}
}
\value{
  \item{D}{An \eqn{n} x \eqn{p} matrix for the current point set.}
  \item{seq}{An \eqn{nseq} x \eqn{p} matrix for the additional \code{nseq} support points.}
}
\references{
Mak, S. and Joseph, V. R. (2018). Support points. \emph{Annals of Statistics}, 46(6A):2562-2592.
}
\examples{
    \dontrun{
    
    #############################################################
    # Generate 50 SPs for the 2-d i.i.d. N(0,1) distribution
    #############################################################
    ncur <- 50
    cur.sp <- sp(ncur,2,dist.str=rep("normal",2))$sp
    
    #Add 50 sequential SPs
    nseq <- 50
    seq.sp <- sp_seq(cur.sp,nseq,dist.str=rep("normal",2))$seq
    
    x1 <- seq(-3.5,3.5,length.out=100) #Plot contours
    x2 <- seq(-3.5,3.5,length.out=100)
    z <- exp(-outer(x1^2,x2^2,FUN="+")/2)
    contour.default(x=x1,y=x2,z=z,drawlabels=FALSE,nlevels=10)
    points(cur.sp,pch=4,cex=1.25,col="black",lwd=2) # (current in black)
    points(seq.sp,pch=16,cex=1.25,col="red")        # (new SPs in red)
    
    #############################################################
    # Support points for big data reduction: Franke distribution
    #############################################################
    
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

    
  # library(MHadaptive) # Package archived, but you can use your favorite MCMC sampler
  #  
  # #Generate MCMC samples
  # li_func <- franke2d #Desired log-posterior
  # ini <- c(0.5,0.5) #Initial point for MCMc
  # NN <- 1e5 #Number of MCMC samples desired
  # burnin <- NN/2 #Number of burn-in runs
  # mcmc_franke <- Metro_Hastings(li_func, pars=ini, prop_sigma=0.05*diag(2),
  #                          iterations=NN, burn_in=burnin)
    data(mcmc_franke) # Loading MCMC sample from data
    
    #Generate ncur SPs
    ncur <- 50
    cur.sp <- sp(ncur,2,dist.samp=mcmc_franke$trace)$sp
    
    #Add nseq sequential SPs
    nseq <- 50
    seq.sp <- sp_seq(cur.sp,nseq,dist.samp=mcmc_franke$trace)$seq
    
    #Plot SPs
    par(mfrow=c(1,2))
    x1 <- seq(0,1,length.out=100) #contours
    x2 <- seq(0,1,length.out=100)
    z <- matrix(NA,nrow=100,ncol=100)
    for (i in 1:100){
      for (j in 1:100){
        z[i,j] <- franke2d(c(x1[i],x2[j]))
      }
    }
    plot(mcmc_franke$trace,pch=4,col="gray",cex=0.75,
      xlab="",ylab="",xlim=c(0,1),ylim=c(0,1))      #big data
    points(cur.sp,pch=4,cex=1.25,col="black",lwd=2) # (current in black)
    points(seq.sp,pch=16,cex=1.25,col="red")        # (new SPs in red)
    contour.default(x=x1,y=x2,z=z,
      drawlabels=TRUE,nlevels=10)                   #contour
    points(cur.sp,pch=4,cex=1.25,col="black",lwd=2) # (current in black)
    points(seq.sp,pch=16,cex=1.25,col="red")        # (new SPs in red)
  }
}
