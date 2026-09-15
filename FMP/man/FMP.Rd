\name{FMP}
\alias{FMP}
\title{Estimate the coefficients of a filtered monotonic polynomial IRT model}
\description{
Estimate the coefficients of a filtered monotonic polynomial IRT model.}
\usage{ FMP(data, thetaInit, item, startvals, k, eps = 1e-06)
}
\arguments{
 \item{data}{N(subjects)-by-p(items) matrix of 0/1 item response data.}
 \item{thetaInit}{Initial theta (\eqn{\theta}) surrogates (e.g., calculated by \link{svdNorm}).}
 \item{item}{Item number for coefficient estimation.}
 \item{startvals}{Start values for function minimization. Start values are in the gamma metric (see Liang & Browne, 2015)} 
 \item{k}{Order of monotonic polynomial = 2k+1 (see Liang & Browne, 2015). k can equal 0, 1, 2, or 3.}
 \item{eps}{Step size for gradient approximation, default = 1e-6. If a convergence failure occurs during function optimization reducing the value of eps will often produce a converged solution.}
}
\value{
 \item{b}{Vector of polynomial coefficients.}
 \item{gamma}{Polynomial coefficients in gamma metric (see Liang & Browne, 2015).}
 \item{FHAT}{Function value at convergence.}
 \item{counts}{Number of function evaluations during minimization (see optim documentation for further details).}
 \item{AIC}{Pseudo scaled Akaike Information Criterion (AIC). Candidate models that produce the smallest AIC suggest the optimal number of parameters given the sample size. Scaling is accomplished by dividing the non-scaled AIC by sample size.}
 \item{BIC}{Pseudo scaled Bayesian Information Criterion (BIC). Candidate models that produce the smallest BIC suggest the optimal number of parameters given the sample size. Scaling is accomplished by dividing the non-scaled BIC by sample size.}
 \item{convergence}{Convergence = 0 indicates that the optimization algorithm converged; convergence=1 indicates that the optimization failed to converge.}
}
\details{
As described by Liang and Browne (2015), the filtered polynomial model (FMP) is a quasi-parametric 
IRT model in which the IRF is a composition of a logistic function and a polynomial function, \eqn{m(\theta)}, of degree 2k + 1. 
When k = 0, \eqn{m(\theta) = b_0 + b_1 \theta} 
(the slope intercept form of the 2PL). When k = 1, 2k + 1 equals 3 resulting in 
\eqn{m(\theta) = b_0 + b_1 \theta + b_2 \theta^2 + b_3 \theta^3}. Acceptable values of k = {0,1,2,3}. According to 
Liang and Browne, the "FMP IRF may be used to approximate any IRF with a continuous derivative arbitrarily 
closely by increasing the number of parameters in the monotonic polynomial" (2015, p. 2) The FMP model assumes that the
IRF is monotonically increasing, bounded by 0 and 1, and everywhere differentiable with respect to theta (the latent trait).
}
\references{
Liang, L. & Browne, M. W. (2015). A quasi-parametric method for fitting flexible item response functions.
\emph{Journal of Educational and Behavioral Statistics, 40}, 5--34.
}
\author{Niels Waller}
\examples{

\dontrun{
## In this example we will generate 2000 item response vectors 
## for a k = 1 order filtered polynomial model and then recover 
## the estimated item parameters with the FMP function.  

k <- 1  # order of polynomial

NSubjects <- 2000


## generate a sample of 2000 item response vectors 
## for a k = 1 FMP model using the following
## coefficients
b <- matrix(c(
   #b0     b1      b2     b3   b4  b5  b6  b7  k
  1.675, 1.974, -0.068, 0.053,  0,  0,  0,  0, 1,
  1.550, 1.805, -0.230, 0.032,  0,  0,  0,  0, 1,
  1.282, 1.063, -0.103, 0.003,  0,  0,  0,  0, 1,
  0.704, 1.376, -0.107, 0.040,  0,  0,  0,  0, 1,
  1.417, 1.413,  0.021, 0.000,  0,  0,  0,  0, 1,
 -0.008, 1.349, -0.195, 0.144,  0,  0,  0,  0, 1,
  0.512, 1.538, -0.089, 0.082,  0,  0,  0,  0, 1,
  0.122, 0.601, -0.082, 0.119,  0,  0,  0,  0, 1,
  1.801, 1.211,  0.015, 0.000,  0,  0,  0,  0, 1,
 -0.207, 1.191,  0.066, 0.033,  0,  0,  0,  0, 1,
 -0.215, 1.291, -0.087, 0.029,  0,  0,  0,  0, 1,
  0.259, 0.875,  0.177, 0.072,  0,  0,  0,  0, 1,
 -0.423, 0.942,  0.064, 0.094,  0,  0,  0,  0, 1,
  0.113, 0.795,  0.124, 0.110,  0,  0,  0,  0, 1,
  1.030, 1.525,  0.200, 0.076,  0,  0,  0,  0, 1,
  0.140, 1.209,  0.082, 0.148,  0,  0,  0,  0, 1,
  0.429, 1.480, -0.008, 0.061,  0,  0,  0,  0, 1,
  0.089, 0.785, -0.065, 0.018,  0,  0,  0,  0, 1,
 -0.516, 1.013,  0.016, 0.023,  0,  0,  0,  0, 1,
  0.143, 1.315, -0.011, 0.136,  0,  0,  0,  0, 1,
  0.347, 0.733, -0.121, 0.041,  0,  0,  0,  0, 1,
 -0.074, 0.869,  0.013, 0.026,  0,  0,  0,  0, 1,
  0.630, 1.484, -0.001, 0.000,  0,  0,  0,  0, 1), 
  nrow=23, ncol=9, byrow=TRUE)  
  
ex1.data<-genFMPData(NSubj = NSubjects, bParams = b, seed = 345)$data

## number of items in the data matrix
NItems <- ncol(ex1.data)

# compute (initial) surrogate theta values from 
# the normed left singular vector of the centered 
# data matrix
thetaInit <- svdNorm(ex1.data)


## earlier we defined k = 1
  if(k == 0) {
            startVals <- c(1.5, 1.5)
            bmat <- matrix(0, NItems, 6)
            colnames(bmat) <- c(paste("b", 0:1, sep = ""),"FHAT", "AIC", "BIC", "convergence") 
  }
  if(k == 1) {
           startVals <- c(1.5, 1.5, .10, .10)
           bmat <- matrix(0, NItems, 8)
           colnames(bmat) <- c(paste("b", 0:3, sep = ""),"FHAT", "AIC", "BIC", "convergence") 
  }
  if(k == 2) {
           startVals <- c(1.5, 1.5, .10, .10, .10, .10)
           bmat <- matrix(0, NItems, 10)
           colnames(bmat) <- c(paste("b", 0:5, sep = ""),"FHAT", "AIC", "BIC", "convergence") 
  }
  if(k == 3) {
           startVals <- c(1.5, 1.5, .10, .10, .10, .10, .10, .10)
           bmat <- matrix(0, NItems, 12)
           colnames(bmat) <- c(paste("b", 0:7, sep = ""),"FHAT", "AIC", "BIC", "convergence") 
  }         
  
# estimate item parameters and fit statistics  
  for(i in 1:NItems){
    out <- FMP(data = ex1.data, thetaInit, item = i, startvals = startVals, k = k)
    Nb <- length(out$b)
    bmat[i,1:Nb] <- out$b
    bmat[i,Nb+1] <- out$FHAT
    bmat[i,Nb+2] <- out$AIC
    bmat[i,Nb+3] <- out$BIC
    bmat[i,Nb+4] <- out$convergence
  }

# print output 
print(bmat)
}
}

\keyword{statistics}% at least one, from doc/KEYWORDS
