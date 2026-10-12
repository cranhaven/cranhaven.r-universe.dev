#' Find Scale Parameters for Inverse Gamma Hyperprior of Nonlinear Effects with Spike and Slab Prior (Simulation-based)
#' 
#' This function implements a optimisation routine that computes the scale parameter \eqn{b} and selection parameter
#' \eqn{r}. . Here, we assume an inverse gamma prior IG(\eqn{a},\eqn{b}) for \eqn{\psi^2} and \eqn{\tau^2\sim N(0,r(\delta)\psi^2)} 
#' and given shape paramter \eqn{a},
#' such that approximately \eqn{P(f(x)\le c|spike,\forall x\in D)\ge 1-\alpha1} and \eqn{P(\exists x\in D s.t. f(x)\ge c|slab)\ge 1-\alpha2}.
#' 
#' @param Z the row of the design matrix (or the complete matrix of several observations) evaluated at. 
#' @param Kinv the generalised inverse of \eqn{K}. 
#' @param a is the shape parameter of the inverse gamma distribution, default is 5.
#' @param c denotes the expected range of eqn{f} . 
#' @param alpha1 denotes the 1-\eqn{\alpha1} level for \eqn{b}.  
#' @param alpha2 denotes the 1-\eqn{\alpha2} level for \eqn{r}.    
#' @param R denotes the number of replicates drawn during simulation.
#' @param myseed denotes the required seed for the simulation based method.  
#' @return an object of class \code{list} with root values \eqn{r}, \eqn{b} from \code{\link{uniroot}}.
#' @author Nadja Klein
#' @references Nadja Klein, Thomas Kneib, Stefan Lang and Helga Wagner (2016). Spike and Slab Priors for Effect Selection in Distributional Regression. 
#' \emph{Working Paper}.
#' 
#' @import splines
#' @import pscl
#' @import mvtnorm
#' @export
#' @examples
#' set.seed(123)
#' library(MASS)
#' # prior precision matrix (second order differences) 
#' # of a spline of degree l=3 and with m=22 inner knots
#' # yielding dim(K)=m+l-1=22
#' K <- t(diff(diag(22), differences=2))%*%diff(diag(22), differences=2)
#' # generalised inverse of K (same as if we used mixed model representation!)
#' Kinv <- ginv(K)
#' # covariate x
#' x <- runif(1)
#' Z <- matrix(DesignM(x)$Z_B,nrow=1)
#' fgrid <- seq(-3,3,length=1000)
#' mdf <- hyperpar(Z,Kinv,a=5,c=0.1,alpha1=0.05,alpha2=0.05,R=10000,myseed=123)
#'


hyperpar <- function (Z, Kinv, a = 5, c = 0.1, alpha1 = 0.1, alpha2 = 0.1, 
          R = 10000, myseed = 123) 
{
   # sigmaref <-    exp(1/(length(diag(Kinv)))*sum(0.5*log(diag(Kinv))))

  # require("mvtnorm")
  # require("pscl")
  simsup <- function(b, delta, r) {
    set.seed(myseed)
    psisample <- rigamma(n = R, alpha = a, beta = b)
    
    rhelp <- r^(1 - delta)
    tausample <- rnorm(n = R, mean = 0, sd = sqrt(psisample * 
                                                    rhelp))

    d <- dim(Kinv)[1]
    s <- svd((Kinv + t(Kinv))/2)
    s$d <- abs(zapsmall(s$d))
    m <- sum(s$d > 0)
    x <- matrix(rnorm(m*R), nrow=m)
    x <- rbind(x, matrix(0, nrow=d-m, ncol=R))
    bsample <- s$u %*% diag(sqrt(s$d)) %*% x
    fsample <- t(Z %*% bsample)

   # fsample <-fsample * abs(tausample/sigmaref^2)
    fsample <-fsample * abs(tausample)

    supsample <- apply(abs(fsample), 1, max)
    print("finish simsup")
    print(all(is.finite(supsample)))
     return(supsample)
  }
  cat("Optimising b\n")
  optfn1 <- function(b, c) {
    quantile(simsup(b = b, delta = 1, r = 1), probs = alpha1) - 
      c
  }
  (bopt <- uniroot(f = optfn1, interval = c(0.000001, upper = 1000000), 
                   c = c))
  bopt <- bopt$root
  cat("Optimising r\n")
  
  optfn2 <- function(r, b, c) {
    quantile(simsup(b = b, delta = 0, r = r), probs = 1 - 
               alpha2) - c
  }
  (ropt <- uniroot(f = optfn2, interval = c(0.000001, upper = 0.5), 
                   b = bopt, c = c))
  ropt <- ropt$root
  res <- list(b = bopt, r = ropt)
  return(res)
}