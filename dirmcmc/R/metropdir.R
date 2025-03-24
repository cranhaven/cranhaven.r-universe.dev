#' @title Directional Metropolis Hastings
#' @author Abhirup Mallik, \email{malli066@umn.edu}
#' @description Implements Markov Chain Monte Carlo for continuous random vectors using Directional Metropolis Hasting Algorithm
#' @param obj an R function that evaluates the log unnormalized probability
#'density of the desired equilibrium distribution of the Markov chain.
#'First argument is the state vector of the Markov chain.  Other arguments
#'arbitrary and taken from the \code{...} arguments of this function.
#'Should return \code{- Inf} for points of the state space having
#'probability zero under the desired equilibrium distribution.
#' @param dobj an R function that evaluates the derivative of the log unnormalized probability density at the current state of the markov chain.
#' @param initial Initial state of the markov chain. \code{obj(state)} must not return #' \code{-Inf}
#' @param lchain length of the chain
#' @param sd.prop scale to use for the proposal
#' @param steplen tuning parameter in mean of proposal
#' @param s tuning parameter in the covariance of proposal
#' @param ... any arguments to be passed to obj and dobj.
#' @details Runs a \dQuote{Directional Metropolis Hastings} algorithm, with multivariate normal proposal
#' producing a Markov chain with equilibrium distribution having a specified
#' unnormalized density.  Distribution must be continuous.  Support of the
#' distribution is the support of the density specified by argument \code{obj}.
#' @return Returns the following objects in a list:
#' \itemize{
#' \item accept. acceptance rate.
#' \item batch. resulting chain.
#' }
#' @examples 
#' \dontrun{
#' Sigma <- matrix(c(1,0.2,0.2,1),2,2)
#' mu <- c(1,1)
# 
#' Sig.Inv <- solve(Sigma)
#' Sig.det.sqrt <- sqrt(det(Sigma))
# 
#' logf <- function(x,mu,Sig.Inv){
#'   x.center <- as.numeric((x-mu))
#'   out <- crossprod(x.center,Sig.Inv)
#'   out <- sum(out*x.center)
#'   -out/2
#'   }
#' 
#' gr.logf <- function(x,mu,Sig.Inv){
#'   x.center <- as.numeric((x-mu))
#'   out <- crossprod(x.center,Sig.Inv)
#'   -as.numeric(out)
#' }
#' set.seed(1234)
#' system.time(out <- metropdir(obj = logf, dobj = gr.logf, initial = c(1,1),
#'                          lchain = 1e4,sd.prop=1,steplen=0,s=1, mu = mu,
#'                          Sig.Inv = Sig.Inv))
#' #acceptance rate
#' out$accept
#' #density plot
#' plot(density(out$batch[,1]))
#' }
#' @seealso \code{\link{metropdir.adapt}} for adapting DMH, \code{\link{iact}} for integrated auto correlation times, \code{\link{mcmcdiag}}, \code{\link{msjd}} for mean squared jump distance.
#' for summary of diagnostic measures of a chain, \code{\link{multiESS}} for Multivariate effective sample size.
#' @keywords dmh, mcmc
#' @export


metropdir <- function(obj,dobj,initial,lchain,sd.prop=1,steplen=0,s=0.95,...){
  tol <- 1e-4
  #sd.prop <- 1
  p <- length(initial)
  chain <- matrix(NA, nrow = lchain, ncol = p)
  chain[1,] <- initial
  accept <- 0

  sdist2 <- function(x,s){
    p <- length(x)
    S <- diag(c(1/s,rep(1,p-1)))
    crossprod(x,S)%*%x
  }

  x <- initial
  dx <- dobj(x,...)
  d.norm <- sqrt(sum(dx*dx))
  if(d.norm > tol){
    d <- dx/d.norm
  }else d <- c(1,rep(0,p-1))
  Gx <- qr.qy(qr(d),diag(p))
  LAM <- diag(c(sqrt(s),rep(1,p-1)))
  Sigma.sqrt.x <- Gx%*%LAM%*%t(Gx)
  mu.x <- x+d*rep(steplen,p)*d.norm
  acceptance <- 0
  for(i in 1:lchain){
    chain[i,] <- x
    z <- rnorm(p,sd=sd.prop)
    mu.x <- x + dx*rep(steplen,p)
    y <- Sigma.sqrt.x%*%z+mu.x

    dy <- dobj(y,...)
    d.norm <- sqrt(sum(dy*dy))
    if(d.norm > tol){
      d <- dy/d.norm
    }else d <- c(1,rep(0,p-1))
    Gy <- qr.qy(qr(d),diag(p))
    mu.y <- y+d*rep(steplen,p)*d.norm
    Sigma.sqrt.y <- Gy%*%LAM%*%t(Gy)

    ##diff <- x-y
    logalpha <- obj(y,...) - obj(x,...) +
      (sdist2(Gx%*%(y-mu.x),s)-sdist2(Gy%*%(x-mu.y),s))/2

    if(logalpha > 0){
      accept <- TRUE
    } else {
      u <- runif(1)
      if(logalpha > log(u)){
        accept <- TRUE
      } else {
        accept <- FALSE
      }
    }

    if(accept){
      x <- y
      dx <- dy
      mu.x <- mu.y
      Gx <- Gy
      Sigma.sqrt.x <- Sigma.sqrt.y
      acceptance <- acceptance + 1
    }
  }
  list(accept = acceptance/lchain, batch = chain)
}
