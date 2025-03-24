#' @title Directional Metropolis Hastings with Adaptation.
#' @author Abhirup Mallik, \email{malli066@umn.edu}
#' @inheritParams metropdir
#' @param batchlen length of batch used for update. Default is 100.
#' @param targetacc Target acceptance ratio
#' @description Implements adaptive version of directional Metropolis Hastings.
#'@details This function is for automatically select a scaling factor for the directional Metropolis Hastings algorithm. This function uses batch wise update of the scale parameter to produce a adaptive chain. The user is required to supply a target acceptance ratio. The adaptive scheme modifies the scale parameter to achieve the target acceptance ratio. It is recommended that to check the output of the adaptation history as returned by the function. 
#'
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
#' system.time(out <- metropdir.adapt(obj = logf, dobj = gr.logf, initial = c(1,1),
#'                          lchain = 1e4,sd.prop=1,steplen=0,s=1, mu = mu,
#'                          Sig.Inv = Sig.Inv,targetacc=0.44))
#' #acceptance rate
#' out$accept
#' #density plot
#' plot(density(out$batch[,1]))
#' }
#' @seealso \code{\link{metropdir}} for DMH, \code{\link{iact}} for integrated auto correlation times, \code{\link{mcmcdiag}}, \code{\link{msjd}} for mean squared jump distance.
#' for summary of diagnostic measures of a chain, \code{\link{multiESS}} for Multivariate effective sample size.
#' @keywords dmh, mcmc, adaptive
#' @export


metropdir.adapt <- function(obj,dobj,initial,lchain,sd.prop=1,steplen=0,s=0.95,
                      batchlen = 100,targetacc=0.234,...){
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

  #Initialize for adaption #
  bc <- 1
  bi <- 1
  bataccept <- 0
  numbatch <- floor(lchain/batchlen)
  sdout <- numeric(numbatch)

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
    ## Adaptation ##
    if(bi < batchlen){
      if(accept) bataccept <- bataccept + 1
    }else{
      sdout[bc] <- sd.prop
      if(accept) bataccept <- bataccept + 1
      if(bataccept > batchlen*targetacc){
        sd.prop <- sd.prop * exp(min(c(0.01,1/sqrt(bc))))
      } else {
        sd.prop <- sd.prop / exp(min(c(0.01,1/sqrt(bc))))
      }
      bi <- 0
      bc <- bc + 1
      bataccept <- 0
    }
    bi <- bi + 1
  }
  list(accept = acceptance/lchain, batch = chain,sdout = sdout)
}
