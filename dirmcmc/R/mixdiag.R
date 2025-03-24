#'@title Integrated Auto correlation times of a Markov Chain
#' @author Abhirup Mallik, \email{malli066@umn.edu}
#' @description This function calculates the Integrated Auto Correlation Times of a Markov Chain.
#' @details The Integrated Auto Correlation Times of a Markov Chain X is defined as: \deqn{1 + 2 \sum \Gamma_i}, where \deqn{\Gamma} indicates the estimated autocorrelation terms of the chain. These are estimated using the sample correlation matrix from the lagged chain. This measure is intended for one dimensional chains or single component of a multivariate chains.
#'@param x chain (one dimension)
#'@return Integrated ACT of the chain.
#' @examples 
#' \dontrun{
#' ## Banana Target
#'lupost.banana <- function(x,B){
#'  -x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2
#'}
#' Banana Gradient
#'gr.banana <- function(x,B){
#'  g1 <- -x[1]/100 - 2*B*(x[2]+B*x[1]^2-100*B)
#'  g2 <- -(x[2]+B*x[1]^2-100*B)
#'  g <- c(g1,g2)
#'  return(g)
#'} 
#'out.metdir.banana <- metropdir(obj = lupost.banana, dobj = gr.banana,
#'initial = c(0,1),lchain = 2000,
#'sd.prop=1.25,
#'steplen=0.01,s=1.5,B=0.03)
#'iact(out.metdir.banana$batch[,1])
#'}
#' @seealso \code{\link{msjd}} for mean squared jumping distance, \code{\link{mcmcdiag}}
#' for summary of diagnostic measures of a chain, \code{\link{multiESS}} for Multivariate effective sample size.
#' @keywords iact, mcmc, dmh.
#'@export

iact <- function(x){
  if(NCOL(x) > 1) stop("Input must be in one dimension.")
  xlen <- length(x)
  tmp <- acf(x,lag.max = xlen,plot = FALSE)$acf
  li <- min(which(tmp<0.05))
  out <- 1 + 2*sum(tmp[1:(li-1)])
  out
}



#'@title Mean Squared Jump Distance of a Markov Chain
#' @author Abhirup Mallik, \email{malli066@umn.edu}
#'@param X chain (Matrix) (in d dim)
#'@description We calculate mean square euclidean jumping distance. The target covariance is unknown and the assumption of elliptical contour might not hold here, hence, we dont implement the variance scaled version. And this version is computationally faster as well.
#' @details Mean squared jump distance of a markov chain is a measure used to diagnose the mixing of the chain. It is calculated as the mean of the squared eucledean distance between every point and its previous point. Usually, this quantity indicates if the chain is moving enough or getting stuck at some region.
#' @return Mean squared jump distance of the chain.
#' @examples 
#' \dontrun{
#' ## Banana Target
#'lupost.banana <- function(x,B){
#'  -x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2
#'}
#' Banana Gradient
#'gr.banana <- function(x,B){
#'  g1 <- -x[1]/100 - 2*B*(x[2]+B*x[1]^2-100*B)
#'  g2 <- -(x[2]+B*x[1]^2-100*B)
#'  g <- c(g1,g2)
#'  return(g)
#'} 
#'out.metdir.banana <- metropdir(obj = lupost.banana, dobj = gr.banana,
#'initial = c(0,1),lchain = 2000,
#'sd.prop=1.25,
#'steplen=0.01,s=1.5,B=0.03)
#'msjd(out.metdir.banana$batch)
#'}
#' @seealso \code{\link{iact}} for integrated auto correlation times, \code{\link{mcmcdiag}}
#' for summary of diagnostic measures of a chain, \code{\link{multiESS}} for Multivariate effective sample size.
#' @keywords iact, mcmc, dmh.
#' @export

msjd <- function(X){
  n <- NROW(X)
  d <- NCOL(X)
  if(is.matrix(X)){
    Xt <- X[-1,]
    Xt1 <- X[-n,]
    jumpdist <- apply(Xt-Xt1, 1, function(y)crossprod(y))
    return(as.numeric(crossprod(jumpdist)/(n-1)))
  }
  if(is.vector(X)){
    Xt <- X[-1]
    Xt1 <- X[-n]
    return(as.numeric(crossprod(Xt-Xt1))/(n-1))
  }
}

#' @title mcmcdiag
#' @author Abhirup Mallik, \email{malli066@umn.edu}
#' @param X Chain (Matrix)
#' @description This function calculates all different diagnostics supported in this library and returns in a list
#' @details This function calculates four metrics useful for diagnostics of a Markov chain. The chain input could be univariate or multivariate. The univariate summaries are calculated marginally, for each component for a multivariate chains. Effective sample size is calculated for each component. Integrared auto correlation times is also another componentwise measure calculated for all the components. Multivariate Effective sample size is calculated from mcmcse package. Mean squared jump distance is another multivariate summary measure that is returned.
#' @return list with following elements:
#' \itemize{
#' \item MEss Multivariate Effective sample size.
#' \item ess vector of effective sample size for each component.
#' \item iact vector of integrated autocorrelation times for each component.
#' \item msjd Mean squared jump distance for the chain.
#' }
#' @examples 
#' \dontrun{
#' ## Banana Target
#'lupost.banana <- function(x,B){
#'  -x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2
#'}
#' Banana Gradient
#'gr.banana <- function(x,B){
#'  g1 <- -x[1]/100 - 2*B*(x[2]+B*x[1]^2-100*B)
#'  g2 <- -(x[2]+B*x[1]^2-100*B)
#'  g <- c(g1,g2)
#'  return(g)
#'} 
#'out.metdir.banana <- metropdir(obj = lupost.banana, dobj = gr.banana,
#'initial = c(0,1),lchain = 2000,
#'sd.prop=1.25,
#'steplen=0.01,s=1.5,B=0.03)
#'mcmcdiag(out.metdir.banana$batch)
#'}
#' @seealso \code{\link{iact}} for integrated auto correlation times, \code{\link{msjd}}
#' for mean squared jump distance of a chain, \code{\link{multiESS}} for Multivariate effective sample size.
#' @keywords iact, mcmc, dmh.
#' @export

mcmcdiag <- function(X){
  n <- NROW(X)
  d <- NCOL(X)
  out <- list()
  mess.out <- mcmcse::multiESS(X)
  ess.out <- mcmcse::ess(X)
  iact.out <- numeric(d)
  for(i in 1:d) iact.out[i] <- iact(X[,i])
  msjd.out <- msjd(X)
  return(list(MEss = mess.out,ess = ess.out,iact = iact.out, msjd = msjd.out))
}
