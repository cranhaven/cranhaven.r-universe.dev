#' Generating a credal partition by bootstraping Gaussian Mixture Models
#'
#' \code{bootclus} generates a credal partition by bootstrapping Gaussian Mixture Models.
#'
#' This function uses the \code{mclust} package to generate and bootstrap the mixture models.
#'
#' @param x attribute matrix or data frame of size (n,p).
#' @param conf confidence level (default: 0.90).
#' @param B number of bootstrap samples (default=500)
#' @param param list of arguments passed to function \code{Mclust} in addition to 'data'.
#' @param type Type of focal sets ("simple": \eqn{\emptyset}, singletons and \eqn{\Omega};
#' "full": all \eqn{2^c} subsets of \eqn{\Omega}; "pairs": \eqn{\emptyset}, singletons,
#' \eqn{\Omega}, and all or selected pairs). Argument passed to \code{makeF}.
#' @param Omega Logical. If TRUE, \eqn{\Omega} is a focal set. Default is FALSE. 
#' Argument passed to \code{makeF}.
#' 
#'
#' @return A list with the following components:
#' \describe{
#' \item{clus}{An object of class '\code{Mclust}' returned by \code{Mclust}.}
#' \item{Clus}{An object of class '\code{credpart}' providing the output credal partition.}
#' \item{CI}{An array of dimension (2,n,n) containing the confidence intervals on pairwise probabilities.}
#' \item{BelPl}{An array of dimension (2,n,n) containing the pairwise Bel-Pl intervals.}
#' \item{Time}{A matrix of size (3,5) containing the computing time as returned by function \code{proctime}
#' for (1) the parameter estimation and bootstrap, (2) the computation fo the quantiles on pairwise 
#' probabilities, and (3) the computation of the credal partition.}
#' }
#' @export
#' @import mclust plyr quadprog
#' @importFrom stats as.dist
#'
#' @seealso \code{\link{ecm}}, \code{\link{recm}},
#'\code{\link{cecm}}, \code{\link{kevclus}}.
#'
#' @references
#'T. Denoeux. Calibrated model-based evidential clustering using bootstrapping. 
#'Information Sciences, Vol. 528, pages 17-45, 2020.
#'
#' @examples
#' ## Example with the Faithful geyser data
#' \dontrun{
#' data("faithful")
#' X<-faithful
#' param=list(G=3)
#' res.faithful<-bootclus(X,conf=0.90,B=100,param=param)
#' ## Plot the results
#' plot(res.faithful$Clus,X)
#' }

bootclus<-function(x,conf=0.90,B=500,param=list(G=NULL),type="pairs",Omega=FALSE){
  alpha<-c((1-conf)/2,(1+conf)/2)
  X<-x
  N<-nrow(X)
  Time<-matrix(0,3,5)
  ptm <- proc.time()
  # Model-based clustering
  clus<-do.call(Mclust,c(list(data=X),param))
  G<-clus$G
  # Bootstrap
  boot<-MclustBootstrap(clus, nboot = B, type = "bs")
  
  # Computation of pairwise probabilities
  R<-mapply(createR,alply(boot$mean,1),alply(boot$variance,1),alply(boot$pro,1),
            MoreArgs=list(X=X),SIMPLIFY="array")
  R1<-simplify2array(R)
  Time[1,]<-proc.time()-ptm
  
  # Computation of quantiles
  CI<-apply(R1,1:2,quantile,probs=alpha,na.rm=TRUE)
  RR<-list(Me=matrix(0,N,N),M1=CI[1,,],M0=1-CI[2,,])
  Time[2,]<-proc.time()-Time[1,]
  
  # Generation of the credal partition
  res<-mass_from_P_new(P=RR,c=G,type=type,Omega=Omega)
  Time[3,]<-proc.time()-Time[2,]
  
  return(list(clus=clus,Clus=res$Clus,CI=CI,BelPl=res$BelPl,Time=Time))
}