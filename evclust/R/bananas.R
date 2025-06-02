#' Generation of "bananas" datasets
#'
#'\code{bananas} generates a dataset with two classes separated by a nonlinear boundary.
#'
#' This function generates a dataset with two complex-shaped classes, useful to test some nonlinear
#' or constrained clustering algorithms.
#'
#' @param n Number of observations.
#' @param r Radius of the two half circles (default: 5).
#' @param s Standard deviation of noise (default 1).
#'
#' @return A list with two attributes:
#'  \describe{
#'   \item{x}{The (n,2) matrix of attributes.}
#'   \item{y}{The vector of class labels.}
#'  }
#'
#'
#'@references F. Li, S. Li and T. Denoeux. k-CEVCLUS: Constrained evidential clustering of 
#'large dissimilarity data. Knowledge-Based Systems (142):29-44, 2018.
#'
#'@author Feng Li.
#'
#' @export
#'
#' @seealso \code{\link{kcevclus}}
#'
#' @examples 
#' data<-bananas(1000)
#' plot(data$x,pch=data$y,col=data$y)
#'

bananas<-function(n,r=5,s=1){
  Pi<-3.141593
  #simulated data generation function
  domaina<-0.125*Pi+runif(n/2)*1.25*Pi
  a<-cbind(r*sin(domaina),r*cos(domaina))+matrix(rnorm(n/2*2),n/2,2)*s
  domainb<-0.375*Pi-runif(n/2)*1.25*Pi
  b<-cbind(r*sin(domainb),r*cos(domainb))+matrix(rnorm(n/2*2),n/2,2)*s+matrix(-0.75*r,n/2,2)
  x<-rbind(a,b)
  y<-rep(1:2,each=n/2)
  return(list(x=x,y=y))
}