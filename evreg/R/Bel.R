#' Degree of belief of interval for a Gaussian random fuzzy number
#'
#' \code{Bel} computes the degree of belief of an interval [x,y] for a given Gaussian
#' random fuzzy number.
#'
#' @param x The lower bound of the interval (may be a vector).
#' @param y The upper bound of the interval (may be a vector).
#' @param GRFN A Gaussian random fuzzy number, encoded as a list with components mu, sig
#' and h.
#'
#' @return The degree of belief of the interval.
#' @importFrom stats pnorm
#' @export
#'
#' @references
#'
#' Thierry Denoeux. Reasoning with fuzzy and uncertain evidence using epistemic random
#' fuzzy sets: general framework and practical models. Fuzzy Sets and Systems, Vol. 453,
#' Pages 1-36, 2023.
#'
#'@seealso \code{\link{Belint}}, \code{\link{Pl}}, \code{\link{pl_contour}},
#'\code{\link{combination_GRFN}}
#'
#' @examples
#' bel<-Bel(1,2,list(mu=2,sig=1,h=2))
#' print(bel)
Bel <- function(x,y,GRFN){
  sig1<-GRFN$sig*sqrt(GRFN$h*GRFN$sig^2+1)
  epsi<-GRFN$h*GRFN$sig^2
  A<-pnorm(y,GRFN$mu,GRFN$sig)-pnorm(x,GRFN$mu,GRFN$sig)
  if(epsi==1){
    B<-pl_contour(x,GRFN)*(pnorm(y*(1+epsi)/2,GRFN$mu,sig1)-pnorm(x,GRFN$mu,sig1))
    C<-pl_contour(y,GRFN)*(pnorm(y,GRFN$mu,sig1)-pnorm(x*(1+epsi)/2,GRFN$mu,sig1))
  } else{
    B<-pl_contour(x,GRFN)*(pnorm(x*(1-epsi)/2+y*(1+epsi)/2,GRFN$mu,sig1)-pnorm(x,GRFN$mu,sig1))
    C<-pl_contour(y,GRFN)*(pnorm(y,GRFN$mu,sig1)-pnorm(x*(1+epsi)/2+y*(1-epsi)/2,GRFN$mu,sig1))
  }
  bel<-pmax(0,A-B-C)
  return(bel)
}

