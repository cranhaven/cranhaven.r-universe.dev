#' Contour function of a Gaussian random fuzzy number
#'
#' \code{pl_contour} computes the degree of plausibility of any number x for a given Gaussian
#' random fuzzy number.
#'
#' pl_contour(x,GRFN) returns the same value as Pl(x,x,GRFN), but is more efficient.
#'
#' @param x The input value (can be a vector).
#' @param GRFN A Gaussian random fuzzy number, encoded as a list with components mu, sig
#' and h.
#'
#' @return The degree of plausibility of x.
#' @export
#'
#' @references
#'
#' Thierry Denoeux. Reasoning with fuzzy and uncertain evidence using epistemic random
#' fuzzy sets: general framework and practical models. Fuzzy Sets and Systems, Vol. 453,
#' Pages 1-36, 2023.
#'
#'@seealso \code{\link{Pl}}, \code{\link{Bel}}, \code{\link{Belint}}
#'
#' @examples
#' pl<-pl_contour(1,list(mu=2,sig=1,h=2))
#' print(pl)
pl_contour<-function(x,GRFN){
  if(GRFN$h==0) pl<-rep(1,length(x)) else{
    Z2<-GRFN$h*GRFN$sig^2+1
    pl<-1/sqrt(Z2)*exp(-0.5*GRFN$h*(x-GRFN$mu)^2/Z2)
  }
  return(pl)
}
