#' Finds a belief interval centered on mu for a Gaussian random fuzzy number
#'
#' \code{Belint} find an interval of the form [mu-r,mu+r] with specified degree of belief
#' for a Gaussian random fuzzy number.
#'
#' @param level The specified degree of belief (between 0 and 1).
#' @param GRFN A Gaussian random fuzzy number, encoded as a list with components mu, sig
#' and h.
#'
#' @return A vector containing the lower and upper bounds of the interval.
#' @importFrom stats uniroot qnorm
#' @export
#'
#' @references
#'
#' Thierry Denoeux. Reasoning with fuzzy and uncertain evidence using epistemic random
#' fuzzy sets: general framework and practical models. Fuzzy Sets and Systems, Vol. 453,
#' Pages 1-36, 2023.
#'
#'@seealso \code{\link{Bel}}, \code{\link{Pl}}, \code{\link{pl_contour}}
#'
#' @examples
#' int<-Belint(0.9,list(mu=2,sig=1,h=2))
#' print(int)
Belint<-function(level=0.9,GRFN){
  alpha<-level
  a <- -GRFN$sig*qnorm((1-alpha)/2)
  b <- 2*a
  while(Bel(GRFN$mu-b,GRFN$mu+b,GRFN)<alpha) b<-2*b
  res<-uniroot(function(r,GRFN,alpha) Bel(GRFN$mu-r,GRFN$mu+r,GRFN)-alpha,c(a,b),
               GRFN=GRFN,alpha=alpha)
  r<-res$root
  return(c(GRFN$mu-r,GRFN$mu+r))
}
