#' Degree of plausibility of interval for a Gaussian random fuzzy number
#'
#' \code{Pl} computes the degree of plausibility of an interval [x,y] for a given Gaussian
#' random fuzzy number.
#'
#' @param x The lower bound of the interval (may be a vector).
#' @param y The upper bound of the interval (may be a vector).
#' @param GRFN A Gaussian random fuzzy number, encoded as a list with components mu, sig
#' and h.
#'
#' @return The degree of plausibility of the interval.
#' @importFrom stats pnorm
#' @export
#'
#' @references
#'
#' Thierry Denoeux. Reasoning with fuzzy and uncertain evidence using epistemic random
#' fuzzy sets: general framework and practical models. Fuzzy Sets and Systems, Vol. 453,
#' Pages 1-36, 2023.
#'
#'@seealso \code{\link{Belint}}, \code{\link{Bel}}, \code{\link{pl_contour}},
#'\code{\link{combination_GRFN}}
#'
#' @examples
#' pl<-Pl(1,2,list(mu=2,sig=1,h=2))
#' print(pl)
Pl <- function(x,y,GRFN){
  sig1<-GRFN$sig*sqrt(GRFN$h*GRFN$sig^2+1)
  A<-pnorm(y,GRFN$mu,GRFN$sig)-pnorm(x,GRFN$mu,GRFN$sig)
  B<-pl_contour(x,GRFN)*pnorm(x,GRFN$mu,sig1)
  C<-pl_contour(y,GRFN)*(1-pnorm(y,GRFN$mu,sig1))
  pl<-A+B+C
  return(pl)
}
