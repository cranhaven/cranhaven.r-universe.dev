#' Combination of Gaussian random fuzzy numbers
#'
#' \code{combination_GRFN} combines two Gaussian random fuzzy numbers using the generalized
#' product-intersection rule with soft or hard normalization.
#'
#' @param GRFN1 A Gaussian random fuzzy number, encoded as a list with components mu, sig
#' and h.
#' @param GRFN2 A Gaussian random fuzzy number, encoded as a list with components mu, sig
#' and h.
#' @param soft If TRUE (default), the combination rule with soft normalization is used.
#' Otherwise, hard normalization is employed.
#'
#' @return A list with two components:
#' \describe{
#' \item{GRFN}{The combined Gaussian random fuzzy number, encoded as a list with components
#' mu, sig and h.}
#' \item{conflict}{The degree of conflict (equal to 0 if \code{soft==FALSE}).}
#' }
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
#' GRFN1<-list(mu=1,sig=1,h=2)
#' GRFN2<-list(mu=2,sig=2,h=3)
#' GRFN12s<-combination_GRFN(GRFN1,GRFN2) # soft normalization
#' GRFN12h<-combination_GRFN(GRFN1,GRFN2,soft=FALSE) # hard normalization
#' print(GRFN12s)
#' print(GRFN12h)
combination_GRFN <- function(GRFN1,GRFN2,soft=TRUE){
  if((GRFN1$h==0)&(GRFN2$h==0)){
    M<-0
    S<-1
    kappa<-0
  } else {
    if(soft){
      hbar<-GRFN1$h*GRFN2$h/(GRFN1$h+GRFN2$h)
      rho<-hbar*GRFN1$sig*GRFN2$sig/sqrt((1+hbar*GRFN1$sig^2)*(1+hbar*GRFN2$sig^2))
      D<-1+hbar*(GRFN1$sig^2+GRFN2$sig^2)
      S1<-GRFN1$sig^2*(1+hbar*GRFN2$sig^2)/D
      S2<-GRFN2$sig^2*(1+hbar*GRFN1$sig^2)/D
      M1<-(GRFN1$mu*(1+hbar*GRFN2$sig^2) + GRFN2$mu*hbar*GRFN1$sig^2)/D
      M2<-(GRFN2$mu*(1+hbar*GRFN1$sig^2) + GRFN1$mu*hbar*GRFN2$sig^2)/D
      SIG<-matrix(c(S1,rho*sqrt(S1*S2),rho*sqrt(S1*S2),S2),2,2)
    } else {
      M1<-GRFN1$mu
      M2<-GRFN2$mu
      SIG<-diag(c(GRFN1$sig^2,GRFN2$sig^2))
    }
    u<-c(GRFN1$h,GRFN2$h)/(GRFN1$h+GRFN2$h)
    M<-as.double(u%*%c(M1,M2))
    S<-as.double(sqrt(u%*%SIG%*%u))

    # Calculation of the degree of conflict
    if(soft){
      if((GRFN1$sig>0) & (GRFN2$sig>0)){
        kappa<-1-sqrt(S1*S2)/(GRFN1$sig*GRFN2$sig)*sqrt(1-rho^2)*
          exp(-0.5*(GRFN1$mu^2/GRFN1$sig^2+GRFN2$mu^2/GRFN2$sig^2)+0.5/(1-rho^2)*
                (M1^2/S1+M2^2/S2-2*rho*M1*M2/sqrt(S1*S2)))
      } else if((GRFN1$sig>0) & (GRFN2$sig==0)){
        M1<-(GRFN1$mu + GRFN2$mu*hbar*GRFN1$sig^2)/D
        kappa<- 1-1/sqrt(1+hbar*S1)*exp(-0.5*hbar/(1+hbar*S1)*(M1-GRFN2$mu)^2)
      } else if((GRFN1$sig==0) & (GRFN2$sig>0)){
        M2<-(GRFN2$mu + GRFN1$mu*hbar*GRFN2$sig^2)/D
        kappa<- 1-1/sqrt(1+hbar*S2)*exp(-0.5*hbar/(1+hbar*S2)*(M2-GRFN1$mu)^2)
      } else kappa<- 1-exp(-0.5*hbar*(GRFN1$mu-GRFN2$mu)^2)
    } else kappa=0
  }
  return(list(GRFN=list(mu=M,sig=S,h=GRFN1$h+GRFN2$h),conflict=kappa))
}


