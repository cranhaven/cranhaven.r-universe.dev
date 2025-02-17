#' @title Student CDF with integer number of degrees of freedom
#' @description Cumulative distribution function of the noncentrel Student
#' distribution with an integer number of degrees of freedom.
#' @param q quantile, a finite number
#' @param nu integer greater than \eqn{1}, the number of degrees of freedom;
#' possibly infinite
#' @param delta numeric vector of noncentrality parameters; possibly infinite
#' @return Numeric vector, the CDF evaluated at \code{q}.
#' @export
#' @importFrom Rcpp evalCpp
#' @importFrom stats pnorm
#' @useDynLib OwenQ
#' @note The results are theoretically exact when the number of degrees of
#' freedom is even.
#' When odd, the procedure resorts to the Owen T-function.
#' @references
#' Owen, D. B. (1965).
#' A special case of a bivariate noncentral t-distribution.
#' \emph{Biometrika} \bold{52}, 437-446.
#' @examples
#' ptOwen(2, 3) - pt(2, 3)
#' ptOwen(2, 3, delta=1) - pt(2, 3, ncp=1)
ptOwen <- function(q, nu, delta=0){
  if(is.infinite(q)){
    return(ifelse(is.infinite(delta),
                  ifelse(sign(q)==sign(delta),
                         NaN,
                         ifelse(q>0, 1, 0)),
                  as.numeric(q>0)))
  }
  if(nu==Inf){
    return(pnorm(q, mean=delta))
  }
  if(isNotPositiveInteger(nu)){
    stop("`nu` must be an integer >=1.")
  }
  out <- numeric(J <- length(delta))
  winf <- which(inf <- is.infinite(delta))
  if(L <- length(winf)){
    out[winf] <- ifelse(delta[winf]==Inf, 0, 1)
  }
  if(L < J){
    noninf <- which(!inf)
    out[noninf] <- RcppOwenStudent(q, nu, delta[noninf])
  }
  out
}

