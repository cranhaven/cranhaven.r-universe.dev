#' @title Owen distribution functions when \ifelse{html}{\out{&delta;<sub>1</sub>>&delta;<sub>2</sub>}}{\eqn{\delta_1>\delta_2}}
#' @description Evaluates the Owen distribution functions
#' when the noncentrality parameters satisfy \ifelse{html}{\out{&delta;<sub>1</sub>>&delta;<sub>2</sub>}}{\eqn{\delta_1>\delta_2}} and
#' the number of degrees of freedom is integer.
#' \itemize{
#' \item \code{powen1} evaluates \ifelse{html}{\out{P(T<sub>1</sub> &le; t<sub>1</sub>, T<sub>2</sub> &le; t<sub>2</sub>)}}{\eqn{P(T_1\le t_1, T_2 \le t_2)}}
#' (Owen's equality 8)
#' \item \code{powen2} evaluates \ifelse{html}{\out{P(T<sub>1</sub> &le; t<sub>1</sub>, T<sub>2</sub> > t<sub>2</sub>)}}{\eqn{P(T_1\le t_1, T_2 > t_2)}}
#' (Owen's equality 9)
#' \item \code{powen3} evaluates \ifelse{html}{\out{P(T<sub>1</sub> > t<sub>1</sub>, T<sub>2</sub> > t<sub>2</sub>)}}{\eqn{P(T_1> t_1, T_2 > t_2)}}
#' (Owen's equality 10)
#' \item \code{powen4} evaluates \ifelse{html}{\out{P(T<sub>1</sub> > t<sub>1</sub>, T<sub>2</sub> &le; t<sub>2</sub>)}}{\eqn{P(T_1> t_1, T_2 \le t_2)}}
#' (Owen's equality 11)
#' }
#' @name powen
#' @param nu integer greater than \eqn{1}, the number of degrees of freedom;
#' infinite allowed
#' @param t1,t2 two numbers, positive or negative, possible infinite
#' @param delta1,delta2 two vectors of possibly infinite numbers with the same length,
#' the noncentrality parameters;
#' must satisfy \code{delta1>delta2}
#' @param algo the algorithm used, \code{1} or \code{2}
#' @return A vector of numbers between \eqn{0} and \eqn{1}, possibly
#' containing some \code{NaN}.
#' @importFrom Rcpp evalCpp
#' @importFrom stats pnorm
#' @useDynLib OwenQ
#' @note When the number of degrees of freedom is odd, the procedure resorts to
#' the Owen T-function (\code{\link{OwenT}}).
#' @seealso Use \code{\link{psbt}} for general values of \code{delta1} and \code{delta2}.
#' @references
#' Owen, D. B. (1965).
#' A special case of a bivariate noncentral t-distribution.
#' \emph{Biometrika} \bold{52}, 437-446.
#' @examples
#' nu=5; t1=2; t2=1; delta1=3; delta2=2
#' # Wolfram integration gives 0.1394458271284726
#' ( p1 <- powen1(nu, t1, t2, delta1, delta2) )
#' # Wolfram integration gives 0.0353568969628651
#' ( p2 <- powen2(nu, t1, t2, delta1, delta2) )
#' # Wolfram integration gives 0.806507459306199
#' ( p3 <- powen3(nu, t1, t2, delta1, delta2) )
#' # Wolfram integration gives 0.018689824158
#' ( p4 <- powen4(nu, t1, t2, delta1, delta2) )
#' # the sum should be 1
#' p1+p2+p3+p4
NULL

#' @rdname powen
#' @export
powen1 <- function(nu, t1, t2, delta1, delta2, algo=2){
  J <- length(delta1)
  if(J != length(delta2)){
    stop("`delta1` and `delta2` must have the same length.")
  }
  if(any(delta1<=delta2)){ # & is.finite(delta1) & is.finite(delta2))){
    stop("`delta1` must be >`delta2`.")
  }
  if(isNotPositiveInteger(nu)){
    stop("`nu` must be an integer >=1.")
  }
  if(t1<=t2){
    out <- rep(NaN, J)
    if(any(i <- t2 != -Inf || delta2 != -Inf)) out[i] <- ptOwen(t1, nu, delta1[i])
    return(out)
  }
  if(nu == Inf){
    return(pnorm(t1, mean=delta1) -
             pmax(0, pnorm(t1, mean=delta1)-pnorm(t2, mean=delta2)))
  }
  if(t1 == Inf){
    out <- rep(NaN, J)
    if(any(i <- delta1 != Inf)) out[i] <- ptOwen(t2, nu, delta2[i])
    return(out)
  }
  if(t2 == -Inf){
    out <- rep(NaN, J)
    if(any(i <- delta2 != -Inf)) out[i] <- ptOwen(t1, nu, delta1[i])
    return(out)
  }
  out <- numeric(J)
  if(!all(i <- (is.finite(delta1) & is.finite(delta2)))){
    if(any(inf2 <- delta2==-Inf)){
      out[inf2] <- ptOwen(t1, nu, delta1[inf2]) ## c'est pas l'inverse ?
    }
  }
  if(any(i)){
    out[i] <- RcppOwenCDF1(nu, t1, t2, delta1[i], delta2[i], algo)
  }
  out
}

#' @rdname powen
#' @export
powen2 <- function(nu, t1, t2, delta1, delta2, algo=2){
  J <- length(delta1)
  if(J != length(delta2)){
    stop("`delta1` and `delta2` must have the same length.")
  }
  if(any(delta1<=delta2)){# & is.finite(delta1) & is.finite(delta2))){
    stop("`delta1` must be >`delta2`.")
  }
  if(isNotPositiveInteger(nu)){
    stop("`nu` must be an integer >=1.")
  }
  if(t1<=t2){
    return(numeric(J)) # y'a des NaN ? à revoir
    # out <- numeric(J)
    # if(is.infinite(t1)){
    #   out[delta1==t1] <- NaN
    # }
    # if(is.infinite(t2)){
    #   out[delta2==t2] <- NaN
    # }
    # return(out)
  }
  if(nu == Inf){
    #return(-pnorm(t2, mean=delta2)+pnorm(t1, mean=delta1) + pmax(0, pnorm(t2, mean=delta2)-pnorm(t1, mean=delta1)))
    return(pmax(0, pnorm(t1, mean=delta1)-pnorm(t2, mean=delta2)))
  }
  #if(is.infinite(t1) || is.infinite(t2)){
  out <- numeric(J)
  # if(is.infinite(t1)){
  #   if(t1==Inf){
  #     out[i <- delta1==Inf] <- NaN
  #     out[!i] <- 1-ptOwen(t2, nu, delta2)
  #   }else{
  #     out[delta1==-Inf] <- NaN # ça n'arrive pas
  #   }
  # }else{
  #   if(t2 == Inf){ # ça n'arrive pas car t1>t2
  #     out[delta2==Inf] <- NaN
  #   }else{
  #     out[i <- delta2==-Inf] <- NaN
  #     out[!i] <- ptOwen(t1, nu, delta1)
  #   }
  # }
  if(t1==Inf){
    out[i <- delta1==Inf] <- NaN
    if(!all(i)) out[!i] <- 1-ptOwen(t2, nu, delta2[!i])
    return(out)
  }
  if(t2 == -Inf){
    out[i <- delta2==-Inf] <- NaN
    if(!all(i)) out[!i] <- ptOwen(t1, nu, delta1[!i])
    return(out)
  }
  if(any(i <- is.finite(delta1) & is.finite(delta2))){
    out[i] <- RcppOwenCDF2(nu, t1, t2, delta1[i], delta2[i], algo)
  }
  out
}

#' @rdname powen
#' @export
powen3 <- function(nu, t1, t2, delta1, delta2, algo=2){
  J <- length(delta1)
  if(J != length(delta2)){
    stop("`delta1` and `delta2` must have the same length.")
  }
  if(any(delta1<=delta2)){ # & is.finite(delta1) & is.finite(delta2))){
    stop("`delta1` must be >`delta2`.")
  }
  if(isNotPositiveInteger(nu)){
    stop("`nu` must be an integer >=1.")
  }
  if(t1<=t2){
    return(1-ptOwen(t2, nu, delta2))
  }
  if(t1 == Inf){
    out <- numeric(J)
    out[delta1 == Inf] <- NaN
    return(out)
  }
  if(t2 == -Inf){
    out <- rep(NaN, J)
    if(any(i <- delta2 != -Inf)) out[i] <- 1-ptOwen(t1, nu, delta1[i])
    return(out)
  }
  if(nu == Inf){ # to simplify ?
    return(1-pnorm(t1, mean=delta1) -
             pmax(0, pnorm(t2, mean=delta2)-pnorm(t1, mean=delta1)))
  }
  out <- numeric(J)
  if(!all(i <- (is.finite(delta1) & is.finite(delta2)))){
    if(length(inf1 <- which(is.infinite(delta1)))){
      out[inf1] <- ifelse(delta1[inf1]==Inf, 1-ptOwen(t2, nu, delta2[inf1]), 0)
      # ? delta1 forcément +Inf
    }
  }
  if(any(i)){
    out[i] <- RcppOwenCDF3(nu, t1, t2, delta1[i], delta2[i], algo)
  }
  out
}

#' @rdname powen
#' @export
powen4 <- function(nu, t1, t2, delta1, delta2, algo=2){
  J <- length(delta1)
  if(J != length(delta2)){
    stop("`delta1` and `delta2` must have the same length.")
  }
  if(any(delta1<=delta2)){ #& is.finite(delta1) & is.finite(delta2))){
    stop("`delta1` must be >`delta2`.")
  }
  if(isNotPositiveInteger(nu)){
    stop("`nu` must be an integer >=1.")
  }
  if(t1<=t2){
    return(ptOwen(t2, nu, delta2)-ptOwen(t1, nu, delta1))
  }
  # if(is.infinite(t1) || is.infinite(t2)){
  #   if(t1==Inf){
  #     return(ifelse(delta1==Inf, NaN, 0))
  #   }
  #   # if(t1==-Inf){ # inutile car t1 <= t2
  #   #   indices <- delta1==-Inf
  #   #   return(ifelse(indices, NaN, ptOwen(t2, nu, delta2[!indices])))
  #   # }
  #   # if(t2==Inf){ # inutile car t1 <= t2
  #   #   indices <- delta2==Inf
  #   #   return(ifelse(indices, NaN, 1-ptOwen(t1, nu, delta1[!indices])))
  #   # }
  #   if(t2==-Inf){
  #     return(ifelse(delta2==-Inf, NaN, 0))
  #   }
  # }
  if(t1==Inf && t2==-Inf){
    return(ifelse(delta1==Inf || delta2==-Inf, NaN, 0))
  }
  if(t1==Inf){
    return(ifelse(delta1==Inf, NaN, 0))
  }
  if(t2==-Inf){
    return(ifelse(delta2==-Inf, NaN, 0))
  }
  if(nu == Inf){
    return(pmax(0, pnorm(t2, mean=delta2)-pnorm(t1, mean=delta1)))
  }
  out <- numeric(J)
  if(!all(i <- (is.finite(delta1) & is.finite(delta2)))){
    if(any(inf1 <- delta1==Inf)){
      out[inf1] <- ptOwen(t2, nu, delta2[inf1])
    }
    if(any(minf2 <- delta2==-Inf & !inf1)){
      out[minf2] <- 1 - ptOwen(t1, nu, delta1[minf2])
    }
  }
  if(any(i)){
    out[i] <- RcppOwenCDF4(nu, t1, t2, delta1[i], delta2[i], algo)
  }
  out
}
