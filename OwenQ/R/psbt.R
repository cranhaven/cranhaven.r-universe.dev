#' @title Owen distribution functions
#' @description Evaluates the Owen cumulative distribution function
#' for an integer number of degrees of freedom.
#' \itemize{
#' \item \code{psbt1} evaluates \ifelse{html}{\out{P(T<sub>1</sub> &le; t<sub>1</sub>, T<sub>2</sub> &le; t<sub>2</sub>)}}{\eqn{P(T_1\le t_1, T_2 \le t_2)}}
#' \item \code{psbt2} evaluates \ifelse{html}{\out{P(T<sub>1</sub> &le; t<sub>1</sub>, T<sub>2</sub> > t<sub>2</sub>)}}{\eqn{P(T_1\le t_1, T_2 > t_2)}}
#' \item \code{psbt3} evaluates \ifelse{html}{\out{P(T<sub>1</sub> > t<sub>1</sub>, T<sub>2</sub> > t<sub>2</sub>)}}{\eqn{P(T_1> t_1, T_2 > t_2)}}
#' \item \code{psbt4} evaluates \ifelse{html}{\out{P(T<sub>1</sub> > t<sub>1</sub>, T<sub>2</sub> &le; t<sub>2</sub>)}}{\eqn{P(T_1> t_1, T_2 \le t_2)}}
#' }
#' @name psbt
#' @param nu integer greater than \eqn{1}, the number of degrees of freedom;
#' infinite allowed
#' @param t1,t2 two numbers, positive or negative, possibly infinite
#' @param delta1,delta2 two vectors of possibly infinite numbers with the same length,
#' the noncentrality parameters
#' @param algo the algorithm used, \code{1} or \code{2}
#' @return A vector of numbers between \eqn{0} and \eqn{1},
#' possibly containing some \code{NaN}.
#' @note When the number of degrees of freedom is odd, the procedure resorts to
#' the Owen T-function (\code{\link{OwenT}}).
#' @seealso It is better to use \code{\link{powen}} if \code{delta1>delta2}.
#' @references
#' Owen, D. B. (1965).
#' A special case of a bivariate noncentral t-distribution.
#' \emph{Biometrika} \bold{52}, 437-446.
#' @examples
#' nu=5; t1=1; t2=2; delta1=2; delta2=3
#' ( p1 <- psbt1(nu, t1, t2, delta1, delta2) )
#' ( p2 <- psbt2(nu, t1, t2, delta1, delta2) )
#' ( p3 <- psbt3(nu, t1, t2, delta1, delta2) )
#' ( p4 <- psbt4(nu, t1, t2, delta1, delta2) )
#' # the sum should be 1
#' p1+p2+p3+p4
NULL

#' @rdname psbt
#' @export
psbt1 <- function(nu, t1, t2, delta1, delta2, algo=2){
  L <- length(delta1)
  if(L != length(delta2)){
    stop("`delta1` and `delta2` must have the same length.")
  }
  out <- numeric(L)
  higher <- (delta1 > delta2) # | is.infinite(delta1) | is.infinite(delta2)
  if(J <- length(whigher <- which(higher))){
    out[whigher] <- powen1(nu, t1, t2, delta1[whigher], delta2[whigher], algo)
  }
  if(J < L){
    equal <- delta1==delta2
    if(K <- length(wequal <- which(equal))){
      out[wequal] <- ptOwen(min(t1,t2), nu, delta1[wequal])
    }
    if(J+K < L){
      wlower <- which(!(higher|equal))
      out[wlower] <- powen1(nu, t2, t1, delta2[wlower], delta1[wlower], algo)
    }
  }
  out
}

#' @rdname psbt
#' @export
psbt2 <- function(nu, t1, t2, delta1, delta2, algo=2){
  L <- length(delta1)
  if(L != length(delta2)){
    stop("`delta1` and `delta2` must have the same length.")
  }
  out <- numeric(L)
  higher <- (delta1 > delta2) # | is.infinite(delta1) | is.infinite(delta2)
  if(J <- length(whigher <- which(higher))){
    out[whigher] <- powen2(nu, t1, t2, delta1[whigher], delta2[whigher], algo)
  }
  if(J < L){
    equal <- delta1==delta2
    if((K <- length(wequal <- which(equal))) && t2 < t1){
      out[wequal] <- ptOwen(t1, nu, delta1[wequal]) -
        ptOwen(t2, nu, delta1[wequal])
    }
    if(J+K < L){
      wlower <- which(!(higher|equal))
      out[wlower] <- powen4(nu, t2, t1, delta2[wlower], delta1[wlower], algo)
    }
  }
  out
}

#' @rdname psbt
#' @export
psbt3 <- function(nu, t1, t2, delta1, delta2, algo=2){
  L <- length(delta1)
  if(L != length(delta2)){
    stop("`delta1` and `delta2` must have the same length.")
  }
  out <- numeric(L)
  higher <- (delta1 > delta2) # | is.infinite(delta1) | is.infinite(delta2)
  if(J <- length(whigher <- which(higher))){
    out[whigher] <- powen3(nu, t1, t2, delta1[whigher], delta2[whigher], algo)
  }
  if(J < L){
    equal <- delta1==delta2
    if(K <- length(wequal <- which(equal))){
      out[wequal] <- 1 - ptOwen(max(t1,t2), nu, delta1[wequal])
    }
    if(J+K < L){
      wlower <- which(!(higher|equal))
      out[wlower] <- powen3(nu, t2, t1, delta2[wlower], delta1[wlower], algo)
    }
  }
  out
}

#' @rdname psbt
#' @export
psbt4 <- function(nu, t1, t2, delta1, delta2, algo=2){
  L <- length(delta1)
  if(L != length(delta2)){
    stop("`delta1` and `delta2` must have the same length.")
  }
  out <- numeric(L)
  higher <- (delta1 > delta2) # | is.infinite(delta1) | is.infinite(delta2)
  if(J <- length(whigher <- which(higher))){
    out[whigher] <- powen4(nu, t1, t2, delta1[whigher], delta2[whigher], algo)
  }
  if(J < L){
    equal <- delta1==delta2
    if((K <- length(wequal <- which(equal))) && t2 > t1){
      out[wequal] <- ptOwen(t2, nu, delta1[wequal]) -
        ptOwen(t1, nu, delta1[wequal])
    }
    if(J+K < L && t1 < t2){
      wlower <- which(!(higher|equal))
      out[wlower] <- powen2(nu, t2, t1, delta2[wlower], delta1[wlower], algo)
    }
  }
  out
}
