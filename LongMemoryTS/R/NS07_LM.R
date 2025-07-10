##########################################################################################
####            Rank estimation in fractionally cointegrated systems               #######
####               Nielsen, Shimotsu (2007)                                        #######                     
##########################################################################################

#' Helper for memory estimation.
#' @keywords internal
elW_wrap<-function(X,m,mean_correct){ELW(X,m,mean_correct)$d}

#' Estimate G-matrix with periodogram of differenced data for NS07.
#' @keywords internal
G_hat_nonstat<-function (X, d, m){
    if (which.max(dim(X)) == 1) { X <- t(X)}
    T <- ncol(X)
    dim_series <- nrow(X)
    for(i in 1:dim_series){X[i,]<-diffseries(X[i,],d[i])}
    I_j <- Peri(X)
    aux.M <- matrix(0, dim_series, dim_series)
    for (j in 1:m) { aux.M <- aux.M + Re(I_j[, , j]) }
    1/m * (aux.M)
}

#' @title Rank estimation in fractionally cointegrated systems by Nielsen, Shimotsu (2007).
#' @description \code{FCI_NS07} Rank estimation in fractionally cointegrated systems by Nielsen, Shimotsu (2007).
#'  Returns estimated cointegrating rank, r=0,...,dim-1.
# #' @details add details here.
#' @param X data matrix.
#' @param m1 bandwith parameter specifying the number of Fourier frequencies
#' used for the memory estimation , usually \code{floor(1+T^delta1)}, where 0<delta1<1 and m1>m.
#' @param m  bandwith parameter specifying the number of Fourier frequencies
#' used for the estimation of G, usually \code{floor(1+T^delta)}, where 0<delta<1 and m1>m.
#' @param mean_correct specifies the form of mean correction in the memory estimation.
#' @param v_n  bandwidth parameter. Nielsen and Shimotsu (2007) use m^(-0.3) in their simulation studies, which is the default value. m^(-b) mit 0<b<0.5 can be used.
#' @references Nielsen, M. 0. and Shimotsu, K. (2007): Determining the cointegrating rank in nonstationary fractional
#' systems by the exact local Whittle approach. Journal of Econometrics, Vol. 141, No. 2, pp. 574 - 596.
#' @author Christian Leschinski, Michelle Voges
#' @examples
#' T<-1000
#' series<-FI.sim(T=T, q=2, rho=0.4, d=c(0.1,0.4), B=rbind(c(1,-1),c(0,1)))
#' FCI_NS07(series, m1=floor(1+T^0.75), m=floor(1+T^0.65))
#' series<-FI.sim(T=T, q=2, rho=0.4, d=c(0.9,0.9))
#' FCI_NS07(series, m1=floor(1+T^0.75), m=floor(1+T^0.65))

#' @export

FCI_NS07<-function(X, m, m1, mean_correct=c("mean", "init", "weighted", "none"), v_n=m^(-0.3))
{
  if (m1 < m) 
        stop("m1 must be larger than m")
  if (which.max(dim(X)) == 1) { X <- t(X)}
  T <- ncol(X)
  dim_series <- nrow(X)
  d.hat<-apply(X,1,elW_wrap, mean_correct, m=m1)
  d.bar.star <- mean(d.hat)
  G.est <- G_hat_nonstat(X = X, d = rep(d.bar.star, dim_series), m = m)
  P.mat <- sqrt(solve(diag(G.est) * diag(dim_series))) %*% G.est %*% sqrt(solve(diag(G.est) * diag(dim_series)))
  deltas <- eigen(P.mat)$values
  sig_1j <- cumsum(deltas)
  L_u <- 0
  for (u in 0:(dim_series - 1)) {
      L_u[(u + 1)] <- v_n * (dim_series - u) - sig_1j[(dim_series - u)]
  }
  r.hat <- which.min(L_u)
  return(r.hat - 1)
}
  

