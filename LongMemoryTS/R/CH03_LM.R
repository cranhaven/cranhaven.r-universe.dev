##########################################################################################
####            Rank estimation in fractionally cointegrated systems               #######
####                by Chen, Hurvich (2003)                                        #######
##########################################################################################


#library(QZ)


#'Tapered periodogram. Only for internal use. 
#'@keywords internal
peri_taper<-function(X,diff_param=1){
  
  if (which.max(dim(X)) == 1) {X <- t(X)}
  T <- ncol(X)
  dim_series <- nrow(X)
  t<-seq(1:T)
  ht<-0.5*(1-exp(1i*2*pi*t/T))
  lambdaj <- 2 * pi * (1:floor(T/2))/T
  weight.mat <- matrix(NA, T, floor(T/2))
  for (j in 1:floor(T/2)) {
    weight.mat[, j] <- ht[j]^(diff_param-1) * exp((0+1i) * (1:T) * lambdaj[j])
  }
  tap<-sum(abs(ht^(diff_param-1))^2)
  w1 <- 1/sqrt(2 * pi * tap) * X %*% weight.mat
  I.lambda <- array(NA, dim = c(dim_series, dim_series, floor(T/2)))
  for (j in 1:floor(T/2)) {
    I.lambda[, , j] <- w1[, j] %*% t(Conj(w1[, j]))
  }
  I.lambda
}

#' Averaged (tapered) periodogram. Only for internal use.
#' @keywords internal
av_peri<-function(X, diff_param=1, m_peri){
  
  #m \geq p-r, but r is unknown, however choosing m larger than p should be fine
  if (which.max(dim(X)) == 1) {X <- t(X)}
  T <- ncol(X)
  dim_series <- nrow(X)
  perid<-peri_taper(X, diff_param=diff_param)
  aux.M<-apply(perid[,,1:m_peri],c(1,2),function(x){sum(Re(x))})
  aux.M
}

#' Calculates eigenvalues and eigenvectors of averaged periodogram. 
#' Generates cointegrating residuals with help of eigenvectors.
#' @keywords internal
eigen_resids<-function(X, diff_param=1, m_peri){
  
  dim_series<-min(dim(X))
  T<-max(dim(X))
    
  I_m<-av_peri(X=X, diff_param=diff_param, m_peri=m_peri)  # calculate averaged tapered periodogram matrix
  eigen_values<-eigen(I_m)     # determine eigenvalues and vectors
  
  # generate linear combinations:
  coint_resids<-matrix(NA,dim_series,T)
  for(i in 1:min(dim(X))){coint_resids[i,]<-X%*%eigen_values$vectors[,(dim_series+1-i)]}
  
  # return result:
  list("values"=eigen_values$values[dim_series:1], "vectors"=eigen_values$vectors[,dim_series:1], "residuals"=t(coint_resids))
}

#' Helper for local Whittle estimation.
#' @keywords internal
lW_wrap<-function(data,m){local.W(data,m)$d}


#' @title Rank estimation in fractionally cointegrated systems.
#' @description \code{FCI_CH03} Rank estimation in fractionally cointegrated systems by Chen, Hurvich (2003).
#'  Returns estimated cointegrating rank.
# #' @details add details here.
#' @param X vector of length T.
#' @param diff_param integer specifying the order of differentiation in order to ensure stationarity of data, where diff_param-1 are the number of differences. 
#' Default is \code{diff_param=1}.
#' @param m_peri fixed positive integer for averaging the periodogram, where \code{m_peri>(nbr of series + 3)}
#' @param m  bandwith parameter specifying the number of Fourier frequencies
#' used for the estimation, usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @references Chen, W. W. and Hurvich, C. M. (2003): Semiparametric estimation of multivariate fractional cointegration.
#' Journal of the American Statistical Association, Vol. 98, No. 463, pp. 629 - 642.
#' @author Christian Leschinski
#' @examples
#' T<-1000
#' series<-FI.sim(T=T, q=3, rho=0.4, d=c(0.1,0.2,0.4), B=rbind(c(1,0,-1),c(0,1,-1),c(0,0,1)))
#' FCI_CH03(series,diff_param=1, m_peri=25, m=floor(1+T^0.65))
#' @export

FCI_CH03<-function(X, diff_param=1, m_peri, m){
  
  if (which.max(dim(X)) == 2) {X <- t(X)}
  dim_series<-min(dim(X))
  T<-max(dim(X))
  if(m_peri<=(dim_series+3))stop('m_peri must be larger than dimension+3')

  aux<-eigen_resids(X=X, diff_param=diff_param, m_peri=m_peri)
  
  d<-mean(apply(X,2,lW_wrap, m=m))
  d_u<-local.W(aux$residuals[,1], m=m)$d
  
  v_n<-T^(3/2*d+d_u/2)    

  deltas<-aux$values
  sig_hat<-rev(cumsum(rev(deltas)))
  
  u<-1:(dim_series-1)
  L_u<-v_n*(diff_param-u)-sig_hat[2:dim_series]
  r_hat<-which.min(L_u)
  r_hat
}
