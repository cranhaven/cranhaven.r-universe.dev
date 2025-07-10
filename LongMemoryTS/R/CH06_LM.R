##########################################################################################
####            Testing fractional cointegration                                   #######
####               Chen, Hurvich (2006)                                            #######                     
##########################################################################################


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

#' Local Whittle estimator for tapered periodogram - function to optimize
#' @keywords internal
R_lw_ch<-function (d, data, m, diff_param=1) 
{
  T.S <- length(data)
  j <- (1:m) + (diff_param-1)/2
  lambdaj <- 2 * pi *j /T.S
  peri <- per(data)[-1]
  K <- log(1/m * (sum(peri[1:m] * (lambdaj )^(2 * d)))) - 
    2 * d/m * sum(log(lambdaj ))
  K
}
#' Local Whittle estimator for tapered periodogram 
#' @keywords internal
local_W_taper<-function (data, m, diff_param=1, int = c(-0.5, 1)) 
{
  d.hat <- optimize(f = R_lw_ch, interval = int, data = data, 
                    m = m, diff_param=diff_param)$minimum
  return(d.hat)
}

#' @title Residual-based test for fractional cointegration (Chen, Hurvich (2006))
#' @description \code{FCI_CH06} Semiparametric residual-based test for fractional cointegration by Chen, Hurvich (2003).
#'  Returns test statistic, critical value and testing decision. Null hypothesis: no fractional cointegration.
# #' @details add details here.
#' @param X data matrix.
#' @param diff_param integer specifying the order of differentiation in order to ensure stationarity of data, where diff_param-1 are the number of differences. 
#' Default is \code{diff_param=1} for no differences.
#' @param m_peri fixed positive integer for averaging the periodogram, where \code{m_peri>(nbr of series + 3)}
#' @param m  bandwith parameter specifying the number of Fourier frequencies
#' used for the estimation, usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param alpha desired significance level. Default is \code{alpha=0.05}.
#' @references Chen, W. W. and Hurvich, C. M. (2006): Semiparametric estimation of fractional
#' cointegrating subspaces. The Annals of Statistics, Vol. 34, No. 6, pp. 2939 - 2979.
#' @author Christian Leschinski
#' @examples
#' T<-1000
#' series<-FI.sim(T=T, q=2, rho=0.4, d=c(0.1,0.4), B=rbind(c(1,-1),c(0,1)))
#' FCI_CH06(series, diff_param=1, m_peri=25, m=floor(T^0.65))
#' series<-FI.sim(T=T, q=2, rho=0.4, d=c(0.4,0.4))
#' FCI_CH06(series, diff_param=1, m_peri=25, m=floor(T^0.65))

#' @export

FCI_CH06<-function(X, m_peri, m, alpha=0.05, diff_param=1){
  
  dim_series<-min(dim(X))
  T<-max(dim(X))
  X<-apply(X, 2, diffseries, (diff_param-1))
  
  aux<-eigen_resids(X, diff_param=diff_param, m_peri=m_peri)
  d<-apply(aux$residuals,2,function(x){local_W_taper(x,m=m, diff_param = diff_param)})
  
  teststat<-sqrt(m)*(d[length(d)]-d[1])        # test statistic (p.2952)
  
  PHI<-(gamma(4*diff_param-3)*gamma(diff_param)^4)/gamma(2*diff_param-1)^4  # needed for critical value (upper bound for variance)
  
  crit<-sqrt(PHI/2) * qnorm(1-alpha/2)      # critical value
  dec<-teststat>crit                        # if TRUE: reject H0 / reject no cointegration
  return(list(T_n=teststat, crit.value=crit, reject=dec))
}
