##########################################################################################
####            Testing fractional cointegration                                   #######
####               Wang, Wang, Chan (2015)                                         #######                     
##########################################################################################


#' Periodogram for positive and negative Fourier frequencies.
#' @keywords internal
 Peri_sym<-function(X){
 if (which.max(dim(X)) == 1) {
     X <- t(X)
 }
 T <- ncol(X)  
 dim_series <- nrow(X)
  lambdaj<-2*pi*((-T/2):(T/2))/T
  TT<-length(lambdaj)
  weight_mat <- matrix(NA, T, TT)
  for (j in 1:TT) {
      weight_mat[, j] <- exp(1i*(1:T)*lambdaj[j])
  }
  w1 <- 1/sqrt(2 * pi * T) * X %*% weight_mat
  I.lambda <- array(NA, dim = c(dim_series, dim_series, TT))
  for (j in 1:TT) {
      I.lambda[, , j] <- w1[, j] %*% t(Conj(w1[, j]))
  }
  array(I.lambda, dim=c(dim_series,dim_series,TT))  
}

#' @title Semiparametric test for fractional cointegration (Wang, Wang, Chan (2015))
#' @description \code{FCI_WWC15} Semiparametric implementation of the testing strategy for fractional cointegration by Wang, Wang, Chan (2015).
#'  Returns test statistic, critical value and testing decision. Null hypothesis: no fractional cointegration.
# #' @details add details here.
#' @param X bivariate data matrix.
#' @param m  bandwith parameter specifying the number of Fourier frequencies
#' used for the estimation, usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param mean_correct specifies the form of mean correction in the memory estimation. 
#' @param alpha desired significance level. Default is \code{alpha=0.05}.
#' @references Wang, B., Wang, M. and Chan, N. H. (2015): Residual-based test for fractional cointegration. Economics Letters, Vol. 126, pp. 43 - 46.
#'  
#' Hualde, J. (2013): A simple test for the equality of integration orders. Economics Letters, Vol. 119, No. 3, pp. 233 - 237.
#' @author Christian Leschinski, Michelle Voges
#' @examples
#' T<-1000
#' series<-FI.sim(T=T, q=2, rho=0.4, d=c(0.1,0.8), B=rbind(c(1,1),c(0,1)))
#' FCI_WWC15(series,  m=floor(1+T^0.65))
#' series<-FI.sim(T=T, q=2, rho=0.4, d=c(0.8,0.8))
#' FCI_WWC15(series,  m=floor(1+T^0.65))
#' @export


FCI_WWC15<-function(X, m, mean_correct=c("init", "mean", "weighted", "none"), alpha=0.05){
 if (which.max(dim(X)) != 1) {
     X <- t(X)
 }
 if(ncol(X)>2)
    stop("Only bivariate data is allowed.")
y <- X[,1]
x <- X[,2]
T <- length(x)
if(is.integer(T/2)){ dim_helper <- T+1}else{ dim_helper <- T }

d_x<-ELW(x,m,mean_correct)$d                     # memory estimation of regressor
#d_x <- mean(c(ELW(y,m,mean_correct)$d, ELW(x,m,mean_correct)$d))            # memory is supposed to be equal, so that averaging improves the performance slightly

betta<-as.numeric(FDLS(x,y,m))
residual <- y - betta*x

d_res<-ELW(residual,m, mean.est="mean")$d        # memory of residuals (supposed to be stationary -> mean as mean correction)

diff_x<-fdiff(x, d_x)                    # difference x with own memory
diff_x_res<-fdiff(x, d_res)              # difference x with residual memory

d_y <- ELW(y, m, mean_correct)$d                 # estimate memory of regressand
diff_y <- fdiff(y, d_y)                  # difference y with own memory

I_diff <- Peri_sym(cbind(diff_y, diff_x))    # Periodogram of differenced data (own memory)
f0 <- matrix(0, 2,2)                         # semiparametric spectral denstiy estimate of Hualde (2013)
for(i in (ceiling(dim_helper/2)-m):(ceiling(dim_helper/2)+m)){ f0 <- f0 + I_diff[,,i] }
f0 <- f0/(2*m+1)
F_hat <- (T^(-0.5) * sum(diff_x_res))/(sqrt(2*pi*f0[2,2]))

krit<-qnorm(1-alpha/2)                            # critical value from standard normal
dec <- abs(F_hat)>krit
return(list("F"=abs(F_hat), "crit"=krit, "reject"=dec))
}
