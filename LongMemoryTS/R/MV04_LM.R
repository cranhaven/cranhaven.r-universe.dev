##########################################################################################
####            Testing fractional cointegration                                   #######
####               Marmol, Velasco (2004)                                          #######                     
##########################################################################################

#' Critical values from MV04 paper, p.1831
#' @keywords internal
asymp_MV04<-function(alpha=c(0.01,0.05,0.1), M=c(1,2,3), type=c("no", "const", "trend"))
{
  if(alpha==0.01){ #a=1%
                    m1_no   <- c(-4.377, 15.169, -6.871,  1.136)
                    m1_const<- c( 1.624, -2.402,  5.932, -1.945)
                    m1_trend<- c( 1.495, -0.465,  1.945, -0.391)
                    m2_no   <- c(-4.852, 15.874, -6.880,  1.597)
                    m2_const<- c( 1.239, -1.813,  5.525, -1.517)
                    m2_trend<- c( 1.308, -0.268,  1.510,  0.134)
                    m3_no   <- c(-5.737, 18.609, -9.609,  3.088)
                    m3_const<- c( 0.981, -0.856,  3.875, -0.292)
                    m3_trend<- c( 1.396, -0.916,  2.193,  0.183)
  alpha1<-rbind(m1_no,m1_const,m1_trend,m2_no,m2_const,m2_trend,m3_no,m3_const,m3_trend) 
  Mm<-c(1,1,1, 2,2,2, 3,3,3)
  typ<-rep(c("no", "const", "trend"),3)
  colnames(alpha1)<-c("const.", "d", "d^2", "d^3")
  alp<-data.frame(Mm, typ, alpha1)
  }
  
  if(alpha==0.05){ #a=5%
                   m1_no   <- c(-0.798,  3.931, 1.879, -1.317)
                   m1_const<- c( 1.824, -2.518, 4.371, -1.256)
                   m1_trend<- c( 1.388, -0.271, 1.169, -0.250)
                   m2_no   <- c(-1.571,  5.614, 1.119, -0.841)
                   m2_const<- c( 1.703, -2.794, 5.094, -1.345)
                   m2_trend<- c( 1.373, -0.662, 1.634, -0.198)
                   m3_no   <- c(-2.135,  6.940, 0.367, -0.283)
                   m3_const<- c( 1.240, -1.336, 3.333, -0.361)
                   m3_trend<- c( 1.196, -0.230, 1.062,  0.250)
  alpha5<-rbind(m1_no,m1_const,m1_trend,m2_no,m2_const,m2_trend,m3_no,m3_const,m3_trend) 
  Mm<-c(1,1,1, 2,2,2, 3,3,3)
  typ<-rep(c("no", "const", "trend"),3)
  colnames(alpha5)<-c("const.", "d", "d^2", "d^3")
  alp<-data.frame(Mm, typ, alpha5)
  }
  
  if(alpha==0.1){ #a=10%
                   m1_no   <- c( 0.430,  0.303, 4.163, -1.818)
                   m1_const<- c( 1.600, -1.761, 3.074, -0.831)
                   m1_trend<- c( 1.148,  0.326, 0.332, -0.017)
                   m2_no   <- c(-0.459,  2.379, 3.075, -1.330)
                   m2_const<- c( 1.647, -2.388, 4.037, -0.985)
                   m2_trend<- c( 1.141,  0.020, 0.737,  0.012)
                   m3_no   <- c(-0.981,  3.533, 2.593, -0.951)
                   m3_const<- c( 1.349, -1.559, 3.128, -0.402)
                   m3_trend<- c( 1.148, -0.134, 0.830,  0.186)
  alpha10<-rbind(m1_no,m1_const,m1_trend,m2_no,m2_const,m2_trend,m3_no,m3_const,m3_trend) 
  Mm<-c(1,1,1, 2,2,2, 3,3,3)
  typ<-rep(c("no", "const", "trend"),3)
  colnames(alpha10)<-c("const.", "d", "d^2", "d^3")
  alp<-data.frame(Mm, typ, alpha10)
  }
  krit_help<- alp[which(alp[,1]==M),]
  krit_h <- krit_help[which(krit_help[,2]==type),]
  return(krit_h)
}


#' Beta estimation which is consistent in the spurious case and inconsistent under cointegration introduced by MV04.
#' @keywords internal
beta_N<-function(X, dx, dep, N){

                    # X is the data matrix
                    # dx is the vector of memory parameters of the regressors
                    # dep is the memory parameter of the cointegrating residuals
                    # N is a bandwidth paramter

  if (which.max(dim(X)) == 1) { X <- t(X)}
  T<-ncol(X)                       # number of observations   
  dim_series<-nrow(X)              # dimension of data
  M<-dim_series-1                  # number of regressors
  diff_data<-apply(X,1,diff)       # differenced data
  I.j <- Peri(diff_data)           # periodogram of differenced data                                                         
  
  d<-rep(mean(dx),dim_series)       # memory estimate of the regressors (needs to be equal)
  lambdaj<-2*pi*(1:floor(T/2))/T   # Fourier frequencies
  Lambda<-array(0, dim=c(dim_series,dim_series,length=floor(T/2)))      # create array for Lambda values
  for(j in 1:floor(T/2)){Lambda[,,j]<- diag(lambdaj[j]^(1-d))}          # Lambda matrices

  # Determine \hat{\Omega}_{xx}
  auxx<-0
  for(i in 1:N){
  Lambda_inv_aux<-solve(Lambda[(2:dim_series),(2:dim_series),i])
  auxx <- auxx +  Lambda_inv_aux %*% Re(I.j[(2:dim_series),(2:dim_series),i])%*% Lambda_inv_aux
  }
  omega_xx <- (2*pi/N) * auxx
  
  # determine \hat{\omega}_{xy}
  auxy<-numeric(length=M)
  for(i in 1:N){auxy <- auxy + Re(I.j[(2:dim_series),1,i])*lambdaj[i]^(2*(dep-1))}    # vorher  Re(I.j[(2:dim_series),1,i])%*%lambdaj[i]^(2*(dep-1))
  omega_xy <- (2*pi/N)*auxy 
  
  beta_N <- solve(omega_xx) %*% omega_xy
  return(beta_N)
}

#' Variance for test statistic MV04.
#' @keywords internal
V_M <- function (x, M, m, residual, type=c("no", "const", "type")){
  
  # x is the regressor vector (matrix)
  # M is the dimension of the regressor vector (matrix)
  # m is the usual bandwidth
  # residual is the residual from the OLS regression of y on x (and potentially the trend)
  if(M==1){T <- length(x)                                 # determine length of univariate regressor 
        }else{
        if (which.max(dim(x)) == 1) { x <- t(x)
                                      T <- ncol(x) }    # determine length of multivariate regressor
        }                            
  # definition of potential constant or trend
  if( type == "no")    {qt<-NULL}
  if( type == "const") {qt<-t(rep(1,T))}
  if( type == "trend") {qt<-t(cbind(rep(1,T),seq(1:T)))}
 
  # calculate detrended regressors
  I_xq<-cross_Peri(x,qt,m)
  if(type!="no"){
    I_qq<-cross_Peri(qt,qt,m)
    x_qt<-x-apply(I_xq,c(1,2),sum)%*%solve(apply(I_qq,c(1,2),sum))%*%qt
  }else{
    x_qt<-x
  }
  
  I_xqxq<-cross_Peri(x_qt,x_qt,m)
  I_xixi<-cross_Peri(residual,residual,m)

  # calculate variance matrix
  SumI_xqxq<-apply(I_xqxq,c(1,2),sum)
  enumerator<-matrix(0, M, M)
  for(i in 1:(2*m+1)){enumerator<-enumerator+I_xqxq[,,i]*I_xixi[,,i]}
  
  V<-solve(SumI_xqxq)%*%enumerator%*%solve(SumI_xqxq)
  
  # return result
  Re(V)
}

#' Cross periodogram for variance estimation in MV04.
#' @keywords internal
cross_Peri<-function(X,Y,m){
  X<-as.matrix(X)
  Y<-as.matrix(Y)
  if(which.max(dim(X))==1){X<-t(X)}         # convert matrix in q x n - dimensional matrix if it is n x q
  if(which.max(dim(Y))==1){Y<-t(Y)}         # convert matrix in q x n - dimensional matrix if it is n x q
  Tx<-ncol(X)                                # number of observations
  qx<-nrow(X)                                # number of dimensions
  Ty<-ncol(Y)
  qy<-nrow(Y)
  if(Tx!=Ty)stop("X and Y must have same length.")
  n<-Ty
  lambdaj<-2*pi*(-m:m)/n            # vector of fourier frequencies
  weight.mat<-matrix(NA,n,floor(n/2))       # create weight matrix
  for(j in 1:floor(n/2)){weight.mat[,j]<-exp(1i*(1:n)*lambdaj[j])}    # fill weight matrix
  wx<-1/sqrt(2*pi*n)*X%*%weight.mat                                  # matrix w1 contains discrete fourier transform
  wy<-1/sqrt(2*pi*n)*Y%*%weight.mat 
  I.lambda<-array(NA,dim=c(qx,qy,floor(n/2)))                          # create periodogram array
  for(j in 1:floor(n/2)){I.lambda[,,j]<-wx[,j]%*%t(Conj(wy[,j]))}          # fill periodogram array with periodogram matrices w1%*%t(w2)
  array(I.lambda, dim=c(qx,qy,(2*m+1)))                                       # return periodogram
}


#' @title Test for fractional cointegration (Marmol, Velasco (2004))
#' @description \code{FCI_MV04} Semiparametric test for fractional cointegration by Marmol, Velasco (2004).
#'  Returns test statistic, critical value and testing decision. Null hypothesis: no fractional cointegration.
# #' @details add details here.
#' @param X data matrix.
#' @param type string that is either \code{"none"}, \code{"const"}, or \code{"trend"} and determines the form of linear regression.
#' @param N bandwidth parameter specifying the number of Fourier frequencies
#' used for the beta estimation, usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param m  bandwith parameter specifying the number of Fourier frequencies
#' used for the memory parameter estimation, usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param alpha desired significance level. Default is \code{alpha=0.05}.
#' @references Marmol, F. and Velasco, C. (2004): Consistent testing of cointegrating relationships.
#' Econometrica, Vol. 72, No. 6, pp. 1809 - 1844.
#' @author Christian Leschinski, Michelle Voges
#' @examples
#' T<-500
#' series<-FI.sim(T=T, q=2, rho=0.1, d=c(0.6,1), B=rbind(c(1,-1),c(0,1)))
#' FCI_MV04(series, type="const", N=floor(T^(0.75)), m=floor(T^(2/3)))
#' series<-FI.sim(T=T, q=2, rho=0.1, d=c(0.8,0.8))
#' FCI_MV04(series, type="const", N=floor(T^(0.75)), m=floor(T^(2/3)))
#' @export


FCI_MV04 <- function(X, type=c("none", "const", "trend"), N, m, alpha=0.05)
{if (which.max(dim(X)) == 1) { X <- t(X)}
 T<-ncol(X)                       # number of observations   
 dim_series<-nrow(X)              # number of variables
 if(dim_series>4){stop("no critical values for data of this dimension")}
 M<-dim_series-1                  # for limiting distribution
 y<- X[1,]                        # regressand
 if(M==1){
 x<- X[(2:dim_series),]          # regressor(s)  
 }else{
 x<- t(X[(2:dim_series),])          # regressor(s)  
 }
 if( type == "none")
 {   reg<-summary(lm(y~x-1))    # OLS regression without constant
     helper<-0               }  # helper for beta_ols
 if( type == "const")
 {   reg<-summary(lm(y~x))      # OLS regression with constant
     helper<-1               }  # helper for beta_ols
 if( type == "trend")
 {   t<-seq(1:T)                # linear time trend  
     reg<-summary(lm(y~t+x))    # OLS regression with constant and trend
     helper<-2               }  # helper for beta_ols

 residual<-as.vector(reg$residuals)                               # regession residuals 
 beta_ols<-as.vector(reg$coefficients[((helper+1):(helper+M)),1]) # beta estimates
 
 diff_regressor <- apply(t(x),1,diff)                      # differenced data (because of nonstationarity assumption)
 d_x <- GSE(diff_regressor, m)+1                           # memory estimation of differenced regressor  
 d_res<-local.W(diff(residual), m)$d+1                     # memory estimation of residuals
 beta_n <- as.vector(unlist(beta_N(X,d_x,d_res,N) ))       # estimate of beta_0^N 
 v <- V_M(x,M,m,residual,type)                             # variance calculation
 W <- as.numeric((1/M) * t(beta_ols-beta_n) %*% solve(v) %*% (beta_ols-beta_n))# teststatistic
 
 value <- as.numeric(asymp_MV04(alpha, M, type))    # calculation of critical value
 dx <- mean(d_x)
 d <- c(1, dx, dx^2, dx^3)
 crit <- as.numeric((as.vector(t(value[3:6]))%*%d)^4)
 dec <- W>crit
 
 return(list(W=W, crit.value=crit, reject=dec))
}
