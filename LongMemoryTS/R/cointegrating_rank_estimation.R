

################################################################################################
####                                                                                        ####
####                     Cointegration rank estimation                                      ####
####                                                                                        ####
####    as in Nielsen and Shimotsu (2007, JoE) and Robinson and Yajima (2002, JoE)          ####
####                                                                                        ####
################################################################################################


#'Test for equality of two elements d_a and d_b of estimated d vector. 
#'This function should not be called directly. It is called as a helper by T.rho.
#'@keywords internal

Tab<-function(d.hat,G.est,m1,a,b,h_n){

  # d.hat is estimated d.vector
  # G.est is estimated G matrix
  # m1 is the bandwidth used for the estimation of d and G
  # a,b specify the elements of d.hat that are supposed to be tested for equality
  # h_n is a user determined parameter

if(a>b)stop("a has to be smaller than b")
if(b>length(d.hat))stop("b can not be higher than dimension of the process")
da<-d.hat[a]
db<-d.hat[b]
Tab<-sqrt(m1)*(da-db)/((1/2*(1-G.est[a,b]^2/(G.est[a,a]*G.est[b,b])))^(1/2)+h_n)
return(Tab)
}

##############          ###########################

#' @title Test for equality of all elements in an estimated d-vector based on pairwise comparisons.
#' @description \code{T.rho} Uses pairwise test as suggested by Robinson and Yajima (2002) to test
#' for the equality of the memory parameters in a vector series.
# #' @details add details here.
#' @param data data matrix of dimension (qxT).
#' @param d.hat the estimated d.vector
#' @param m bandwith parameter specifying the number of Fourier frequencies.
#' used for the estimation of G, usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param m1 the bandwidth parameter used for estimation of d.vec with m1>>m
#' @param alpha the desired significance level for the tests
#' @param s_bar number of subvectors to be tested in partitioning procedure. 
#'        Default is \code{s_bar=1}, for independent use.
#' @param h_n bandwidth parameter. Default is \code{h_n=1/sqrt(log(max(dim(data))))} 
#'        which is recommended by Nielsen and Shimotsu (2007) in their simulation study. 
#'        Robinson and Yajima (2002) argue non-rejection with h_n=0 would imply 
#'        non-rejection with any h_n>0.  
#' @author Christian Leschinski
#' @references Robinson, P. M. and Yajima, Y. (2002): Determination of cointegrating rank
#'            in fractional systems. Journal of Econometrics, Vol. 106, No.2, pp. 217-241. 
#'            
#'            Nielsen, M. O. and Shimotsu, K. (2007): Determining the coinegrating rank in 
#'            nonstationary fractional systems by the exact local Whittle approach. Journal of Econometrics,
#'            141, pp. 574-596.
#' @examples
#' library(fracdiff)
#' T<-1000
#' d1<-0.2
#' d2<-0.4
#' X<-cbind(fracdiff.sim(n=T,d=d1)$series,fracdiff.sim(n=T,d=d1)$series,
#' fracdiff.sim(n=T,d=d2)$series,fracdiff.sim(n=T,d=d2)$series)
#' alpha<-0.05
#' m1<-floor(1+T^0.75)
#' m<-floor(1+T^0.65)
#' lW.wrap<-function(data,m){local.W(data,m)$d}
#' d.hat<-apply(X,2,lW.wrap, m=m1)
#' T.rho(data=X, d.hat=d.hat, m=m, m1=m1)
#' @export

T.rho<-function(data,d.hat,m,m1,alpha=0.05,s_bar=1,h_n=1/sqrt(log(max(dim(data))))){

  # data is pxT data matrix
  # d.hat is estimated d.vector
  # m1 is the bandwidth used in the estimation of d.hat
  # h_n is a user chosen bandwidth parameter. Default is h_n=1/sqrt(log(T)). 
  
  # Robinson and Yajima (2002) argue non-rejection with h_n=0 would imply non-rejection with any h_n>0.
  # Nielsen and Shimotsu (2007) report that their simulation studies worked best for 1/sqrt(log(T)). 
  # More rejections with smaller h_n.
  
n_l<-ncol(data)
G.est<-G.hat(X=data,d=d.hat,m=m)
combinations<-matrix(NA,(n_l*(n_l-1)/2),2)
count<-1
for(i in 1:(n_l-1)){
for(j in (i+1):n_l){
combinations[count,]<-c(i,j)
count<-count+1}}

stats<-0
for(i in 1:nrow(combinations)){stats[i]<-Tab(d.hat=d.hat,G.est=G.est,m1=m1,a=combinations[i,1],b=combinations[i,2],h_n)}
T.rho<-max(abs(stats))
level<-2*(alpha)/(s_bar*n_l*(n_l-1))
crit<-qnorm((1-level/2),mean=0,sd=1)
p.val<-1-pnorm(T.rho)
out<-list("T.stat"=T.rho, "crit"=crit, "p.val"=p.val, "level"=level)
return(out)
}

#T.rho(data, d.hat=d.hat, m=m, m1=m1, alpha=0.01)

#' @title Test for equality of all elements in an estimated d-vector based. 
#' @description \code{T0stat} tests equality of all memory parameters simultaneously.
#'              Statistic was suggested by Robinson and Yajima (2002). Test statistic
#'              was denoted by T_0 in Nielsen and Shimotsu (2007).
#' @details add details here.
#' @param data data matrix of dimension (qxT).
#' @param d.hat the estimated d.vector
#' @param m bandwith parameter specifying the number of Fourier frequencies.
#' used for the estimation of d, usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param m1 the bandwidth parameter used for estimation of d.vec with m1>>m
#' @param alpha the desired significance level for the tests
#' @param s_bar number of subvectors to be tested in partitioning procedure. 
#'        Default is \code{s_bar=1}, for independent use.
#' @param h_n bandwidth parameter. Default is \code{h_n=1/sqrt(log(max(dim(data))))} 
#'        which is recommended by Nielsen and Shimotsu (2007) in their simulation study. 
#'        Robinson and Yajima (2002) argue non-rejection with h_n=0 would imply 
#'        non-rejection with any h_n>0.  
#' @references Robinson, P. M. and Yajima, Y. (2002): Determination of cointegrating rank
#'            in fractional systems. Journal of Econometrics, Vol. 106, No.2, pp. 217-241. 
#'            
#'            Nielsen, M. O. and Shimotsu, K. (2007): Determining the coinegrating rank in 
#'            nonstationary fractional systems by the exact local Whittle approach. Journal of Econometrics,
#'            141, pp. 574-596.
#' @examples
#' library(fracdiff)
#' T<-1000
#' d1<-0.2
#' d2<-0.4
#' X<-cbind(fracdiff.sim(n=T,d=d1)$series,fracdiff.sim(n=T,d=d1)$series,
#' fracdiff.sim(n=T,d=d2)$series,fracdiff.sim(n=T,d=d2)$series)
#' alpha<-0.05
#' m1<-floor(1+T^0.75)
#' m<-floor(1+T^0.65)
#' lW.wrap<-function(data,m){local.W(data,m)$d}
#' d.hat<-apply(X,2,lW.wrap, m=m1)
#' T0stat(data=X, d.hat=d.hat, m=m, m1=m1)
#' @export

T0stat<-function(data, d.hat,m, m1, alpha=0.05, s_bar=1, h_n=1/sqrt(log(max(dim(data))))){
  
  # T0 statistic to test for equality of all elements in d.hat from p. 228
  # in Robinson and Yajima (2002).
  # m1 used for estimation of d,
  # m<m1 is bandwidth for estimation of G
  # h_n is a user chosen bandwidth parameter. Nielsen and Shimotsu suggest
  # h_n=1/sqrt(log(T))
  
  G.est<-G.hat(data, d.hat, m=m)
  
  q<-length(d.hat)
  D1<-solve(diag(G.est)*diag(q))
  S<-cbind(diag((q-1)),-matrix(1,(q-1),1))
  
  T0<-m1*t(S%*%d.hat)%*%solve(1/4*S%*%D1%*%(G.est*G.est)%*%D1%*%t(S)+h_n^2*diag((q-1)))%*%S%*%d.hat
                             
  p.val<-1-pchisq(T0, df=(q-1))
  level<-(alpha)/(s_bar)
  crit<-qchisq(1-level, df=(q-1))
  return(list("T.stat"=T0, "crit"=crit, "p.val"=p.val, "level"=level))
}

#d.hat<-apply(data,2,local.W, m=m1)

#T0stat(data=data, d.hat=d.hat, m=m, m1=m1, h_n=1/sqrt(log(T)))


##############  Cointegration Rank Estimation using Model Selection            ###################

#' @title Cointegration Rank Estimation using Model Selection. 
#' @description Model selection procedure to estimate the cointegrating rank 
#'              based on eigenvalues of correlation matrix P suggested by
#'              Robinson and Yajima (2002). 
#' @details add details here.
#' @param data data matrix of dimension (qxT).
#' @param d.hat the estimated d.vector
#' @param m bandwith parameter specifying the number of Fourier frequencies.
#' used for the estimation of d, usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param m1 the bandwidth parameter used for estimation of d.vec with m1>>m
#' @param v_n bandwidth parameter. Nielsen and Shimotsu (2007) use m^(-0.3) in their 
#'        simulation studies, which s the default value. m^(-b) mit 0<b<0.5 can be used.
#' @references Robinson, P. M. and Yajima, Y. (2002): Determination of cointegrating rank
#'            in fractional systems. Journal of Econometrics, Vol. 106, No.2, pp. 217-241. 
#'            
#'            Nielsen, M. O. and Shimotsu, K. (2007): Determining the coinegrating rank in 
#'            nonstationary fractional systems by the exact local Whittle approach. Journal of Econometrics,
#'            141, pp. 574-596.
#' @examples
#' library(fracdiff)
#' T<-2000
#' d<-0.4
#' m1<-floor(1+T^0.75)
#' m<-floor(1+T^0.65)
#' xt<-fracdiff.sim(n=T, d=d)$series
#' yt<-xt+rnorm(T)
#' zt<-xt+rnorm(T)
#' X<-cbind(xt,yt,zt)
#' lW.wrap<-function(data,m){local.W(data,m)$d}
#' d.hat<-apply(X,2,lW.wrap, m=m1)
#' rank.est(data=X, d.hat, m=m, m1=m1)
#' @export

rank.est<-function(data,d.hat,m,m1,v_n=m^(-0.3)){
  
  # data is pxT data matrix
  # d.hat is estimated d.vector
  # m1 is the bandwidth used in the estimation of d.hat
  # m<m1 is the bandwidth used to estimate G for rank test
  # v_n is another user chosen bandwidth parameter. 
  # Nielsen and Shimotsu (2007) use m^(-0.3) in their simulation studies. m^(-b) mit 0<b<0.5 can be used.
  
if(m1<m)stop("m1 must be larger than m")
p<-ncol(data)  
d.bar.star<-mean(d.hat)
G.est<-G.hat(X=data, d=rep(d.bar.star,p), m=m)
#deltas<-eigen(G.hat)$values
P.mat<-sqrt(solve(diag(G.est)*diag(p)))%*%G.est%*%sqrt(solve(diag(G.est)*diag(p)))
deltas<-eigen(P.mat)$values
sig_1j<-cumsum(deltas)
L_u<-0
for(u in 0:(p-1)){L_u[(u+1)]<-v_n*(p-u)-sig_1j[(p-u)]}
r.hat<-which.min(L_u)
return(r.hat-1)
}


################## Example ############################

#T<-250
#data<-cbind(fracdiff.sim(T, d=0.1)$series,fracdiff.sim(T, d=0.4)$series)
#fd.series<-fracdiff.sim(T, d=0.2)$series
#data<-cbind(fd.series+rnorm(T),fd.series+rnorm(T))
#ts.plot(data,col=1:2)
#ts.plot(data[,1]-data[,2])
#acf(data[,1]-data[,2])
#m1=floor(T^0.75)
#m=floor(T^0.6)
#v_n=m^(-0.3)

#d.hat<-apply(data,2,ELW, m=m1)
#G.hat<-G.est(data, d.vec, m=m)
#T0stat(d.vec=d.hat, m1=m1, G.hat=G.hat, h_n=1/sqrt(log(T)))
#rank.est(data,d.hat,m,m1,v_n)


