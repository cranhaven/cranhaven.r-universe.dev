

##### PDC function based on a multivariate arima model  ######
# By Guillermo Granados
# Department of Mathematics and Statistics Lancaster University

#' Quadratic multiplication of a matrix M with respect to a matrix A:
#'  Conj(M)  A M, where Conj() is the complex conjugate function

#' @param M A Matrix of dimension P by P
#' @param A A squared Matrix of dimension P by P
#' @return The squared root of the absolute values of the matrix result of the 
#' operation Conj(M) A M 
#'  
#' @export
#' @examples
#' M=matrix( rnorm(100), ncol=10  )
#' A=matrix( rnorm(100), ncol=10  )
#' sqnorms(M, A)
sqnorms<-function(M,A){ sqrt(abs( Conj( M) %*% A %*% M ) )  }




#' Partial directed coherence matrix    

#' @param unit A Matrix containing the multivariate time series. Each column 
#' represents a univariate time series.
#' @param ar  Integer vector containing all the lags considered for the
#' vector autoregressive model
#' @return An real array of dimensions, ncol(unit), ncol(unit), n, where n is the number of 
#' frequencies at which the PDC is estimated.   
#'  
#' @export
#' @examples
#' X=matrix( rnorm(2000), ncol=10  )
#' ar=c(1, 2)
#' matrix_PDC(X, ar)
matrix_PDC=function(  unit, ar ){
  kvar=ncol(unit)
  order=length(ar)
  model = marima::define.model(kvar=kvar ,  ar=ar  )
  arp = model$ar.pattern
  fit = marima::marima(unit,    ar.pattern=arp, penalty=2 )
  # penalty=2 conventional AIC criterion
  arest<- -marima::short.form(fit$ar.estimates, leading=FALSE)
  omegas=TSA::periodogram(unit[,1], plot = F)$freq  
  #PDC in complex values with a certain level of precision 
  dimens<-dim(arest)[1]
  AF<-array(1:length(omegas), c(dimens,dimens,length(omegas) ))  
  for(j in 1:length(omegas)){
    AF[,,j]<- diag(rep(1,dimens )) 
    for(i in 1:order){
      mycom<- complex( real = cos(2*pi*omegas[j]), imaginary = sin(2*pi*omegas[j]) )^i
      AF[,,j]=AF[,,j] - arest[,,i]*mycom
    }# end i
  }#end j
  # PDC in real values
  dimens<-dim(AF)[1]
  PDCmat<-array(1:length(omegas), c(dimens,dimens,length(omegas) ))
  for(j in 1:length(omegas)){  
    #solve(fit$resid.cov)   diag( rep(1,dimens) )
    sq<- apply ( AF[,,j], 2, sqnorms, A=diag( rep(1,dimens) ) )   
    PDCmat[,,j]<-  t( t(AF[,,j] )/ sq )
  }
  return(PDCmat)
}
