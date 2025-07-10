

#' @title Log-likelihood function of a VARFIMA(1,1) in final equations form.
#' @description \code{ll.VARFIMA} returns the value of the log-likelihood function for a given sample and parameter vector.
# #' @details add details here.
#' @param theta parameter vector.
#' @param data data matrix with T observations of q-dimensional process.
#' @param q dimension of the process.
#' @param approx order of the AR-approximation that is supposed to be used. Default is \code{approx=100}.
#' @param pre.sample if likelihood is conditioned on previous observations pre.sample is an additional sample matrix.
#' @param rep determines whether the parameter vector is printed.
#' @references Lutkepohl, H. (2007): New introduction to multiple time series analysis. Springer.
#' @export

ll.VARFIMA<-function(theta,data,q,approx=100, pre.sample=matrix(0,approx,q), rep=FALSE){
  
  # recover parameter values from theta
  T<-nrow(data)
  d.vec<-theta[1:q]
  phi<-theta[q+1]
  THETA<-matrix(theta[(q+2):(q+1+q^2)],q,q)
  sigs<-theta[(q+2+q^2):(2*q+1+q^2)]
  cor.vec<-theta[(2*q+2+q^2):((2*q+1+q^2)+q*(q-1)-sum(1:(q-1)))]
  
  Sigma<-diag(q)
  count<-0
  for(b in 1:(q-1)){
    for(a in (b+1):q){
      count<-count+1
      Sigma[a,b]<-Sigma[b,a]<-cor.vec[count]
    }
  }
  
  Sigma<-sigs%o%sigs*Sigma
  
  if(rep==TRUE){
    print("Latest Paramter Values:")
    print(list("d.vec"=d.vec, "phi"=phi, "THETA"=THETA, "sigs"=sigs, "cor.vec"=cor.vec))
  }
  
residuals<-ll_inner(T=T, data=data, pre_sample=pre.sample, d_vec=d.vec, phi=phi, THETA=THETA,q=q, approx=approx)
ll<-sum(log(dmvnorm(residuals, mean=rep(0,q), sigma=Sigma)))
ll
}


