#' weightW
#' 
#' Weight function \code{ktaucenters}
#' @param arg An 1-D array containing the distances.
#' @param p the dimension of the element   
#' @return an array of the same size of \code{arg} with the value of the weights

weightW=function(arg,p){

  cconstant <- ktaucenters::normal_consistency_constants(p)
  Zarg <- (arg==0);
  ret <- ktaucenters::psiOpt(arg,cc=cconstant)/(2*arg)
  ret[Zarg] <- ktaucenters::derpsiOpt(arg[Zarg],cc=cconstant)/2
  ret
}



#' klfor2normals
#' Compute the Kullback-Leibler divergence for 2 normal multivariate distributions
#' @param theta1.mu the location parameter of the first distribution
#' @param theta1.sigma the covariance matrix of the first distribution
#' @param theta2.mu the location parameter of the second distribution
#' @param theta2.sigma the covariance matrix of the second distribution
#' @return the K-L divergence.
klfor2normals<-function(theta1.mu,theta1.sigma,theta2.mu,theta2.sigma){

  sigma2inv=MASS::ginv(theta2.sigma)
  sigma2inv_mult_sigma1=sigma2inv%*%theta1.sigma;
  
  ter1=sum(diag(sigma2inv_mult_sigma1)) - 
    log(abs(det(sigma2inv_mult_sigma1)))-
    dim(sigma2inv_mult_sigma1)[1];
  
  ter2=t((theta1.mu -theta2.mu ))%*% sigma2inv%*%(theta1.mu -theta2.mu );
  ret=0.5*(ter1+ter2) 
}




#' sumkl
#' The sum of K-L divergence measure between two successive iterations 
#' for each component of a mixture distribution,
#' 
#' @param thetaNew.mu the location parameters of the first distribution
#' @param thetaNew.sigma the covariance matrix of the first distribution
#' @param thetaOld.mu the location parameter of the second distribution
#' @param thetaOld.sigma the covariance matrix of the second distribution
#' @return the K-L divergence.
sumkl=function(thetaNew.mu,thetaNew.sigma,thetaOld.mu,thetaOld.sigma){
  KK=length(thetaNew.mu)
  aux=rep(0,KK)
  for (j1 in 1:KK){
    aux[j1]=klfor2normals(theta1.mu=thetaNew.mu[[j1]],
                          theta1.sigma=thetaNew.sigma[[j1]],
                          theta2.mu=thetaOld.mu[[j1]],
                          theta2.sigma=thetaOld.sigma[[j1]])
  }
  ret=sum(aux)
  ret 
}


##########################################################################
##########################################################################
#################### test kl vs klfor2normals ############################ 
##########################################################################


# value1=klfor2normals(theta1.mu=salGenerarDatos$actualMu[[j1]],
#                      theta1.sigma=salGenerarDatos$actualSigma[[j1]],
#                      theta2.mu=salGenerarDatos$actualMu[[j1]]+4,
#                      theta2.sigma=salGenerarDatos$actualSigma[[j1]]*0.15)
# 
# 
# value2=kl(theta0.mu=list(salGenerarDatos$actualMu[[j1]]),
#           theta0.sigma=list(salGenerarDatos$actualSigma[[j1]]),
#           theta.mu=list(salGenerarDatos$actualMu[[j1]]+4),
#           theta.sigma=list(salGenerarDatos$actualSigma[[j1]]*0.15),
#           theta.alpha = 1,theta0.alpha = 1,n = 1000)
# 
# #test1: 
# abs(round(value1,2)-round(value2[1],2))==0
# 
# abs(value2[1]-value1)< (value2[3]-value2[1])
# 

##########################################################################
##########################################################################
##########################################################################
