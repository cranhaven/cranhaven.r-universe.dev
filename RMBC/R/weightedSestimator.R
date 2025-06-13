#' weightedSestimator
#'
#' Computes the weighted location and scatter matrix estimators 
#' of the j-th mixture component , where the weights 
#' are calculated in the expectation-step.
#' @param Y A matrix of size n x p.
#' @param mu_init The previously computed center: an numerical array 
#' of length p.
#' @param sigma_init The previously computed scatter matrix: an array of numeric 
#' values p x p 
#' @param max_iterFP the maximum number of fixed point iterations  used for the
#' algorithm, defaults to 1
#' @param weights The weights that contain the probability membership of each 
#' observation (related to the overall mixture components)
#' @param fixed_alpha the fixed alpha value for the corresponding mixture component
#' @return A list including the estimated K centers and labels for the
#'     observations  
#' list(cov=matrixSigma,covAux1=covAux1,mu=muk,s=sk)
#' \itemize{
#' \item{\code{cov}}{:the computed weithted scatter matrix}
#' \item{\code{mu}}{: the computed weithted center}
#' \item{\code{s}}{: the weighted scale factor s.}
#' }



#################################
weightedSestimator=function(Y,mu_init,sigma_init,max_iterFP=1,weights,fixed_alpha){
  muk=mu_init
  n=dim(Y)[1]
  p=dim(Y)[2]
  dik=mahalanobis(Y,muk,MASS::ginv(sigma_init), inverted = TRUE);
  weightsNorm=weights/fixed_alpha;
  # weight are written as is in the Victor's proof.
  
  for (iter in 1:max_iterFP){
    
    sk=weightedMscale(sqrt(dik),b=0.5, weights =  weightsNorm,
                              c=ktaucenters::normal_consistency_constants(p),initialsc = 0)
    
    
    wi=weights*weightW(sqrt(dik)/sk,p)/fixed_alpha
    wi_normalized=wi/sum(wi)
    muk=colSums(sweep(Y,MARGIN=1,wi_normalized,`*`))
    acum=matrix(0,nrow=p,ncol=p);
    for (i in 1:n){
      acum = acum + (wi[i])* as.matrix(Y[i,]-muk)%*%as.matrix(t(Y[i,]-muk))
    }
    
    sigmak=acum;
    sigmakNor=sigmak;
    dik=mahalanobis(Y,muk,MASS::ginv(sigmakNor),inverted = TRUE);
  }
  
    sk=weightedMscale(sqrt(dik),b=0.5,weights = weightsNorm, 
                      c=ktaucenters::normal_consistency_constants(p),initialsc = 0)
  
  
  ## At this point we estimate the shape of the scatter matrix,
  # in order to estimate the "size" we could follow the practical rule
  ## (Maronna Martin y Yohai, 2006) at section 6.3.2 libro!
  #cSize=median(dik)/qchisq(0.5,p)
  
  ## On the other hand, due to we know that the right
  ## actualSigma= c*Sigma0 should have scale equal to 1.
  ## S( sqrt(d(xi,mu,cSigma0)))=1.
  ##  as the S is homogeneous of grade 1,
  ##  and the mahalanobis distance is homogeneous of grade -1
  ##  we know that S( sqrt(d(xi,mu,Sigma0)))=sqrt(c).
  ## Therefore el valor de c =  S( sqrt(d(xi,mu,Sigma0)))^2.
  ## Therefore  the proper size constant is  sk^2
  cSize2=sk^2;
  matrixSigma=sigmakNor*cSize2
  #covAux1= sigmakNor*cSize
  #list(cov=matrixSigma,covAux1=covAux1,mu=muk,s=sk)
  list(cov=matrixSigma,mu=muk,s=sk)
}


