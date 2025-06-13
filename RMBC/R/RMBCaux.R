#' RMBCaux
#'
#' Robust Model Base Clustering algorithm based on centers, a robust and
#' efficient version of EM algorithm.
#' @param Y A matrix of size n x p.
#' @param K The number of clusters.
#' @param thetaOld.alpha The initial alpha: An array of K positive real numbers
#' they must verify the condition sum(thetaOld.mu)== 1.
#' @param thetaOld.mu The initial centers: A list with K elements, each of them 
#' is an array of length p.
#' @param thetaOld.sigma The initial stcatter matrix: A list with K matrix, each of them 
#' has dimension p x p 
#' @param max_iter a maximum number of iterations used for the
#' algorithm stopping rule
#' @param niterFixedPoint the maximum number of iteration in the internal loop which 
#' computes sigma an mu separately. The default value is niterFixedPoint=1
#' @param tolerance tolerance parameter used for the algorithm stopping
#'     rule
#' @param cutoff optional argument for outliers detection - quantiles of chi-square 
#' to be used as a threshold for outliers detection, defaults to 0.999
#' @return A list including the estimated K centers and labels for the
#'     observations
#'
#' \itemize{
#' \item{\code{centers}}{: matrix of size K
#'     x p, with the estimated K centers.}
#' \item{\code{cluster}}{:
#'     array of size n x 1 integers labels between 1 and K.}
#' \item{\code{tauPath}}{: sequence of tau scale values at each
#'     iterations.}
#' \item{\code{Wni}}{: numeric array of size n x 1
#'     indicating the weights associated to each observation.}
#' \item{\code{emptyClusterFlag}}{: a boolean value. True means
#'     that in some iteration there were clusters totally empty}
#' \item{\code{niter}}{: number of iterations until convergence
#'     is achived or maximum number of iteration is reached}
#' \item{\code{di}}{distance of each observation to its assigned
#'     cluster-center} 
#'     }
#' @export
RMBCaux=function(Y,K, thetaOld.alpha,thetaOld.mu,
                 thetaOld.sigma,max_iter,
                 niterFixedPoint,tolerance,cutoff=1-1e-3){
  #######################################################
  ############### start the algorithm      #############
  ###################################################
  n=nrow(Y);
  valLLK=rep(0,max_iter)
  errkl=2*tolerance;
  iter=1
  
  thetaNew.alpha=0*thetaOld.alpha
  thetaNew.mu=lapply(thetaOld.mu,FUN=function(x){0*x})
  thetaNew.sigma=lapply(thetaOld.sigma,FUN=function(x){0*x})
  
  outliersConGr=which(!is_in_gr(Y, cutoff=cutoff,thetaOld.mu,thetaOld.sigma));
  while ((errkl>tolerance) & (iter<max_iter)){
    OutliersNew=outliersConGr
    
    ############################################################################
    ############################################################################
    ### This block can be used to control that loglikelihood without outliers 
    ### should be decreasing 
    ## if(length(OutliersNew)>0){
    ##   valLLK[iter] = loglk(Y[-OutliersNew,], thetaOld.alpha,
    ##   thetaOld.mu,thetaOld.sigma)
    ## }
    ## if(length(OutliersNew)==0){
    ##   valLLK[iter] = loglk(Y, thetaOld.alpha,thetaOld.mu,thetaOld.sigma)
    ## }
    ############################################################################
    ############################################################################
    
    ### E-step the weights membership estimation
    # Fji has the density values of evaluated in the point y_i
    # at the parameters j=1:k#
    Fji=matrix(0,nrow=K,ncol=n)
    logFji=matrix(0,nrow=K,ncol=n);
    alphaji=matrix(0,nrow=K,ncol=n)
    w=matrix(0,nrow=K,ncol=n);
    for (j in 1:K){
      Fji[j,]=mvtnorm::dmvnorm(x=Y,mean=thetaOld.mu[[j]],sigma=as.matrix(thetaOld.sigma[[j]]));
      logFji[j,]=mvtnorm::dmvnorm(x=Y,mean=thetaOld.mu[[j]],sigma=as.matrix(thetaOld.sigma[[j]]),log=TRUE);
    }
    
    for (i in 1:n){
      for (j in 1:K){
        alphaji[j,i]=Fji[j,i]*thetaOld.alpha[j]/sum(Fji[,i]*thetaOld.alpha)
      }
      # # The next step fixes the situation where a Fji is numerically zero
      # an the denominator also is numerically zero, this happens when an outlier is very
      # far from all the densities and the tail of the normal distribution return zero.
      # To avoid that, we use a trick by using logarithms
      if (sum(Fji[,i])<1e-16){
        KKK=min(abs(logFji[,i]))
        FjiNEW=exp(logFji[,i] + KKK);
        alphaji[,i]=FjiNEW*thetaOld.alpha/sum(FjiNEW*thetaOld.alpha)
      }
    }
    
    ## For each variable we tidy up the  weights of each observation.
    for (j in 1:K){
      for (i in 1:n){
        w[j,i]=alphaji[j,i]/sum(alphaji[j,])
      }
    }
    
    # The alpha - estimator is the same in the robust and classic case
    for (j in 1:K){
      thetaNew.alpha[j]=mean(alphaji[j,])
    }
    
    
    ##### ROBUST M-step  (We solve the fixed-point equation.)
    for (j in 1:K){
      muIni=thetaOld.mu[[j]]
      sigmaIni= thetaOld.sigma[[j]]
      
      salW=weightedSestimator(Y,muIni,sigmaIni,max_iterFP=niterFixedPoint,
                              weights=alphaji[j,], fixed_alpha=thetaNew.alpha[j])
      
      thetaNew.mu[[j]]=salW$mu    
      thetaNew.sigma[[j]]=salW$cov
    }

    relativeAlphaDiff=abs(thetaOld.alpha/thetaNew.alpha - 1)
    maxErr=max(relativeAlphaDiff)
    if(maxErr<(tolerance)){
      errkl=sumkl(thetaNew.mu,thetaNew.sigma,thetaOld.mu,thetaOld.sigma)
    }
    # The old stopping rule: we follow the GSE algorithm.
    #diferencias= c(unlist(thetaOld.alpha)- unlist(thetaNew.alpha),
    #               unlist(thetaOld.mu)- unlist(thetaNew.mu),
    #               unlist(thetaOld.sigma)- unlist(thetaNew.sigma))
    #maxDiferencia= max(abs(diferencias))
    thetaOld.alpha=thetaNew.alpha;
    thetaOld.mu=thetaNew.mu
    thetaOld.sigma= thetaNew.sigma
    iter=iter+1
    #points(matrix(unlist(thetaNew.mu),nrow=K,byrow=T), pch=19, col =3)
    outliersConGr=which(!is_in_gr(Y, cutoff=cutoff,theta.mu=thetaNew.mu,theta.sigma=thetaNew.sigma));
    nonoutliersConGr=which(is_in_gr(Y, cutoff=cutoff,theta.mu=thetaNew.mu,theta.sigma=thetaNew.sigma));
  }
  
  salqs=quad_disc(Y = Y,theta.alpha = thetaNew.alpha,
                 theta.mu = thetaNew.mu,theta.sigma = thetaNew.sigma)
  
  clusterRMBC=apply(salqs,1,function(x) which(x==max(x))[1])
  
  list(theta.mu=thetaNew.mu,theta.sigma=thetaNew.sigma, 
       theta.alpha= thetaNew.alpha,
       outliers=outliersConGr,nonoutliers=nonoutliersConGr, cluster=clusterRMBC,iter=iter-1)
}
