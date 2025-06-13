  #'
  #' Robust Model Base Clustering  a robust and efficient version of EM algorithm.
  #' @param Y A matrix of size n x p.
  #' @param K The number of clusters.
  #' @param max_iter a maximum number of iterations used for the
  #' algorithm stopping rule
  #' @param tolerance tolerance parameter used for the algorithm stopping
  #'     rule
  #' @return A list including the estimated mixture distribution parameters and cluster-label for the
  #' observations  
  #' \itemize{
  #' \item{\code{alpha}}{: K numeric values representing the convex combination coefficients.}
  #' \item{\code{mu}}{: a list of length K with the location initial estimators.}
  #' \item{\code{sigma}}{: a list of length K with the location scatter matrix estimators.}
  #' \item{\code{nonoutliers}}{: an array of indices that contains the estimated nonoutliers observations}
  #' \item{\code{outliers}}{: an array of indices that contains the estimated outliers observations}
  #' \item{\code{cluster}}{: A vector of integers (from 0:k) indicating the cluster to which each point is allocated.
  #'  0 label corresponds to outliers}
  #' \item{\code{cluster_without_outliers}}{: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
  #'  Note: Outliers are assigned to the cluster that maximizes 
  #'  the belonging probability. To recognize the outliers the field 
  #'  outliers should be used}
  #' }
  #' @examples 
  #' # Generate Sintetic data (three normal cluster in two dimension)
  #' # clusters have different shapes and orentation.
  #' # The data is contaminated uniformly (level 20%).
  #' ################################################
  #' #### Start data generating process ############
  #' ##############################################
  #' 
  #' # generates base clusters
  #' 
  #' Z1 <- c(rnorm(50,0),rnorm(50,0),rnorm(50,0))
  #' Z2 <- rnorm(150);
  #' X <-  matrix(0, ncol=2,nrow=150);
  #' X[,1]=Z1;X[,2]=Z2
  #' true.cluster= c(rep(1,50),rep(2,50),rep(3,50))
  #' # rotate, expand and translate base clusters
  #' theta=pi/3;
  #' aux1=matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),nrow=2)
  #' 
  #' aux2=sqrt(4)*diag(c(1,1/4))
  #' 
  #' B=aux1%*%aux2%*%t(aux1)
  #' 
  #' X[true.cluster==3,]=X[true.cluster==3,]%*%aux2%*%aux1 + 
  #' matrix(c(15,2), byrow = TRUE,nrow=50,ncol=2)
  #' X[true.cluster==2,2] = X[true.cluster==2,2]*4
  #' X[true.cluster==1,2] = X[true.cluster==1,2]*0.1
  #' X[true.cluster==1, ] = X[true.cluster==1,]+ 
  #' matrix(c(-15,-1),byrow = TRUE,nrow=50,ncol=2)
  #' 
  #' ### Generate 30 sintetic outliers (contamination level 20%)
  #' 
  #' outliers=sample(1:150,30)
  #' X[outliers, ] <- matrix(runif( 60, 2 * min(X), 2 * max(X) ),
  #'                         ncol = 2, nrow = 30)
  #' 
  #' ###############################################
  #' #### END data generating process ############
  #' #############################################
  #' 
  #' ### APLYING RMBC ALGORITHM 
  #' 
  #' ret = RMBC(Y=X, K=3,max_iter = 82)
  #' 
  #' cluster = ret$cluster
  #' #############################################
  #' ### plotting results ########################
  #' #############################################
  #' oldpar=par(mfrow=c(1,2))
  #' plot(X,  main="actual clusters" )
  #' for (j in 1:3){
  #'   points(X[true.cluster==j,],pch=19, col=j+1)
  #' }
  #' points(X[outliers,],pch=19,col=1)
  #' 
  #' plot(X,main="clusters estimation")
  #' for (j in 1:3){
  #'   points(X[cluster==j,],pch=19, col=j+1)
  #' }
  #' points(X[ret$outliers,],pch=19,col=1)
  #' par(oldpar)
  #'
  #' @export
  
  RMBC=function(Y,K,max_iter=80, tolerance=1e-4){
  
    niterFixedPoint=1;
    result=robustINIT(Y = Y,K = K)
    resultRMBCaux=RMBCaux(Y = Y,K = K,
                          thetaOld.alpha = result$alphaINIT,
                          thetaOld.mu=result$muINIT,
                          thetaOld.sigma = result$sigmaINIT,
                          max_iter,niterFixedPoint,tolerance)
    
    Sigma=resultRMBCaux$theta.sigma;
    mu=resultRMBCaux$theta.mu;
    alpha=resultRMBCaux$theta.alpha
    outliers=resultRMBCaux$outliers
    nonoutliers=resultRMBCaux$nonoutliers
    iter=resultRMBCaux$iter
    cluster_without_outliers=resultRMBCaux$cluster
    cluster=resultRMBCaux$cluster
    cluster[outliers]=0
    list(mu=mu,Sigma=Sigma,alpha=alpha, outliers=outliers,
         nonoutliers=nonoutliers, 
         cluster_without_outliers=cluster_without_outliers,
         cluster=cluster,
         iter=iter)
  }
  