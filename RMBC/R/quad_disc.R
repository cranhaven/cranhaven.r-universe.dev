#' quad_disc
#'
#' Computes the quadratic discriminant of each mixture component, 
#' @param Y A matrix of size n x p.
#' @param theta.alpha The  alpha values: An array of K positive real numbers
#' they must verify the condition sum(thetaOld.mu)== 1.
#' @param theta.mu The estimated centers: A list with K elements, each of them 
#' is an array of length p.
#' @param theta.sigma The estimated scatter matrices: A list with K matrices, each of them 
#' has dimension p x p 
#' @return A n x K matrix, where each row has the values of the quadratic discriminant 
#' with regarding to the j-th mixture component, j = 1,...,K 
#' @export 
quad_disc=function(Y,theta.alpha, theta.mu,theta.sigma){
  #computes the values of quadratic discriminant in each case.  
  actualk <- length(theta.alpha);n=dim(Y)[1] 
  mahalanobisMatrix <- matrix(0,ncol=actualk,nrow=n)
  qsMatrix <- matrix(0,ncol=actualk,nrow=n)
  logdet <- rep(0,actualk); 
  logalpha=log(theta.alpha); 
  for (j in 1:length(theta.alpha)){
    mahalanobisMatrix[,j] <- mahalanobis(x=Y ,center= theta.mu[[j]],cov=MASS::ginv(theta.sigma[[j]]),inverted = TRUE)
    logdet[j] <- log(det(theta.sigma[[j]]))
  }
  
  for (j in 1:length(theta.alpha)){
    qsMatrix[,j] <- logalpha[j] - .5*logdet[j] -.5*mahalanobisMatrix[,j]
  }
  qsMatrix
}