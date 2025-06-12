#' Local entropy computation
#' 
#' This function allows you to compute the family of local alpha entropy as
#' stated in Martos et al (2018).
#' 
#' The function computes the alpha entropy and the local alpha entropy (see
#' reference for further details) of a data set using a non parametric density
#' estimator.
#' 
#' @param X is the input matrix of n x d. n are the observations (e.g.:
#' projected curves) and d is the dimension of X (dimension of the projection
#' space).
#' @param alpha a parameter defining the entropy function. Default = 2.
#' @param K number of neighbour points to consider in the computation of
#' entropy.
#' @param scale logical variable indicating if scaling is required. Default =
#' FALSE.
#' @return \item{local.entropy}{local entropy relative to each point in the
#' sample.} \item{entropy}{estimated entropy.}
#' @author N. Hernández
#' @noRd
#' @references Martos, G. et al (2018): Entropy Measures for Stochastic
#' Processes with Applications in Functional Anomaly Detection. Entropy 20(1):
#' 33 (2018).
entropy <-
function(X,alpha=2,K,scale=F)  {
  # Parameters: alhpa and k (parametro de delta-localidad)
  X=as.matrix(X)
  if(scale==T){X=scale(X)}else{X=X}
  n  =  dim(X)[1]
  Knn.distances = FNN::knn.dist(X, k=K)
  v_k = as.matrix(rowMeans(Knn.distances),ncol=1)
  l.entropy=log(1+v_k^alpha)/abs(1-alpha)
  entropy=log(sum(1+v_k^alpha))/abs(1-alpha)
  return(list(local.etropy=l.entropy,
              entropy=entropy))
}
