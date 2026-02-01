#' Learning linear non-Gaussian DAG via topological layers.
#'
#' @author Ruixuan Zhao <ruixuanzhao2-c@my.cityu.edu.hk>, Xin He, and Junhui Wang
#' @references Zhao, R., He X., and Wang J. (2022). Learning linear non-Gaussian directed acyclic graph with diverging number of nodes. Journal of Machine Learning Research.
#' @usage TLLiNGAM (X, hardth=0.3, criti.val=0.01, precision.refit = TRUE,
#'                  precision.method="glasso", B.refit=TRUE)
#'
#' @description Learning linear non-Gaussian DAG via topological layers.
#' @param X The n * p sample matrix, where n is the sample size and p is data dimension.
#' @param hardth The hard threshold of regression.
#' @param criti.val The critical value of independence test based on distance covariance.
#' @param precision.refit Whether to perform regression for re-fitting the coefficients in the precision matrix to improve estimation accuracy, after determining the non-zero elements of the precision matrix. The default is True.
#' @param precision.method Methods for Estimating Precision Matrix, which can be selected from "glasso" and "CLIME".
#' @param B.refit Whether to perform regression for re-fitting the coefficients in structural equation models to improve estimation accuracy, after determining the parent sets of all nodes. The default is True.
#'
#'
#' @return A result list including:
#' \describe{
#' \item{A}{The information of layer.}
#' \item{B}{The coefficients in structural equation models.}
#' }
#' @export
#'
#'
#'

TLLiNGAM = function(X, hardth=0.3, criti.val=0.01, precision.refit = TRUE,
                    precision.method="glasso", B.refit=TRUE){

  check=FALSE
  n=nrow(X)
  p=ncol(X)

  A=matrix(0,p,2) # store the information of layer
  A[,1]=1:p
  colnames(A)=c("Node","Layer")
  B=matrix(0,p,p)

  S=1:p
  # Identify A_0 layer
  stdPreciMat = Compute.precision(X, hardth, precision.refit, precision.method)
  dcor_value=sapply(1:p,function(k) Compute.dcor(k,stdPreciMat[,k],X))

  node_S_select=S[dcor_value>criti.val]
  if (length(node_S_select)==0){
    check=TRUE
    return(list(A=A,B=B,check=check))
  }
  S=S[-node_S_select]
  B[S,node_S_select]=stdPreciMat[S,node_S_select]
  t=1

  while(length(S)>1){
    len_S=length(S)
    X_wait=X[,S]

    stdPreciMat = Compute.precision(X_wait, hardth, precision.refit, precision.method)
    dcor_value=sapply(1:len_S, function(k) Compute.dcor(k,stdPreciMat[,k],X_wait))
    node_S_select=S[dcor_value>criti.val]
    index_node_S_select=which(dcor_value>criti.val)
    if (length(node_S_select)==0){
      node_S_select=S
    }
    A[node_S_select,2]=t
    S=setdiff(S,node_S_select)
    index_S=setdiff(c(1:len_S),index_node_S_select)
    B[S,node_S_select]=stdPreciMat[index_S,index_node_S_select]
    t=t+1
  }
  if (length(S)==1){
    A[S,2]=t
  }

  if(B.refit){
    B=Parent.set.refit(X,B)
  }
  return(list(A=A,B=B,check=check))
}

