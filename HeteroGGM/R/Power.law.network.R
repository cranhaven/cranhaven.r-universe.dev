#' Power law network
#'
#' @description Generating three s-block power-law precision matrices
#' @param p The dimensions of the precision matrix.
#' @param s The number of sub-networks.
#' @param umin The lower bound of non-zero elements on non-diagonal elements.
#' @param umax The upper bound of non-zero elements on non-diagonal elements.
#' @param I2 The replacement blocks for the precision matrix of the second subgroup.
#' @param I3 The replacement blocks for the precision matrix of the third subgroup.
#'
#' @return A list including The precision matrices of three subgroups.
#' @export
#'
#' @importFrom igraph ba.game get.edgelist
#' @importFrom Matrix bdiag
#' @importFrom stats runif
#'
#' @examples
#' p <- 20               # The dimension of the precision matrix
#' ################ The true parameters ################
#' # Power law network
#' set.seed(2)
#' A.list <- Power.law.network(p,s=5,I2=c(1),I3=c(2))
#' Theta01 <- A.list$A1
#' Theta02 <- A.list$A2
#' Theta03 <- A.list$A3
#' sigma01 <- solve(Theta01)
#' sigma02 <- solve(Theta02)
#' sigma03 <- solve(Theta03)
#' Sigma0.list <- list(sigma01,sigma02,sigma03)
#' Theta0.list <- list(Theta01,Theta02,Theta03)
#'
Power.law.network = function(p,s=10,umin=0.1,umax=0.4,I2=0,I3=0){

  ## ---------------------------------------------------------------------------------------------------------------
  ## The name of the function: Power.law.network
  ## ---------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Generating the s-block power-law precision matrices.
  ## ---------------------------------------------------------------------------------------------------------------
  ## Required preceding data: No
  ## ---------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ p: The dimensions of the precision matrix.
  ## @ s: The number of sub-networks.
  ## @ umin: The lower bound of non-zero elements on non-diagonal elements.
  ## @ umax: The upper bound of non-zero elements on non-diagonal elements.
  ## @ I2: The replacement blocks for the precision matrix of the second subgroup.
  ## @ I3: The replacement blocks for the precision matrix of the third subgroup.
  ## ---------------------------------------------------------------------------------------------------------------
  ## Output:
  ## @ A list including The precision matrices of three subgroups.
  ## ---------------------------------------------------------------------------------------------------------------

  pp=p/s
  if(p%%s != 0){
    print("warning! Matrix dimensions cannot be rounded by sub-matrix dimensions.")
  }
  submatrix=list()
  for (ss in 1:s) {
    g = ba.game(pp, m=2,directed = F)
    Eg = as.data.frame(get.edgelist(g))
    subi = diag(1,pp)
    for (q in 1:dim(Eg)[1]) {
      i=Eg[q,1];j=Eg[q,2]
      ij = sample(c(runif(1,umin,umax),runif(1,-umax,-umin)))[1]
      subi[i,j]=ij;subi[j,i]=ij
    }
    for (i in 1:pp) {
      subi[i,i] = sum(abs(subi[i,setdiff(1:pp,i)]))+0.1
    }
    submatrix[[ss]]=subi
  }
  submatrix2=submatrix
  submatrix3=submatrix
  if(length(I2)>1 | I2[1]!=0){
    for (ii in 1:length(I2)) {
      iii=I2[ii]
      g = ba.game(pp, m=2,directed = F)
      Eg = as.data.frame(get.edgelist(g))
      subi = diag(1,pp)
      for (q in 1:dim(Eg)[1]) {
        i=Eg[q,1];j=Eg[q,2]
        ij = sample(c(runif(1,umin,umax),runif(1,-umax,-umin)))[1]
        subi[i,j]=ij;subi[j,i]=ij
      }
      for (i in 1:pp) {
        subi[i,i] = sum(abs(subi[i,setdiff(1:pp,i)]))+0.1
      }
      submatrix2[[iii]] = subi
    }
  }
  if(length(I3)>1 | I3[1]!=0){
    for (ii in 1:length(I3)) {
      iii=I3[ii]
      g = ba.game(pp, m=2,directed = F)
      Eg = as.data.frame(get.edgelist(g))
      subi = diag(1,pp)
      for (q in 1:dim(Eg)[1]) {
        i=Eg[q,1];j=Eg[q,2]
        ij = sample(c(runif(1,umin,umax),runif(1,-umax,-umin)))[1]
        subi[i,j]=ij;subi[j,i]=ij
      }
      for (i in 1:pp) {
        subi[i,i] = sum(abs(subi[i,setdiff(1:pp,i)]))+0.1
      }
      submatrix3[[iii]] = subi
    }
  }
  A=submatrix[[1]]
  for (ss in 2:s) {
    A=bdiag(A,submatrix[[ss]])
  }
  A = as.matrix(A)
  A2=submatrix2[[1]]
  for (ss in 2:s) {
    A2=bdiag(A2,submatrix2[[ss]])
  }
  A2 = as.matrix(A2)
  A3=submatrix3[[1]]
  for (ss in 2:s) {
    A3=bdiag(A3,submatrix3[[ss]])
  }
  A3 = as.matrix(A3)
  return(list(A1=A,A2=A2,A3=A3))
}
