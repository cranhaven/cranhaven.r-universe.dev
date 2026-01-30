#########1#########2#########3#########4#########5#########6#########7#########8
#' Solve a System of Equations
#'
#' Solve a system of equations if it has a unique solution;
#' output an error message otherwise
#'
#' @param A matrix A in Ax=y
#' @param y output vector in Ax=y
#' @return the unique solution x to Ax=y
#' @examples
#' systemEq(matrix(c(1:3,2,4,4),3),c(3,6,7))
#' @export
################################################################################
systemEq<-function(A,y) {
  A=as.matrix(A)
  y=as.vector(y)
  if (nrow(A)!=length(y)) stop("Dimensions do not conform")
  if (nrow(A)<ncol(A)) stop("Many solutions")
  ech=matlib::echelon(A,y)
  zero=rep(0,ncol(ech))
  j=NULL
  #Remove zero rows
  for (i in 1:nrow(ech)) {
    if (all(ech[i,]==zero)) j=c(j,i)
  }
  if (!is.null(j)) ech=ech[-j,,drop=F]
  if (all(ech[nrow(ech),1:(ncol(ech)-1)]==zero[1:(length(zero)-1)])) stop("Inconsistent System: No solution")
  if (ncol(ech)>(nrow(ech)+1)) stop("Many solutions")
  ech[,ncol(ech)]
}
