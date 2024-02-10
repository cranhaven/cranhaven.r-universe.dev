#' @title Find best sub-matrix B in stratifiedcube
#' @name findB
#' @description
#' This function is computing a sub-matrix used in \code{\link{stratifiedcube}}.
#' 
#' @param X A matrix of size (\eqn{N} x \eqn{p}) of auxiliary variables on which the sample must be balanced.
#' @param strata A vector of integers that specifies the stratification.
#'
#' @details
#'
#' The function finds the smallest matrix B such that it contains only one more row than the number of columns.
#' It consecutively adds the right number of rows depending on the number of categories that is added.
#'
#' @return A list of two components. The sub-matrix of \code{X} and the corresponding disjunctive matrix.
#'  If we use the function \code{cbind} to combine the two matrices, the resulting matrix has only one more row than the number of columns. 
#'
#' @export
#'
#' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
#' @examples
#' N <- 1000
#' strata <-  sample(x = 1:6, size = N, replace = TRUE)
#'
#' p <- 3
#' X <- matrix(rnorm(N*p),ncol = 3)
#' findB(X,strata)
#'
findB <- function(X,
                  strata){

  strata <- as.matrix(strata)
  X <- as.matrix(X)
  eps <- 1e-9
  N <- nrow(X)
  pInit <- ncol(X)

  if(pInit > nrow(strata)){
    # print(pInit)
    # print(nrow(strata))
    # strata_tmp <- as.matrix(strata[1:(pInit+1),]) # error if the number of column is greater than the number of row we take all row.
    strata_tmp <- as.matrix(strata[1:nrow(strata),])
  }else{
    strata_tmp <- as.matrix(strata[1:pInit,])
  }
  
  
  nstrata <- sum(ncat(strata_tmp))
  nstrata_tmp <- 0

  while(nstrata != nstrata_tmp){
    nstrata_tmp = nstrata
    p =  pInit  + nstrata
    if(p >= nrow(X)){
      p <- nrow(X)-1
      strata_tmp <- as.matrix(strata[1:(p+1),])
      nstrata <- sum(ncat(strata_tmp))
      break;
    }
    strata_tmp <- as.matrix(strata[1:(p+1),])
    nstrata <- sum(ncat(strata_tmp))
  }

  strata_tmp <- as.matrix(strata_tmp)
  disj_strata <- disjMatrix(strata_tmp)

  return(list(X = as.matrix(X[1:(p+1),]),Xcat = disj_strata))

}
