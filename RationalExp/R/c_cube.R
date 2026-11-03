
#' Instrumental functions computations
#'
#' This function defines, for each specified value of r_n the set of indicator funtions h(X_i) which are the key elements for the RE test with co
#' covariates
#' @param X_adj the standardised version of the covariates X
#' @param N the size of X
#' @param DX the number of covariates
#' @param r_n the parameter indexing the number of instrumental function, which is chosen according the the rule used in AS y default.
#'
#' @return  a list containing, in order:
#'
#'   - X_adj he standardised version of the covariates X
#'
#'   - r_n the parameter indexing the number of instrumental function, which is chosen according the the rule used in AS y default.
#'
#'   - g_col a vector containing part of the weights
#'
#'   - Q_AR  a matrix with the weights that enter the statistic T
#'
#'   - G_X a binary matrix indexing the observations X that fall into the hypercubes indexed by h.
#' @export
#'
c_cube <- function(X_adj,N,DX,r_n){

  res <- vector("list")
  g_col = matrix(0,r_n,1)
  for (i in 1:r_n) {
    g_col[i,1] = (2*i)^(DX)
  }

  G_X = matrix(0,N,sum(g_col))
  Q_AR =  matrix(0,1,sum(g_col))
  ##
  for (r in 1:r_n){
    X_index_dim = ceiling(X_adj*2*r)
    X_index = matrix(1,N,1)
    for(d in 1:DX) {
      X_index_temp = X_index_dim[,DX-d+1] -1
      X_index_temp[X_index_temp==-1] <- 0
      X_index = X_index + ((2*r)^(d-1))*X_index_temp
    }
    for(g_index in 1:((2*r)^(DX))){
      G_X[,sum(g_col[1:r])-g_col[r]+g_index] = 1*(X_index==g_index)
    }
    Q_AR[,(sum(g_col[1:r])-g_col[r]+1):sum(g_col[1:r])] = 1/g_col[r]/(r^2+100)*matrix(1,1,g_col[r])
  }
  # length(sum(g_col[1:r])-g_col[r]+1:sum(g_col[1:r]))
  Q_AR = Q_AR / sum(Q_AR) #// Adjust the weight function
  res[[1]] <-  X_adj
  res[[2]] <- r_n
  res[[3]] <- g_col
  res[[4]] <- Q_AR
  res[[5]] <-G_X

  return(res)
}
