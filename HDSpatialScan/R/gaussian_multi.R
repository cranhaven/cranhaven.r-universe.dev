########################################################################################################################################################
##' @title Index for the MG scan procedure
##'
##' @description This function returns the index we want to minimize on the set of potential clusters, for each potential cluster
##'
##' @param data numeric matrix. Matrix of the data, the rows correspond to the sites (or individuals) and each column represents a variable.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. It is the result of the "clusters" function.
##'
##'
##' @return numeric vector.
##'
##'
multi_gaussian <- function(data, matrix_clusters){

  in_cluster <- lapply(1:ncol(matrix_clusters), function(j) which(matrix_clusters[,j]==1))

  mean_inside <- t(sapply(1:ncol(matrix_clusters), function(j) colMeans(data[in_cluster[[j]],,drop = FALSE])))
  mean_outside <- t(sapply(1:ncol(matrix_clusters), function(j) colMeans(data[-in_cluster[[j]],, drop = FALSE])))

  sum_inside <- lapply(1:ncol(matrix_clusters), function(j) (apply(data[in_cluster[[j]], , drop = FALSE], 1, FUN = "-", mean_inside[j, ])%*%t(apply(data[in_cluster[[j]], , drop = FALSE], 1, FUN = "-", mean_inside[j, ]))))
  sum_outside <- lapply(1:ncol(matrix_clusters), function(j) (apply(data[-in_cluster[[j]], , drop = FALSE], 1, FUN = "-", mean_outside[j, ])%*%t(apply(data[-in_cluster[[j]], , drop = FALSE], 1, FUN = "-", mean_outside[j, ]))))

  stat <- sapply(1:ncol(matrix_clusters), function(j) det(sum_inside[[j]] + sum_outside[[j]]))

  return(stat)
}

