########################################################################################################################################################
##' @title Index for the MNP scan procedure
##'
##' @description This function returns the index we want to maximize on the set of potential clusters, for each potential cluster
##'
##' @param rank_data numeric matrix. Matrix of the ranks of the initial data, the rows correspond to the sites (or the individuals) and each column represents a variable.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. It is the result of the "clusters" function.
##'
##'
##' @return numeric vector.
##'
##'
multi_WMW <- function(rank_data, matrix_clusters){

  in_cluster <- lapply(1:ncol(matrix_clusters), function(j) which(matrix_clusters[,j]==1))
  nb_var <- ncol(rank_data)
  nb_sites <- nrow(rank_data)
  coeff <- sum(rowSums(rank_data^2))
  coeff <- nb_var * nrow(rank_data) / coeff

  mean_inside <- t(sapply(1:ncol(matrix_clusters), function(i) colMeans(rank_data[in_cluster[[i]],,drop = FALSE])))
  mean_outside <- t(sapply(1:ncol(matrix_clusters), function(i) colMeans(rank_data[-in_cluster[[i]],,drop = FALSE])))

  norm2_inside <- rowSums(mean_inside^2)
  norm2_outside <- rowSums(mean_outside^2)

  card_w <- sapply(1:ncol(matrix_clusters), function(i) length(in_cluster[[i]]))

  stat <- coeff * (card_w * norm2_inside + (nb_sites - card_w) * norm2_outside)

  return(stat)
}

