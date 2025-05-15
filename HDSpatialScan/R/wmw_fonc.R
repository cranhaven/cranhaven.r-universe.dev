########################################################################################################################################################
##' @title Computation of the matrix of signs
##'
##' @description This function returns the matrix of signs of the data.
##'
##' @param data numeric matrix. Matrix of the data, the rows correspond to the sites (or the individuals) and each column represents an observation time.
##'
##'
##' @return numeric matrix.
##'
##'
uni_signs_matrix <- function(data){
  nb_sites <- nrow(data)
  nb_times <- ncol(data)
  signs <- matrix(ncol = nb_times, nrow = nb_sites)
  for(i in 1:nb_sites){
    temp <- -t(data[i,]-t(data))
    norm_temp <- sqrt(rowSums(temp^2))
    signs[i,] <- colSums(temp / norm_temp, na.rm = TRUE)
  }
  return(signs)
}

######################################################################################################################################################################
##' @title List of matrix of signs (multivariate functional data)
##'
##' @description This function returns the list of matrix of signs for the multivariate functional data
##'
##' @param data list of numeric matrices. List of nb_sites (or nb_individuals) matrices of the data, the rows correspond to the variables and each column represents an observation time.
##'
##' @return list of numeric matrices.
##'
##'
multi_signs_matrix <- function(data){
  nb_sites <- length(data)
  nb_var <- nrow(data[[1]])
  nb_times <- ncol(data[[1]])
  signs <- list()
  for(i in 1:nb_sites){
    signs[[i]] <- matrix(0, ncol = nb_times, nrow = nb_var)
    list_diff <- list()
    for(j in 1:nb_sites){
      list_diff[[j]] <- data[[j]] - data[[i]]
    }
    norms <- sqrt(sapply(1:nb_sites, function(j) sum(list_diff[[j]]^2))/nb_times)
    for(j in 1:nb_sites){
      if(j != i){
        signs[[i]] <- signs[[i]] + list_diff[[j]]/norms[j]
      }
    }
  }
  return(signs)
}


########################################################################################################################################################
##' @title Index for the NPFSS scan procedure (univariate functional case)
##'
##' @description This function returns the index we want to maximize on the set of potential clusters, for each potential cluster
##'
##' @param signs numeric matrix. Matrix of signs of the data, the rows correspond to the sites (or the individuals) and each column represents an observation time.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. It is the result of the "clusters" function.
##'
##'
##' @return numeric vector.
##'
##'
uni_fWMW <- function(signs, matrix_clusters){
  nb_sites <- nrow(signs)
  nb_times <- ncol(signs)
  in_cluster <- lapply(1:ncol(matrix_clusters), function(j) which(matrix_clusters[,j]==1))
  stats <- numeric(ncol(matrix_clusters))
  denominator <- sapply(1:ncol(matrix_clusters), function(c) sqrt(length(in_cluster[[c]]) * (nb_sites-length(in_cluster[[c]]))*(nb_sites+1)))

  for(c in 1:ncol(matrix_clusters)){
    stats[c] <- sqrt(sum(colSums(signs[in_cluster[[c]],,drop = FALSE])^2))/denominator[[c]]
  }
  return(stats)
}

#################################################################################################################################################################
##' @title Index for the NPFSS scan procedure (multivariate functional case)
##'
##' @description This function returns the index we want to maximize on the set of potential clusters, for each potential cluster
##'
##' @param signs list of numeric matrices. List of nb_sites (or nb_individuals) sign matrices, the rows correspond to the variables and each column represents an observation time.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. It is the result of the "clusters" function.
##'
##'
##' @return numeric vector.
##'
##'
multi_fWMW <- function(signs, matrix_clusters){
  nb_sites <- length(signs)
  nb_var <- nrow(signs[[1]])
  nb_times <- ncol(signs[[1]])
  in_cluster <- lapply(1:ncol(matrix_clusters), function(j) which(matrix_clusters[,j]==1))
  stats <- numeric(ncol(matrix_clusters))
  denominator <- sapply(1:ncol(matrix_clusters), function(c) sqrt(length(in_cluster[[c]]) * (nb_sites-length(in_cluster[[c]]))*(nb_sites+1)))

  for(c in 1:ncol(matrix_clusters)){
    A <- matrix(0, ncol = nb_times, nrow = nb_var)
    for(i in in_cluster[[c]]){
      A <- A + signs[[i]]
    }
    stats[c] <- sqrt(sum(A^2)/nb_times)/denominator[[c]]
  }
  return(stats)
}
