########################################################################################################################################################
##' @title Index for the DFFSS scan procedure
##'
##' @description This function returns the index we want to maximize on the set of potential clusters, for each potential cluster
##'
##' @param data numeric matrix. Matrix of the data. The rows correspond to the sites (or the individuals) and each column represents an observation time.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. It is the result of the "clusters" function.
##'
##' @return numeric vector.
##'
##'
pointwise_dfree <- function(data, matrix_clusters)
{
  nb_sites <- nrow(data)
  nb_times <- ncol(data)

  nb_clusters <- ncol(matrix_clusters)
  matrix_clusters1 <- matrix(as.logical(matrix_clusters),nb_sites)
  n.k <- colSums(matrix_clusters1)

  stat <- numeric(nb_clusters)

  for(cl in 1:nb_clusters){
    if(n.k[cl] == 1){
      var_out <- colVars(data[matrix_clusters1[,cl]==0,,drop = FALSE])
      sp2 <- (nb_sites - n.k[cl] - 1) * var_out / (nb_sites-2)
    }else{
      var_out <- colVars(data[matrix_clusters1[,cl]==0,, drop = FALSE])
      var_in <- colVars(data[matrix_clusters1[,cl]==1,, drop = FALSE])
      sp2 <- ( (n.k[cl] - 1) * var_in + (nb_sites - n.k[cl] - 1) * var_out ) / (nb_sites-2)
    }

    denominator <- sqrt(sp2 * (1/n.k[cl] + 1/(nb_sites - n.k[cl]) ))

    mean_inside <- colMeans(data[matrix_clusters1[,cl]==1,, drop = FALSE])
    mean_outside <- colMeans(data[matrix_clusters1[,cl]==0,, drop = FALSE])

    stat[cl] <- max(abs(mean_inside - mean_outside)/(denominator))
  }


  return(stat)
}
