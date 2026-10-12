#' @title To explain how to create a new cluster.
#' @description To explain how to create the cluster formed by the two clusters given. Add the new cluster to \code{list}.
#' @param list is the generic cluster list.
#' @param clusters is a vector with the matrix index clusters.
#' @details This function is part of the hierarchical clusterization method.
#' @details 1 - The function maps \code{clusters} in \code{list}.
#' @details 2 - It creates a new cluster from them.
#' @details 3 - It adds the new cluster to \code{list}.
#' @details 4 - It disables the clusters used in the second step.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return A list with clusters. Explanation.
#' @examples
#'
#' data <- c(1:10)
#'
#' list <- toList(data)
#'
#' clusters <- c(1,2)
#'
#' newCluster.details(list,clusters)
#'
#' @export

newCluster.details <-
  function(list,clusters){
    message("\n 'newCluster' function creates a new cluster from the clusters given. \n")
    message("\n It adds the new cluster to 'list' and disables the clusters used to create the new one. \n")
    cluster1 <- list[[clusters[1]]]
    cluster2 <- list[[clusters[2]]]
    message("\n Using ", clusters[1], " and ", clusters[2], " it searches the clusters in 'list' parameter and creates a new one with \n their components.\n")
    newCluster <- c()
    for (i in (1:nrow(cluster1))){
      newCluster <- c(newCluster,cluster1[i,])
    }
    for (i in (1:nrow(cluster2))){
      newCluster <- c(newCluster,cluster2[i,])
    }
    message("\n After the new cluster is created, the initial clusters must be disabled.\n")
    newCluster1 <- c()
    newCluster2 <- c()
    for (i in (1:nrow(cluster1))){
      cluster <- cluster1[i,]
      cluster[3] <- FALSE
      newCluster1 <- c(newCluster1,cluster)
    }
    for (i in (1:nrow(cluster2))){
      cluster <- cluster2[i,]
      cluster[3] <- FALSE
      newCluster2 <- c(newCluster2,cluster)
    }
    cluster1 <-matrix(newCluster1, ncol=3, byrow=TRUE)
    cluster2 <-matrix(newCluster2, ncol=3, byrow=TRUE)
    list[[clusters[1]]] <- cluster1
    list[[clusters[2]]] <- cluster2
    message("\n The new cluster:\n\n")
    newCluster <- matrix(newCluster, ncol=3, byrow=TRUE)
    print(newCluster)
    message("\n\n is added to the list: \n\n")
    list[[length(list) + 1]] <- newCluster
    #print(list)
    list
  }
