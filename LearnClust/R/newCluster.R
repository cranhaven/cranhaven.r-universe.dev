#' @title To create a new cluster.
#' @description To create the cluster formed by the two clusters given. Add the new cluster to \code{list}.
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
#' @return A list with clusters.
#' @examples
#'
#' data <- c(1:10)
#'
#' list <- toList(data)
#'
#' clusters <- c(1,2)
#'
#' newCluster(list,clusters)
#'
#' @export

newCluster <-
function(list,clusters){
    cluster1 <- list[[clusters[1]]]
    cluster2 <- list[[clusters[2]]]
    newCluster <- c()
    for (i in (1:nrow(cluster1))){
        newCluster <- c(newCluster,cluster1[i,])
    }
    for (i in (1:nrow(cluster2))){
        newCluster <- c(newCluster,cluster2[i,])
    }
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
    newCluster <- matrix(newCluster, ncol=3, byrow=TRUE)
    list[[length(list) + 1]] <- newCluster
    list
}


