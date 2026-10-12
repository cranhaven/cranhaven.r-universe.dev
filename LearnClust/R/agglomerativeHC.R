#' @title To execute agglomerative hierarchical clusterization algorithm by distance and approach.
#' @description To execute complete agglomerative hierarchical clusterization algorithm choosing distance and approach type.
#' @param data could be a numeric vector, a matrix or a numeric data frame. It will be transformed into matrix and list to be used.
#' @param distance is a string. It chooses the distance to use.
#' @param approach is a string. It chooses the approach to use.
#' @details This function is the main part of the agglomerative hierarchical clusterization method.
#' It executes the theoretical algorithm step by step.
#' @details 1 - The function transforms data in useful object to be used.
#' @details 2 - It creates the clusters.
#' @details 3 - It calculates a matrix distances with the clusters created applying distance and approach given.
#' @details 4 - It chooses the distance value and gets the clusters.
#' @details 5 - It groups the clusters in a new one and updates clusters list.
#' @details 6 - It repeats these steps until an unique cluster exists.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return R object with a dendrogram, the grouped clusters and the list with every cluster.
#' @examples
#'
#' a <- c(1,2,1,3,1,4,1,5,1,6)
#'
#' matrixA <- matrix(a,ncol=2)
#'
#' dataFrameA <- data.frame(matrixA)
#'
#' agglomerativeHC(a,'EUC','MAX')
#'
#' agglomerativeHC(matrixA,'MAN','AVG')
#'
#' agglomerativeHC(dataFrameA,'CAN','MIN')
#'
#' @export


agglomerativeHC <-
  function(data,distance,approach){
    if(!is.data.frame(data)){
      mtx <- matrix(data, ncol=2, byrow=TRUE)
      plot(mtx, main = 'Initial Data Visualization',xlab = 'First property', ylab = 'Second property')
    }
    list <- toList(data)
    lengthList <- length(list)
    #matrix <- matrix(data,ncol=2,byrow=TRUE)
    goal <- list[[length(list)]]
    res <- list
    clusters <- data.frame()
    labels <- c()
    clustersDendrogram <- data.frame()
    oneCluster <- (nrow(goal) == lengthList)
    while (!oneCluster){
      matrixDistance <- mdAgglomerative(list,distance,approach)
      minDistance <- minDistance(matrixDistance)
      groupedClusters <- getCluster(minDistance, matrixDistance)
      updatedClusters <- newCluster(list, groupedClusters)
      groupedClustersDendrogram <- checkClusters(groupedClusters,lengthList)
      clustersDendrogram <- rbind(clustersDendrogram, data.frame(matrix(groupedClustersDendrogram,ncol=2)))
      clusters <- rbind(clusters, data.frame(matrix(groupedClusters,ncol=2)))
      res[[length(res) + 1]] <- updatedClusters[[length(updatedClusters)]]
      hierarchicCluster <- updatedClusters
      cleanCluster <- usefulClusters(updatedClusters)
      list <- cleanCluster
      goal <- updatedClusters[[length(updatedClusters)]]
      oneCluster <- (nrow(goal) == lengthList)
    }

    names(clusters) <-c("cluster1", "cluster2")

    res <- cleanClusters(res)
    for (index in c(1:length(res))) {
      cluster <- res[[index]]
      res[[index]] <- data.frame(cluster)
    }

    dendrogram <- list()
    dendrogram$merge <- as.matrix(clustersDendrogram)
    dendrogram$height <- c(1:(lengthList-1))
    dendrogram$order <- c(1:lengthList)
    dendrogram$labels <- c(1:lengthList)
    class(dendrogram) <- "hclust"

    sol <- list()
    sol$dendrogram <- dendrogram
    sol$clusters <- res
    sol$groupedClusters <- clusters
    sol
  }

checkClusters <- function(clusters,lengthList) {
  max <- lengthList
  as.numeric(clusters[1])
  as.numeric(clusters[2])
  if(clusters[1] <= max){
    clusters[1] <- -(clusters[1])
  } else {
    clusters[1] <- clusters[1] - max
  }
  if(clusters[2] <= max){
    clusters[2] <- -(clusters[2])
  }else {
    clusters[2] <- clusters[2] - max
  }
  clusters
}


cleanClusters <- function(list){
  sol <- list()
  for (index in c(1:length(list))) {
    cluster <- list[[index]]
    cluster <- cluster[,1:2]
    sol[[length(sol) + 1]] <- matrix(cluster,ncol=2)
  }
  sol
}
