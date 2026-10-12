#' @title To execute divisive hierarchical clusterization algorithm by distance and approach.
#' @description To execute complete divisive hierarchical clusterization algorithm by choosing distance and approach types.
#' @param data could be a numeric vector, a matrix or a numeric data frame. It will be transformed into matrix and list to be used.
#' @param distance is a string. It chooses the distance to use.
#' @param approach is a string. It chooses the approach to use.
#' @details This function is the main part of the divisive hierarchical clusterization method.
#' It executes the theoretical algorithm step by step.
#' @details 1 - The function transforms data in useful object to be used.
#' @details 2 - It creates a cluster that includes every simple elements.
#' @details 3 - It initializes posible clusters using the initial elements.
#' @details 4 - It calculates a matrix distance with the clusters created in the 3rd step.
#' @details 5 - It chooses the maximal distance value and gets the clusters to be divided.
#' @details 6 - It divides the cluster into two new complementary clusters and updates the clusters list.
#' @details 6 - It repeats these steps until every cluster can't be divided again. The solution includes every simple cluster.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return A list with the divided clusters.
#' @examples
#'
#' a <- c(1,2,1,3,1,4,1,5,1,6)
#'
#' matrixA <- matrix(a,ncol=2)
#'
#' dataFrameA <- data.frame(matrixA)
#'
#' divisiveHC(a,'EUC','MAX')
#'
#' divisiveHC(matrixA,'MAN','AVG')
#'
#' divisiveHC(dataFrameA,'CHE','MIN')
#'
#' @export

divisiveHC <-
  function(data,distance,approach){
	if(!is.data.frame(data)){
		mtx <- matrix(data, ncol=2)
		plot(mtx, main = 'Initial Data Visualization',xlab = 'First property', ylab = 'Second property')
	}
    list <- toListDivisive(data)
    lengthList <- length(list)
    clustersList <- initClusters(list)
    init <- clustersList[[length(clustersList)]]
    activeClusters <- list(init)
    clusters <- list(init)

    while(length(activeClusters) > 0){

      # cat('_____________________________________________________________________________________________')
      # cat('-------------- i  ', i,'\n')
      initializedClusters <- list()
      maxValues <- c()
      matrix <- list()
      posibleClusters <- list()

      for (i in c(1:length(activeClusters))) {
        list <- toListDivisive(activeClusters[[i]])
        initializedClusters[[length(initializedClusters) + 1]] <- list
        clustersList <- initClusters(list)
        posibleClusters[[length(posibleClusters) + 1]] <- clustersList
        matrixDistance <- mdDivisive(clustersList,distance,approach,list)
        matrix[[length(matrix) + 1]] <- matrixDistance
        maxDistance <- maxDistance(matrixDistance)
        maxValues <- c(maxValues, maxDistance)
      }
      selectedCluster <- getClusterDivisive(max(maxValues), maxValues)
      list <- initializedClusters[[selectedCluster]]

      clustersList <- posibleClusters[[selectedCluster]]
      matrixDistance <- matrix[[selectedCluster]]

      maxDistance <- maxValues[[selectedCluster]]
      goal <- activeClusters[[selectedCluster]]

      groupedClusters <- getCluster(maxDistance, matrixDistance)

      dividedClusters1 <- clustersList[groupedClusters[1]]
      dividedClusters2 <- clustersList[groupedClusters[2]]
      clusters[length(clusters)+1] <- dividedClusters1
      clusters[length(clusters)+1] <- dividedClusters2

      aux <- list()
      for (cluster in c(1:length(activeClusters))) {
        if(!equalCluster(activeClusters[[cluster]],goal)){
          aux[[length(aux) + 1]] <- activeClusters[[cluster]]
        }
      }

      activeClusters <- aux
      if(nrow(dividedClusters1[[1]]) > 1){
        activeClusters <- c(activeClusters, dividedClusters1)
      }
      if(nrow(dividedClusters2[[1]]) > 1){
        activeClusters <- c(activeClusters, dividedClusters2)
      }
    }
    for (index in c(1:length(clusters))) {
      cluster <- clusters[[index]]
      clusters[[index]] <- data.frame(cluster)
    }
    clusters
  }

equalCluster <- function(cluster1,cluster2){
  bool <- FALSE
  aux <- c()
  if(nrow(cluster1) != nrow(cluster2)){
    bool <- FALSE
  } else {
    for (k in (1:nrow(cluster1))) {
      row1 <- cluster1[k,]
      row2 <- cluster2[k,]
      if ((row1[1] == row2[1]) & (row1[2] == row2[2])){
        aux <- c(aux, TRUE)
      }
    }
  }
  if(length(aux) == nrow(cluster1)){
    bool <- TRUE
  }
  bool
}

