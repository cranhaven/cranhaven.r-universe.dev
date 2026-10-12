#' @title To initialize clusters for the divisive algorithm.
#' @description To initialize clusters for the divisive algorithm.
#' @param initList is a clusters list. It will contain clusters with one element.
#' @details This function will calculate every cluster that can be created by joining initial clusters with each other. It creates clusters
#' from length = 1 until a cluster with every element is created.
#' @details These clusters will be used to find the most different clusters that we can create by dividing the initial cluster.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return A cluster list.
#' @examples
#'
#' data <- c(1:8)
#'
#' matrix <- matrix(data,ncol=2)
#'
#' listData <- toListDivisive(data)
#'
#' listMatrix <- toListDivisive(matrix)
#'
#' initClusters(listData)
#'
#' initClusters(listMatrix)
#'
#' @export

initClusters <- function(initList){

  clusters <- initList
  unitClusters <- initList
  aux <- initList
  goal <- initList[[length(initList)]]
  res <- c()
  auxAux <- c()

  while(nrow(clusters[[length(clusters)]]) != length(unitClusters)){

    for(i in seq_len(length(aux))){
      lastElement <- aux[[i]]
      lastElement <- (lastElement[nrow(lastElement),])
      lastElement <- matrix(lastElement,ncol=2)

      clusterIndex <- getClusterIndex(unitClusters,lastElement)
      for (j in 1:length(unitClusters)){
        cluster1 <- aux[[i]]
        cluster1 <- matrix(cluster1,ncol=2)
        cluster2 <- unitClusters[[j]]
        if (j > clusterIndex){
          newCluster <- c()
          for (k in (1:nrow(cluster1))){
            newCluster <- c(newCluster,cluster1[k,])
          }
          for (k in (1:nrow(cluster2))){
            newCluster <- c(newCluster,cluster2[k,])
          }
          newCluster <- matrix(newCluster, ncol=2, byrow=TRUE)
          res[[length(res) + 1]] <- newCluster
          data <- newCluster[nrow(newCluster),]
          asMatrix <- matrix(data,ncol=2)
          if(!equalCluster(asMatrix,goal)){
            auxAux[[length(auxAux) + 1]] <- newCluster
          }
        }
      }
    }
    aux <- auxAux
    auxAux <- c()
    clusters <- c(clusters, res)
    res <- c()
  }
  clusters
}

getClusterIndex <- function(clusters,cluster){
  found <- FALSE
  index <- 1
  while(!found & (index < length(clusters))){
    cluster1 <- (clusters[[index]])
    cluster2 <- (cluster)

    if(equalCluster(cluster1,cluster2)){
      found <- TRUE
    } else {
      index <- index + 1
    }
  }
  index
}
