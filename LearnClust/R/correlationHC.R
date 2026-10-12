#' @title To execute hierarchical correlation algorithm.
#' @description To execute hierarchical correlation algorithm applying weights, distance types, ...
#' @param data is a data frame with the main data.
#' @param target is a data frame , a numeric vector or a matrix. Default value = NULL.
#' @param weight is a numeric vector. Default value = empty vector.
#' @param distance is a string. The distance type. Default value = Euclidean distance.
#' @param normalize is a boolean parameter. If the user wants to normalize weights. Default value = TRUE.
#' @param labels is a string vector. For the graphical solution. Default value = NULL.
#' @details This function execute the complete hierarchical correlation method.
#' @details 1 - The function transforms data in useful object to be used.
#' @details 2 - It creates the clusters.
#' @details 3 - It calculates the distance from the target to every cluster applying distance type given.
#' @details 4 - It orders the distance in increasing way.
#' @details 5 - It orders the clusters according to their distance from the previous step
#' @details 6 - It shows the clusters sorted and the distance used.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return R object with a dendrogram, the sorted distances and the list with every cluster.
#' @examples
#'
#' data <- matrix(c(1,2,1,4,5,1,8,2,9,6,3,5,8,5,4),ncol= 3)
#'
#' dataFrame <- data.frame(data)
#'
#' target1 <- c(1,2,3)
#'
#' target2 <- dataFrame[1,]
#'
#' weight1 <- c(1,6,3)
#'
#' weight2 <- c(0.1,0.6,0.3)
#'
#' correlationHC(dataFrame, target1)
#'
#' correlationHC(dataFrame, target1, weight1)
#'
#' correlationHC(dataFrame, target1, weight1, normalize = FALSE)
#'
#' correlationHC(dataFrame, target1, weight2, 'CAN', FALSE)
#'
#' @export

correlationHC <- function(data, target = NULL, weight = c(), distance = 'EUC', normalize = TRUE, labels = NULL){

  res = list()

  weight <- normalizeWeight(normalize,weight,data)

  if(is.null(target) | is.null(data)){
    res$sortedValues <- NULL
    res$distances <- NULL
    res <- NULL
    message('\n Not correct parameters! \n\n')
  } else {
    list <- initData(data)
    target <- initTarget(target,data)
    distances <- c()
    for (index in c(1:length(list))) {
      dist <- 0
      cluster <- list[[index]]
      dist <- distances(list[[index]],target,distance,weight)
      distances <- c(distances, dist)
    }
    sortedDistances <- sort(distances)
    values <- data.frame()
    names <- names(data)
    clusters <- list()
    dendrogramLabels <- c()
    clustersDendrogram <- data.frame()
    for (cluster in c(1:length(list))) {
      distance <- sortedDistances[cluster]
      clust <- getClusterPosition(distance,distances,list)
      list[[clust[[2]]]] <- data.frame(list[[clust[[2]]]])
      if(cluster == 1){
        data <- c()
        data <- c(-clust[[2]])
      } else if(cluster == 2) {
        data <- c(data, -clust[[2]])
        clustersDendrogram <- rbind(clustersDendrogram, data.frame(matrix(data,ncol=2)))
      }else {
        clustersDendrogram <- rbind(clustersDendrogram, data.frame(matrix(c(-clust[[2]], cluster-2),ncol=2)))
      }
      dendrogramLabels <- c(dendrogramLabels,clust[[2]])
      values <- rbind(values, data.frame(clust[[1]]))
      clusters[[length(clusters) + 1]] <- data.frame(clust[[1]])
    }
    names(values) <- names
    distances <- data.frame(cluster=dendrogramLabels,sortedDistances)

    res$sortedValues <- data.frame(cluster=dendrogramLabels,values)
    if(is.data.frame(data)){
      names(res$sortedValues) <- names(data)
    }
    res$distances <- distances
    dendrogram <- list()
    dendrogram$merge <- as.matrix(clustersDendrogram)
    dendrogram$height <- c(1:(length(list)-1))
    dendrogram$order <- c(1:length(list))
    if(!is.null(labels)){
      dendrogram$labels <- labels
    } else {
      dendrogram$labels <- c(1:length(list))
    }
    class(dendrogram) <- "hclust"
    res$dendrogram <- dendrogram
    res
  }
}



getClusterPosition <- function(distance,vector,list){
  found <- FALSE
  index <- 1
  while(!found & (index < length(vector))){

    if(!is.data.frame(list[[index]]) & vector[index] == distance){
      found <- TRUE
    } else {
      index <- index + 1
    }
  }
  cluster <- list[[index]]
  res <- list(cluster , index)
  res
}


