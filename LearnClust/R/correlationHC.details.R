#' @title To explain how hierarchical correlation algorithm works.
#' @description To explain how the hierarchical correlation algorithm works.
#' @param data is a data frame with the main data.
#' @param target is a data frame , a numeric vector or a matrix. Default value = NULL.
#' @param weight is a numeric vector. Default value = empty vector.
#' @param distance is a string. The distance type. Default value = Euclidean distance.
#' @param normalize is a boolean parameter. If the user wants to normalize weights. Default value = TRUE.
#' @param labels is a string vector. For the graphical solution. Default value = NULL.
#' @details This function explains the complete hierarchical correlation method.
#' It explains the theoretical algorithm step by step.
#' @details 1 - The function transforms data in useful object to be used.
#' @details 2 - It creates the clusters.
#' @details 3 - It calculates the distance from the target to every cluster applying the distance type given.
#' @details 4 - It orders the distance in an increasing way.
#' @details 5 - It orders the clusters according to their distance from the previous step
#' @details 6 - It shows the clusters sorted and the distance used.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return R object with a dendrogram, the sorted distances and the list with every cluster. Explanation.
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
#' correlationHC.details(dataFrame, target1)
#'
#' correlationHC.details(dataFrame, target1, weight1)
#'
#' correlationHC.details(dataFrame, target1, weight1, normalize = FALSE)
#'
#' correlationHC.details(dataFrame, target1, weight2, 'CAN', FALSE)
#'
#' @export

correlationHC.details <- function(data, target = NULL, weight = c(), distance = 'EUC', normalize = TRUE, labels = NULL){

  message("  Correlation hierarchical function is a classification technique that initializes a cluster for each data. \n")
  message("\n  It calculates the distance between clusters and a target given depending on the distance type.  \n")

  res = list()
  message("\n The function applies weights to each property from main data to get weighted results.  \n")
  if(normalize){
    message("\n Due to normalized = TRUE, the initial weights change to a [0,1] values.  \n")
  } else {
    message("\n Due to normalized = FALSE, the initial weights do not change. \n")
  }
  message("\n These are the weight to be used: \n")
  weight <- normalizeWeight(normalize,weight,data)
  print(weight)

  if(is.null(target) | is.null(data)){
    message("\n If target or main data are not acceptable data, the function finishes the process. \n")
    res$sortedValues <- NULL
    res$distances <- NULL
    res <- NULL
    message('\n Not correct parameters! \n\n')
  } else {
    list <- initData(data)
    message("\n Initialized data are (more information about how to initialize data in 'initData.details'): \n")
    print(list)
    target <- initTarget(target,data)
    message("\n Initialized target is (more information about how to initialize data in 'initTarget.details'): \n")
    print(target)
    distances <- c()
    message("\n The function calculates the distances between each cluster and the target. It applies \n weights and uses ", distance, " distance type. \n \n")
    for (index in c(1:length(list))) {
      dist <- 0
      cluster <- list[[index]]
      dist <- distances(list[[index]],target,distance,weight)
      distances <- c(distances, dist)
    }
    message("\n The calculated distances are:  \n")
    print(distances)
    message("\n The previous distances sorted are:  \n ")
    sortedDistances <- sort(distances)
    print(sortedDistances)
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
    message("\n Then, using sorted distances, the function order the clusters. \n \n")
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
    message("\n Finally, the sorted distances are:  \n")
    print(res$distances)
    message("\n The sorted clusters are:  \n ")
    print(res$sortedValues)
    message("\n And the final dendrogram is on the image. \n \n")
    plot(res$dendrogram)
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


