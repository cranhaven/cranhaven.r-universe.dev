#' @title To explain agglomerative hierarchical clusterization algorithm by distance and approach.
#' @description To explain the complete agglomerative hierarchical clusterization algorithm choosing distance and approach type.
#' @param data could be a numeric vector, a matrix or a numeric data frame. It will be transformed into matrix and list to be used.
#' @param distance is a string. It chooses the distance to use.
#' @param approach is a string. It chooses the approach to use.
#' @details This function is the main part of the agglomerative hierarchical clusterization method.
#' It explains the theoretical algorithm step by step.
#' @details 1 - The function transforms data into useful object to be used.
#' @details 2 - It creates the clusters.
#' @details 3 - It calculates a matrix distance with the clusters created by applying the distance and the approach given.
#' @details 4 - It chooses the distance value and gets the clusters.
#' @details 5 - It groups the clusters in a new one and updates clusters list.
#' @details 6 - It repeats these steps until an unique cluster exists.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return agglomerative algorithm explanation.
#' @examples
#'
#' a <- c(1,2,1,3,1,4,1,5,1,6)
#'
#' matrixA <- matrix(a,ncol=2)
#'
#' dataFrameA <- data.frame(matrixA)
#'
#' agglomerativeHC.details(a,'EUC','MAX')
#'
#' agglomerativeHC.details(matrixA,'MAN','AVG')
#'
#' agglomerativeHC.details(dataFrameA,'CAN','MIN')
#'
#' @export
#' @importFrom graphics plot


agglomerativeHC.details <-
  function(data,distance,approach){
    message("  Agglomerative hierarchical clustering is a classification technique that initializes \n a cluster for each data. \n")
    message("\n  It calculates the distance between datas depending on the approach type given and \n")
    message("\n it creates a new cluster joining the most similar clusters until getting only one.  \n")
    if(!is.data.frame(data)){
      mtx <- matrix(data, ncol=2, byrow=TRUE)
      plot(mtx, main = 'Initial Data Visualization',xlab = 'First property', ylab = 'Second property')
    }
    list <- toList.details(data)
    message("\n  These are the clusters with only one element:\n")
    print(list)
    lengthList <- length(list)
    goal <- list[[length(list)]]
    res <- list
    clusters <- c()
    oneCluster <- (nrow(goal) == lengthList)
    message("\n  In each step: \n   - It calculates a matrix distance between active clusters depending on the approach and distance type. \n")
    message("   - It gets the minimum distance value from the matrix. \n")
    message("   - It creates a new cluster joining the minimum distance clusters. \n")
    message("   - It repeats these steps while final clusters do not include all datas. \n")
    i <- 1
    while (!oneCluster){
      message('\n_____________________________________________________________________________________________\n')
      message("STEP => ", i , "\n")
      #------------
      matrixDistance <- mdAgglomerative(list,distance,approach)
      message("\n Matrix Distance (distance type = ", distance,", approach type = ", approach, "): \n")
      print(matrixDistance)
      #------------
      minDistance <- minDistance(matrixDistance)
      message("\n The minimum distance is: ",minDistance," \n")
      #------------
      groupedClusters <- getCluster(minDistance, matrixDistance)
      message("\n The closest clusters are: ",groupedClusters[1], ", ",groupedClusters[2]," \n")
      #------------
      clusters[[length(clusters)+1]] <- groupedClusters
      message("\n The grouped clusters are added to the solution.\n")
      updatedClusters <- newCluster(list, groupedClusters)
      message("\n Grouping clusters ", groupedClusters[1], " and cluster ", groupedClusters[2], ", it is created a new cluster: \n")
      print(data.frame(cleanClusters(list(updatedClusters[[length(updatedClusters)]]))))
      res[[length(res) + 1]] <- updatedClusters[[length(updatedClusters)]]
      message("\n The new cluster is added to the solution.\n")
      cleanCluster <- usefulClusters(updatedClusters)
      list <- cleanCluster
      goal <- updatedClusters[[length(updatedClusters)]]
      oneCluster <- (nrow(goal) == lengthList)
      i <- i + 1
    }
    message("\n This loop has been repeated until the last cluster contained every single clusters.\n")
  }
