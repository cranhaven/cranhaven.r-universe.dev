#' @title To explain the divisive hierarchical clusterization algorithm by distance and approach.
#' @description To explain the complete divisive hierarchical clusterization algorithm by choosing distance and approach types.
#' @param data could be a numeric vector, a matrix or a numeric data frame. It will be transformed into matrix and list to be used.
#' @param distance is a string. It chooses the distance to use.
#' @param approach is a string. It chooses the approach to use.
#' @details This function is the main part of the divisive hierarchical clusterization method.
#' It explains the theoretical algorithm step by step.
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
#' @return A list with the divided clusters. Explanation
#' @examples
#'
#' a <- c(1,2,1,3,1,4,1,5,1,6)
#'
#' matrixA <- matrix(a,ncol=2)
#'
#' dataFrameA <- data.frame(matrixA)
#'
#' divisiveHC.details(a,'EUC','MAX')
#'
#' divisiveHC.details(matrixA,'MAN','AVG')
#'
#' divisiveHC.details(dataFrameA,'CHE','MIN')
#'
#' @export

divisiveHC.details <-
  function(data,distance,approach){
    message("\n  Divisive hierarchical clustering is a classification technique that initializes a cluster \n with every data. \n")
    message("\n  It calculates the distance between datas depending on the approach type given and \n")
    message("\n it divides the most different clusters until any cluster can be divided again.  \n")
    if(!is.data.frame(data)){
		mtx <- matrix(data, ncol=2)
		plot(mtx, main = 'Initial Data Visualization',xlab = 'First property', ylab = 'Second property')
    }
	list <- toListDivisive(data)
    message("\n  These are the clusters with only one element:\n")
    printedList <- list()
    for (index in c(1:length(list))) {
      cluster <- list[[index]]
      printedList[[index]] <- data.frame(cluster)
    }
    print(printedList)
    lengthList <- length(list)
    clustersList <- initClusters(list)
    message("\n  And this is the initial cluster with every element:\n")
    init <- clustersList[[length(clustersList)]]
    print(data.frame(init))
    activeClusters <- list(init)
    clusters <- list(init)

    message("\n  In each step: \n   - It calculates a matrix distance between valid clusters depending on the approach \n and the distance types. \n")
    message("   - It gets the maximal distance value from the matrixes of every cluster. We have to \n look for the most different clusters available  in every cluster. \n")
    message("   - It divides the selected cluster in two new and complementary clusters using tha maximal \n distance between clusters. \n")
    message("   - It repeats these steps while there isn't any cluster that can be divided again. \n")
    step <- 0
    while(length(activeClusters) > 0){
      step <- step + 1
      message('\n_____________________________________________________________________________________________\n')
      message("STEP => ", step , "\n")

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
      message("\n The algorithm calculates a matrix distance from every active cluster, but it has to find \n the maximal value from all matrix and then chooses which is the selected one.\n")
      message("\n It gets the maximal value from every matrix and then chooses the maximal between them. So... \n\n")
      selectedCluster <- getClusterDivisive(max(maxValues), maxValues)
      list <- initializedClusters[[selectedCluster]]

      clustersList <- posibleClusters[[selectedCluster]]
      matrixDistance <- matrix[[selectedCluster]]
      message("\n Matrix Distance (distance type = ", distance,", approach type = ", approach, "): \n")
      print(matrixDistance)

      maxDistance <- maxValues[[selectedCluster]]
      goal <- activeClusters[[selectedCluster]]
      message("\n The maximal distance is: ",maxDistance," \n")

      groupedClusters <- getCluster(maxDistance, matrixDistance)
      message("\n The most distant clusters are:  \n")

      dividedClusters1 <- clustersList[groupedClusters[1]]
      print(data.frame(dividedClusters1))
      message("\n")
      dividedClusters2 <- clustersList[groupedClusters[2]]
      print(data.frame(dividedClusters2))
      clusters[length(clusters)+1] <- dividedClusters1
      clusters[length(clusters)+1] <- dividedClusters2

      message("\n The divided clusters are added to the solution.\n")

      aux <- list()
      for (cluster in c(1:length(activeClusters))) {
        if(!equalCluster(activeClusters[[cluster]],goal)){
          aux[[length(aux) + 1]] <- activeClusters[[cluster]]
        }
      }

      activeClusters <- aux
      if(nrow(dividedClusters1[[1]]) > 1){
        activeClusters <- c(activeClusters, dividedClusters1)
        message("\n - The first divided cluster is added to the active clusters list because it can be divided again. \n ")
      }
      if(nrow(dividedClusters2[[1]]) > 1){
        activeClusters <- c(activeClusters, dividedClusters2)
        message("\n - The second divided cluster is added to the active clusters list because it can be divided again. \n ")
      }
      message("\n If the divided clusters can't be divided again, then they don`t be added to the active clusters. \n")

    }
    message("\n This loop has been repeated until there aren't any active cluster, that is, any cluster \n can be divided again. \n")
    clusters
  }
