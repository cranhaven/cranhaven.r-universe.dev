#' @title To explain how and why two clusters are complementary.
#' @description To explain how and why two clusters include every element but without repeating anyone.
#' @param components is an elements list. It contains every component that has to be in one cluster or in the other one. But each element can only be included in one cluster.
#' @param cluster1 is a cluster (matrix).
#' @param cluster2 is a cluster (matrix).
#' @details This function checks if the cluster that will be divided contains the simple elements that they have to include. They have to contain every element, but anyone should be
#'  duplicated.
#' @details The function will return a boolean value.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Boolean value. Explanation.
#' @examples
#'
#' data <- c(1,2,1,3,1,4,1,5)
#'
#' components <- toListDivisive(data)
#'
#' cluster1 <- matrix(c(1,2,1,3),ncol=2)
#' cluster2 <- matrix(c(1,4,1,5),ncol=2)
#' cluster3 <- matrix(c(1,6,1,7),ncol=2)
#'
#' complementaryClusters.details(components,cluster1,cluster2) #TRUE
#'
#' complementaryClusters.details(components,cluster3,cluster2) #FALSE
#'
#' @export

complementaryClusters.details <- function(components,cluster1,cluster2) {
  message("\n 'complementaryClusters' checks if clusters are complementary.   \n\n")
  message("\n Each element from 'components' list has to be in one cluster, but never included in both. \n")
  message("\n If every element is in one cluster, the function will return 'TRUE',\n")
  message("\n if not, the result will be 'FALSE' \n\n")
  message("\n The clusters to be checked are: \n")
  print(cluster1)
  print(cluster2)
  message("\n And they have to include: \n")
  print(components)
  complementary <- FALSE
  check <- c()
  for(i in c(1:length(components))){
    value <- components[[i]]
    if(xor(includeCluster(cluster1,value),includeCluster(cluster2,value))){
      check <- c(check , TRUE)
    }
  }
  if(length(check) == length(components)){
    complementary <- TRUE
  }
  message("\n\n So, the result is ", complementary, ".\n\n")
  complementary
}
