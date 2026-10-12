#' @title To check if two clusters are complementary
#' @description To check if two clusters include every element but without repeating anyone.
#' @param components is an elements list. It contains every component that has to be in one cluster or in the other one. But each element can only be included in one cluster.
#' @param cluster1 is a cluster (matrix).
#' @param cluster2 is a cluster (matrix).
#' @details This function checks if the cluster that will be divided contains the simple elements that they have to include. They have to contain every element, but anyone should be
#'  duplicated.
#' @details The function will return a boolean value.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return Boolean value.
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
#' complementaryClusters(components,cluster1,cluster2) #TRUE
#'
#' complementaryClusters(components,cluster3,cluster2) #FALSE
#'
#' @export


complementaryClusters <- function(components,cluster1,cluster2) {
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
  complementary
}


includeCluster <- function(cluster,element){
  index <- 1
  include <- FALSE
  while(nrow(cluster) >= index & !include){
    clusterElement <- cluster[index,]
    auxCluster <- matrix(clusterElement,ncol = 2)
    if(equalCluster(auxCluster, element)){
      include <- TRUE
    } else {
      index <- index +1
    }
  }
  include
}
