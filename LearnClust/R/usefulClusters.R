#' @title To delete clusters grouped.
#' @description To delete the clusters already used to create a new one.
#' @param list is a list of clusters.
#' @details This function is part of the hierarchical clusterization method. The function updates the cluster
#' list with the clusters used after to calculate de matrix distance.
#' @author Roberto Alcántara \email{roberto.alcantara@@edu.uah.es}
#' @author Juan José Cuadrado \email{jjcg@@uah.es}
#' @author Universidad de Alcalá de Henares
#' @return A list of clusters.
#' @examples
#'
#' data <- c(1:10)
#'
#' list <- toList(data)
#'
#' usefulClusters(list)
#'
#' @export


usefulClusters <-
function(list){
    listAux <- c()
    for (i in (1:(length(list)))){
        cluster <- list[[i]]
        if (!is.null(cluster)){
            rowCluster <- cluster[1,]
            if(rowCluster[3] | (rowCluster[3] != 0)){
                listAux[[i]] <- cluster
            }
        }
    }
    listAux
}
