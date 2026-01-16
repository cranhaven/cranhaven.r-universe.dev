#' Find the Number of Clusters by the Standard  Silhouette Statistics
#'
#' Find the number of clusters by the standard Silhouette statistics. The cluster is
#' hierarchical.

#' @param nClus Maximum number of groups.
#' @param distanceMatrix Matrix of distances.
#' @param method Hierarchical method "single", "average","complete".
#'
#' @return A list containing:
#' \itemize{
#'    \item nClus - Number of groups
#'    \item  list - Silhouette statistics for each value of nclus.
#' }
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output_gcc <- GCCmatrix(TaiwanAirBox032017[1:100,1:10])
#' output <- silh.clus(nClus=3,distanceMatrix=output_gcc$DM ,method="complete")
#'
silh.clus <- function(nClus, distanceMatrix, method){


  if(sum(class(distanceMatrix)!="dist")!=0) distanceMatrix <- as.dist(distanceMatrix)

  silIndex <- data.frame(nClus = 1, silIndex = 0)

  if(method == "complete"){


    hclust.dist <- hclust(distanceMatrix, method="complete")
    Cl.hclust <- cutree(hclust.dist, 2:nClus)

    for (jj in 2:nClus) {
      coef <- silhouette(Cl.hclust[, jj-1], distanceMatrix)
      jjSilIndex <- mean(coef[, "sil_width"])
      silIndex <- rbind(silIndex, data.frame(nClus = jj, silIndex = jjSilIndex))
    }


  }

  if(method == "average"){


    hclust.dist <- hclust(distanceMatrix, method="average")
    Cl.hclust <- cutree(hclust.dist, 2:nClus)

    for (jj in 2:nClus) {
      coef <- silhouette(Cl.hclust[, jj-1], distanceMatrix)
      jjSilIndex <- mean(coef[, "sil_width"])
      silIndex <- rbind(silIndex, data.frame(nClus = jj, silIndex = jjSilIndex))

    }


  }



  if(method == "single"){


    hclust.dist <- hclust(distanceMatrix, method="single")
    Cl.hclust <- cutree(hclust.dist, 2:nClus)

    for (jj in 2:nClus) {
      coef <- silhouette(Cl.hclust[, jj-1], distanceMatrix)
      jjSilIndex <- mean(coef[, "sil_width"])
      silIndex <- rbind(silIndex, data.frame(nClus = jj, silIndex = jjSilIndex))

    }


  }
  maxPos <- which(silIndex[, "silIndex"]==max(silIndex[, "silIndex"]))
  return (list(nClus = silIndex[maxPos, "nClus"], coef = silIndex))
}
