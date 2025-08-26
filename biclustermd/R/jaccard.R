#' Compute the Jaccard similarity coefficient for two clusterings
#' 
#' @param clus1 vector giving the first set of clusters
#' @param clus2 vector giving the second set of clusters
#' 
#' @return a numeric
#' 
#' @references Milligan, G.W. and Cooper, M. C. (1986) \emph{A study of the comparability of external criteria for hierarchical cluster analysis. 
#' Multivariate Behavioral Research, 21, 441-458.}

jaccard_similarity <- function(clus1, clus2) {
  
  M11 <- 0
  M10 <- 0
  M01 <- 0
  M00 <- 0
  
  n <- length(clus1)
  
  for(i in 1:(n - 1)) {
    for(j in (i + 1):n) {
      
      cm1 <- clus1[i] == clus1[j]
      cm2 <- clus2[i] == clus2[j]
      
      if(cm1 & cm2) {
        M11 <- M11 + 1
      } else if(!cm1 & cm2) {
        M01 <- M01 + 1
      } else if(cm1 & !cm2) {
        M10 <- M10 + 1
      } else {
        M00 <- M00 + 1
      }
    }
  }
  
  M11 / (M10 + M01 + M11)
  
}