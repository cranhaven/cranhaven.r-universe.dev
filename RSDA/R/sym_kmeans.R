#' Symbolic k-Means
#' @name sym.kmeans
#' @aliases sym.kmeans
#' @author Oldemar Rodriguez Rojas
#' @description This is a function is to carry out a k-means overs a interval symbolic data matrix.
#' @usage sym.kmeans(sym.data, k = 3, iter.max = 10, nstart = 1,
#' algorithm = c('Hartigan-Wong', 'Lloyd', 'Forgy', 'MacQueen'))
#' @param sym.data Symbolic data table.
#' @param k The number of clusters.
#' @param iter.max Maximun number of iterations.
#' @param nstart As in R kmeans function.
#' @param algorithm The method to be use, as in kmeans R function.
#'
#' @return
#' This function return the following information: \cr
#'
#' K-means clustering with 3 clusters of sizes 2, 2, 4\cr
#'
#' Cluster means:\cr
#'
#'   GRA     FRE     IOD    SAP\cr
#'
#' 1 0.93300 -13.500 193.500 174.75\cr
#'
#' 2 0.86300  30.500  54.500 195.25\cr
#'
#' 3 0.91825  -6.375  95.375 191.50\cr
#'
#'
#' Clustering vector:\cr
#'
#'   L  P Co  S Ca  O  B  H \cr
#'
#' 1  1  3  3  3  3  2  2 \cr
#'
#' Within cluster sum of squares by cluster:\cr
#'
#'   [1] 876.625 246.125 941.875\cr
#'
#' (between_SS / total_SS =  92.0 %) \cr
#'
#' Available components:\cr
#'
#'   [1] 'cluster'      'centers'      'totss'        'withinss'     'tot.withinss' 'betweenss'  \cr
#'
#' [7] 'size'        \cr
#'
#' @references
#' Carvalho F., Souza R.,Chavent M., and Lechevallier Y. (2006)
#' Adaptive Hausdorff distances and dynamic clustering of symbolic interval data. Pattern
#' Recognition Letters Volume 27, Issue 3, February 2006, Pages 167-179
#'
#' @seealso sym.hclust
#' @examples
#' data(oils)
#' sk <- sym.kmeans(oils, k = 3)
#' sk$cluster
#' @keywords Symbolic Kmeans
#' @export
#' @importFrom stats kmeans
#'
sym.kmeans <- function(sym.data, k = 3, iter.max = 10, nstart = 1,
                       algorithm = c(
                         "Hartigan-Wong", "Lloyd",
                         "Forgy", "MacQueen"
                       )) {
  sym.data <- to.v2(sym.data)
  algorithm <- match.arg(algorithm)
  idn <- all(sym.data$sym.var.types == "$I")
  if (idn == FALSE) {
    stop("The two variables have to be interval type")
  }
  nn <- sym.data$N
  mm <- sym.data$M
  centers <- matrix(0, nn, mm)
  centers <- as.data.frame(centers)
  rownames(centers) <- sym.data$sym.obj.names
  colnames(centers) <- sym.data$sym.var.names
  for (i in 1:nn) {
    for (j in 1:mm) {
      centers[i, j] <- (sym.var(sym.data, j)$var.data.vector[
        i,
        1
      ] + sym.var(sym.data, j)$var.data.vector[i, 2]) / 2
    }
  }
  return(stats::kmeans(centers, k, iter.max, nstart, algorithm))
}
