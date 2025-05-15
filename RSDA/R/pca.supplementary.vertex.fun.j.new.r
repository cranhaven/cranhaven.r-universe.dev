#' Calculate the distance
#'
#' @param x A Matrix
#' @param M Number of variables
#' @param N Number of concepts
#' @param sym.var.names Names of concepts
#' @param sym.data.vertex.matrix Vertex Matrix
#' @param tot.individuals Number of individuals
#'
#' @return Distance

pca.supplementary.vertex.fun.j.new<-function (x, N, M, sym.var.names,
                                          sym.data.vertex.matrix,
                                          tot.individuals){
  M.x <- matrix(x, nrow = N)
  colnames(M.x) <- sym.var.names
  M.x <- rbind(M.x, sym.data.vertex.matrix)
  pca.min <- PCA(X = M.x, scale.unit = TRUE,
                 ind.sup = (N + 1):tot.individuals,
                 ncp = M, graph = FALSE)
  min.dist.pca <- pca.min$ind.sup$dist * pca.min$ind.sup$dist
  return(sum(min.dist.pca))
}
