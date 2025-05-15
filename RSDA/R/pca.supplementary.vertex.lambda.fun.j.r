#' Calculate the variance
#'
#' @param x A Matrix
#' @param M Number of variables
#' @param N Number of concepts
#' @param sym.var.names Names of concepts
#' @param sym.data.vertex.matrix Vertex Matrix
#' @param tot.individuals Number of individuals
#' @param num.dimen.aux Number of dimensions
#'
#' @return Cumulative variance

pca.supplementary.vertex.lambda.fun.j<-function(x, M, N, sym.var.names, sym.data.vertex.matrix, tot.individuals,
          num.dimen.aux) {
  M.x <- matrix(x, nrow = N)
  colnames(M.x) <- sym.var.names
  M.x <- scale(M.x)
  mean.var <- attr(M.x, "scaled:center")
  desv.var <- attr(M.x, "scaled:scale")
  sym.data.vertex.matrix.cent <- sym.data.vertex.matrix
  for (i in 1:M) {
    sym.data.vertex.matrix.cent[, i] <- (sym.data.vertex.matrix.cent[,i] - mean.var[i])/desv.var[i]
  }
  M.x <- rbind(M.x, sym.data.vertex.matrix.cent)
  pca.max <- PCA(X = M.x, scale.unit = FALSE, ind.sup = (N +
                                                           1):tot.individuals, ncp = M, graph = FALSE)
  out <- list(pca.max = pca.max, out = -sum(pca.max$eig[(1:num.dimen.aux),1]))
  return(-sum(pca.max$eig[(1:num.dimen.aux),1]))
}

