#' vertex.pca.j
#' @keywords internal
vertex.pca.j <- function(data.sym) {
  data.sym.vertex <- vertex.interval.new.j(data.sym)
  data.sym.center <- centers.interval.j(data.sym)
  data.sym.center <- data.sym.center$centers
  data.sym.center <- scale(data.sym.center)
  mean.var <- attr(data.sym.center, "scaled:center")
  desv.var <- attr(data.sym.center, "scaled:scale")
  N <- data.sym$N
  M <- data.sym$M
  sym.data.vertex.matrix.cent <- data.sym.vertex$vertex
  for (i in 1:M) {
    sym.data.vertex.matrix.cent[, i] <- (sym.data.vertex.matrix.cent[, i] - mean.var[i]) / desv.var[i]
  }
  dim.vertex <- dim(data.sym.vertex$vertex)[1]
  tot.individuals <- N + dim.vertex
  data.sym.matrix <- rbind(data.sym.center, sym.data.vertex.matrix.cent)
  pca.centers <- FactoMineR::PCA(X = data.sym.matrix, scale.unit = FALSE, ind.sup = (N +
    1):tot.individuals, ncp = M, graph = FALSE)
  data.sym.cent <- data.frame.to.RSDA.inteval.table.j(sym.scale.interval(
    data.sym,
    mean.var, desv.var
  ))
  res <- sym.interval.vertex.pca.j(data.sym.cent)
  class(res$Sym.Components) <- "sym.data.table"
  res$Sym.Components <- to.v3(res$Sym.Components)
  return(res)
}
