#' optim.desv.fun.interval
#' @keywords internal
#' @importFrom nloptr lbfgs
optim.pca.distance.j <- function(sym.data) {
  N <- sym.data$N
  M <- sym.data$M
  seq.min <- seq(from = 1, by = 2, length.out = M)
  seq.max <- seq(from = 2, by = 2, length.out = M)

  sym.var.names <- sym.data$sym.var.names
  sym.data.vertex <- vertex.interval.new.j(sym.data)
  sym.data.vertex.matrix <- sym.data.vertex$vertex
  dim.vertex <- dim(sym.data.vertex.matrix)[1]
  tot.individuals <- N + dim.vertex

  min.interval <- as.vector(as.matrix(sym.data$data[, seq.min]))
  max.interval <- as.vector(as.matrix(sym.data$data[, seq.max]))
  init.point <- as.vector(as.matrix(centers.interval.j(sym.data)$centers))

  res.min <- nloptr::lbfgs(init.point, pca.supplementary.vertex.fun.j,
    lower = min.interval,
    upper = max.interval, nl.info = FALSE, control = list(xtol_rel = 1e-08, maxeval = 20000),
    N = N, M = M, sym.var.names = sym.var.names, sym.data.vertex.matrix = sym.data.vertex.matrix,
    tot.individuals = tot.individuals
  )

  M.x <- matrix(res.min$par, nrow = N)
  colnames(M.x) <- sym.var.names
  M.x <- scale(M.x)
  mean.var <- attr(M.x, "scaled:center")
  desv.var <- attr(M.x, "scaled:scale")
  sym.data.vertex.matrix.cent <- sym.data.vertex.matrix

  for (i in 1:M) {
    sym.data.vertex.matrix.cent[, i] <- (sym.data.vertex.matrix.cent[, i] - mean.var[i]) / desv.var[i]
  }
  M.x <- rbind(M.x, sym.data.vertex.matrix.cent)
  pca.min <- FactoMineR::PCA(
    X = M.x, scale.unit = FALSE, ind.sup = (N + 1):tot.individuals,
    ncp = M, graph = FALSE
  )

  pca.min.sym <- sym.interval.pca.limits.new.j(sym.data, pca.min$ind.sup$coord, sym.data.vertex$num.vertex)

  return(list(Sym.Components = pca.min.sym, pca.min = pca.min, res.min = res.min))
}
