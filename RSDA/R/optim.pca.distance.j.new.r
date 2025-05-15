#' Optimized PCA Distance
#' @keywords internal
#' @importFrom nloptr lbfgs
#'
#' @param sym.data An Interval Matrix
#'
#' @return Concept Projections onto the principal components,
#'         Classical PCA,
#'         Variance Best Matrix
#'
optim.pca.distance.j.new<-function (sym.data){
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
  res.min <- nloptr::lbfgs(init.point, pca.supplementary.vertex.fun.j.new,
                   lower = min.interval, upper = max.interval, nl.info = FALSE,
                   control = list(xtol_rel = 1e-05, maxeval = 20000), N = N,
                   M = M, sym.var.names = sym.var.names,
                   sym.data.vertex.matrix = sym.data.vertex.matrix,
                   tot.individuals = tot.individuals)
  M.x <- matrix(res.min$par, nrow = N)
  colnames(M.x) <- sym.var.names

  M.x <- rbind(M.x, sym.data.vertex.matrix)

  pca.min <- PCA(X = M.x, scale.unit = TRUE,
                 ind.sup = (N + 1):tot.individuals,
                 ncp = M, graph = FALSE)

  svd<-list(values = pca.min$eig[,1],
            vectors = pca.min$svd$V)

  best.stan.mean<-pca.min$call$centre
  best.stan.stand<-pca.min$call$ecart.type
  best.stan<-sym.scale.matrix.j(M.x,best.stan.mean,best.stan.stand,N,M)

  data<-stand.data(sym.data,best.stan.mean,best.stan.stand,N,M)

  sym.PCA.res<-get.limits.PCA(sym.data,best.stan,data[,seq.min],
                              data[,seq.max],svd,N,M)

  return(list(symbolic.PCA = sym.PCA.res,
              classic.PCA = pca.min,
              res.best = res.min))
}
