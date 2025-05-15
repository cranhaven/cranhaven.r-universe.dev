#' Optimized PCA Variance
#'
#' @keywords internal
#' @importFrom nloptr lbfgs
#' @param sym.data An Interval Matrix
#' @param num.dimension Number of dimensions
#'
#' @return Concept Projections onto the principal components,
#'         Classical PCA,
#'         Variance Best Matrix
#'
optim.pca.variance.j.new<-function (sym.data, num.dimension){
  N <- sym.data$N
  M <- sym.data$M
  num.dimen.aux <- num.dimension
  seq.min <- seq(from = 1, by = 2, length.out = M)
  seq.max <- seq(from = 2, by = 2, length.out = M)
  sym.var.names <- sym.data$sym.var.names
  sym.data.vertex <- vertex.interval.new.j(sym.data)
  sym.data.vertex.matrix <-  sym.data.vertex$vertex
  dim.vertex <- dim(sym.data.vertex.matrix)[1]
  tot.individuals <- N + dim.vertex
  min.interval <- as.vector(as.matrix(sym.data$data[, seq.min]))
  max.interval <- as.vector(as.matrix(sym.data$data[, seq.max]))
  init.point <- as.vector(as.matrix(centers.interval.j(sym.data)$centers))
  res.min <- nloptr::lbfgs(init.point, pca.supplementary.vertex.lambda.fun.j,
                   lower = min.interval, upper = max.interval, nl.info = FALSE,
                   control = list(xtol_rel = 1e-10, maxeval = 20000), N = N,
                   M = M, sym.var.names = sym.var.names,
                   sym.data.vertex.matrix = sym.data.vertex.matrix,
                   tot.individuals = tot.individuals,
                   num.dimen.aux = num.dimen.aux)
  M.x <- matrix(res.min$par, nrow = N)
  colnames(M.x) <- sym.var.names

  M.x <- rbind(M.x, sym.data.vertex.matrix)
  pca.max <- PCA(X = M.x, scale.unit = TRUE,
                 ind.sup = (N + 1):tot.individuals,
                 ncp = M, graph = FALSE)

  svd<-list(values = pca.max$eig[,1],
            vectors = pca.max$svd$V)

  best.stan.mean<-pca.max$call$centre
  best.stan.stand<-pca.max$call$ecart.type
  best.stan<-sym.scale.matrix.j(M.x,best.stan.mean,best.stan.stand,N,M)

  data<-stand.data(sym.data,best.stan.mean,best.stan.stand,N,M)

  sym.PCA.res<-get.limits.PCA(sym.data,best.stan,data[,seq.min],
                              data[,seq.max],svd,N,M)


  return(list(symbolic.PCA = sym.PCA.res,
              classic.PCA = pca.max,
              res.best = res.min))
}
