#' fixed.pca.j.new
#' @author Jorge Arce Garro
#' @keywords internal
#' @importFrom FactoMineR PCA
fixed.pca.j.new<- function(sym.data,fixed.matrix){
  N<-nn <- sym.data$N
  mm <- sym.data$M
  seq.min<-seq(from = 1, to = 2*mm,by = 2)
  seq.max<-seq(from = 2, to = 2*mm,by = 2)

  centers<- fixed.matrix
  sym.data.vertex <- vertex.interval.new.j(sym.data)
  sym.data.vertex.matrix <- sym.data.vertex$vertex
  dim.vertex <- dim(sym.data.vertex.matrix)[1]
  tot.individuals <- N + dim.vertex
  centers <- rbind(centers, sym.data.vertex.matrix)

  PCA.centers<-PCA(X = centers, scale.unit = TRUE,
                   ind.sup = (N + 1):tot.individuals,
                   ncp = mm, graph = FALSE)

  centers.stan.mean<-PCA.centers$call$centre
  centers.stan.stand<-PCA.centers$call$ecart.type
  centers.stan<-sym.scale.matrix.j(centers,centers.stan.mean,centers.stan.stand,nn,mm)
  data<-stand.data(sym.data,centers.stan.mean,centers.stan.stand,nn,mm)

  svd<-list(values = PCA.centers$eig[,1],
            vectors = PCA.centers$svd$V)

  sym.PCA.res<-get.limits.PCA(sym.data,centers.stan,data[,seq.min],
                              data[,seq.max],svd,nn,mm)

  return(list(classic.PCA = PCA.centers,
              symbolic.PCA = sym.PCA.res))
}
