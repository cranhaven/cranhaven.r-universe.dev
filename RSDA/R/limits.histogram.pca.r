#' limits.histogram.pca
#' @keywords internal
#' @param sym.hist.matrix A Histogram Matrix
#' @param pca.sym.interval PCA result of Interval's PCA
#'
#' @return Bin's Projections onto principal components

limits.histogram.pca<-function(sym.hist.matrix,pca.sym.interval){
  sym.interval.limits<-get.sym.interval.limits(sym.hist.matrix)
  sym.interval.limits.V2<-to.v2(sym.interval.limits)
  average.col<-pca.sym.interval$classic.PCA$call$centre
  stand.col<-pca.sym.interval$classic.PCA$call$ecart.type
  pca.vectors<-pca.sym.interval$classic.PCA$svd
  pca.sym.coord<-pca.sym.interval$symbolic.PCA$Sym.Components$data
  vectors<-pca.vectors$V

  columns<-c("Object.Name","Id","Variable","Principal.Component","Positive","Coord.Min","Coord.Max","Frequency")
  df<- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(df)<-columns

  num.components<-dim(vectors)[2]
  num.concepts<-sym.interval.limits.V2$N
  num.variables<-sym.interval.limits.V2$M
  row.hist.names<-row.names(sym.hist.matrix)
  variables<-colnames(sym.hist.matrix)

  for(k in 1:num.variables){
    for(i in 1:num.concepts){
      row.hist.names.act<-row.hist.names[i]
      for(j in 1:num.variables){
        bool.signo<-vectors[j,k] >= 0
        temp.hist<-sym.hist.matrix[[j]][[i]]
        breaks.norm<- (temp.hist$breaks-average.col[j])/(stand.col[j])
        num.breaks<-length(breaks.norm)-1
        min.act<-pca.sym.coord[i,2*(k-1)+1]
        max.act<-pca.sym.coord[i,2*k]
        if(bool.signo){
          for(h in 1:num.breaks){
            id<-paste0(row.hist.names.act,".",h)
            Coord.min<- as.numeric(min.act + (breaks.norm[h]-breaks.norm[1])*vectors[j,k])
            Coord.max<- as.numeric(max.act + (breaks.norm[h+1] - breaks.norm[num.breaks+1])*vectors[j,k])

            data.tmp<-data.frame(
              Object.Name = row.hist.names.act,
              Id = id,
              Variable = variables[j],
              Principal.Component = k,
              Positive = bool.signo,
              Coord.Min = Coord.min,
              Coord.Max = Coord.max,
              Frequency = temp.hist$props[h]
            )
            df<-rbind(df,data.tmp)
          }
        }else{
          for(h in 1:num.breaks){
            id<-paste0(row.hist.names.act,".", h)
            Coord.min<- as.numeric(min.act + (breaks.norm[h+1] - breaks.norm[num.breaks+1])*vectors[j,k])
            Coord.max<- as.numeric(max.act - (breaks.norm[1]-breaks.norm[h])*vectors[j,k])
            data.tmp<-data.frame(
              Object.Name = row.hist.names.act,
              Id = id,
              Variable = variables[j],
              Principal.Component = k,
              Positive = bool.signo,
              Coord.Min = Coord.min,
              Coord.Max = Coord.max,
              Frequency = temp.hist$props[h]
            )
            df<-rbind(df,data.tmp)
          }
        }
      }
    }
  }
  return(df)
}
