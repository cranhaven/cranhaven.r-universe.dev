
#' limits.histogram.disjoint.pca.variable
#' @author Jorge Arce Garro
#' @keywords internal
#' @param df.histogram Bin's Projections onto principal components
#' @param BIN.Matrix Number of Bins for each histogram projections
#'
#' @return Histogram Projection onto principal components
limits.histogram.disjoint.pca.variable<-function(df.histogram,BIN.Matrix){
  columns<-c("Object.Name","Id","Variable","Principal.Component","Positive","Coord.Min","Coord.Max","Frequency")
  df.disjoint<- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(df.disjoint)<-columns
  num.components<-max(df.histogram$Principal.Component)
  concepts<-unique(df.histogram$Object.Name)
  num.concepts<-length(concepts)
  variables<-unique(df.histogram$Variable)
  num.variables<-length(variables)
  row.hist.names<-concepts

  for(k in 1:num.components){

    for(i in 1:num.concepts){
      name.act<-concepts[i]
      num.bins<-BIN.Matrix[i,k]
      df.tmp<-df.histogram[df.histogram$Principal.Component == k & df.histogram$Object.Name == name.act,]
      min.tmp<-min(df.tmp$Coord.Min)
      max.tmp<-max(df.tmp$Coord.Max)
      large.bin<- (max.tmp - min.tmp)/num.bins
      seq.bin<-seq(from = min.tmp,to = max.tmp,by = large.bin)

      for(j in 1:num.variables){
        df.tmp.2<-df.tmp[df.tmp$Variable == variables[j],]
        df.tmp.2$large<-df.tmp.2$Coord.Max - df.tmp.2$Coord.Min
        dim.tmp.2<-dim(df.tmp.2)
        count.act<-1
        for(h in 1:num.bins)
        {
          interval.act<-seq.bin[c(h,h+1)]
          proba.pca<-0
          for(z in 1:dim.tmp.2[1]){
            large.act<-df.tmp.2$large[z]
            if(large.act > 0){
              int.interval<- intersection.interval(interval.act,c(df.tmp.2$Coord.Min[z],df.tmp.2$Coord.Max[z]))
              proba.pca<- proba.pca + (int.interval[2]-int.interval[1])/large.act*df.tmp.2$Frequency[z]
            }
          }
          df.disjoint.tmp<-data.frame(
            Object.Name = name.act,
            Id = paste0(name.act,'.',count.act),
            Variable = variables[j],
            Principal.Component = k,
            Positive = df.tmp$Positive[1],
            Coord.Min = interval.act[1],
            Coord.Max = interval.act[2],
            Frequency = proba.pca
          )
          df.disjoint<-rbind(df.disjoint,df.disjoint.tmp)
          count.act<-count.act+1

        }
      }
    }
  }
  return(df.disjoint)
}
