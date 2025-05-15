#' sym.histogram.pca
#'
#' @author Jorge Arce Garro
#' @param sym.hist.matrix A Histogram matrix
#' @param BIN.Matrix A matrix with the number of bins for each individual and variable
#' @param method Weigthed Method
#'
#' @importFrom dplyr group_by summarise
#'
#' @return Histogram PCA
#'
#' @examples
#' \dontrun{
#' data("hardwoodBrito")
#' Hardwood.histogram<-hardwoodBrito
#' weighted.center<-weighted.center.Hist.RSDA(Hardwood.histogram)
#'  M<-length(Hardwood.cols)
#'  N<-length(Hardwood.names)
#'  BIN.Matrix<-matrix(rep(3,N*M),nrow = N)
#' pca.hist<-sym.histogram.pca(Hardwood.histogram,BIN.Matrix)
#' pca.hist
#' }
#'
#' @export
sym.histogram.pca<-function(sym.hist.matrix,BIN.Matrix,method = NULL){
  weighted.center<-weighted.center.Hist.RSDA(sym.hist.matrix)
  interval.matrix<-get.sym.interval.limits(sym.hist.matrix)
  pca.intervals.sal<-sym.pca(sym.data = interval.matrix,
                             method = "fixed",
                             fixed.matrix = weighted.center)

  df.hist<-limits.histogram.pca(sym.hist.matrix,pca.intervals.sal)

  num.components<-max(df.hist$Principal.Component)
  num.concepts<-dim(sym.hist.matrix)[1]

  df.disjoint.histogram<-limits.histogram.disjoint.pca.variable(df.hist,BIN.Matrix)

  df.disjoint.histogram.group <- df.disjoint.histogram %>% group_by(Principal.Component,Object.Name,Id,Coord.Min,Coord.Max)%>% summarise(Frequency = mean(Frequency))
  data.sym.df<-df.disjoint.histogram.group

  sym.hist.matrix.PCA<-sym.hist.matrix
  dim.hist<-dim(sym.hist.matrix.PCA)
  colnames(sym.hist.matrix.PCA)<-paste0("PC.",1:dim.hist[2])

  sym.hist.names<-row.names(sym.hist.matrix)
  sym.hist.cols<-colnames(sym.hist.matrix.PCA)
  row.names(sym.hist.matrix.PCA)<-sym.hist.names

  N<-dim.hist[1]
  M<-dim.hist[2]

  for(i in 1:N){
    HW.act<-sym.hist.names[i]
    data.HW.TMP<- df.disjoint.histogram.group[df.disjoint.histogram.group$Object.Name == HW.act,]
    for(j in 1:M){
      data.HW.TMP.2<- data.HW.TMP[data.HW.TMP$Principal.Component == j,]
      dim.tmp<-dim(data.HW.TMP.2)
      breaks.tmp<-c(data.HW.TMP.2$Coord.Min,data.HW.TMP.2$Coord.Max[dim.tmp[1]])
      sym.hist.matrix.PCA[[j]][[i]]$breaks<-round(as.numeric(breaks.tmp),2)
      sym.hist.matrix.PCA[[j]][[i]]$props<-as.numeric(data.HW.TMP.2$Frequency)
    }
  }
  row.names(sym.hist.matrix.PCA)<-sym.hist.names

  return(
    list ( classic.PCA = pca.intervals.sal$classic.PCA,
           sym.hist.matrix.PCA = sym.hist.matrix.PCA,
           Bins.df = data.sym.df
    )
  )
}
