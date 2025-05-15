#' quantiles.RSDA.KS
#' @author Jorge Arce Garro
#' @param histogram.matrix A matrix of histograms
#' @param num.quantiles Number of quantiles
#'
#' @return Quantiles of a Histogram Matrix
#' @export
#' @examples
#' \dontrun{
#' data("hardwoodBrito")
#' Hardwood.histogram<-hardwoodBrito
#' Hardwood.cols<-colnames(Hardwood.histogram)
#' Hardwood.names<-row.names(Hardwood.histogram)
#'  M<-length(Hardwood.cols)
#'  N<-length(Hardwood.names)
#'  BIN.Matrix<-matrix(rep(3,N*M),nrow = N)
#' pca.hist<-sym.histogram.pca(Hardwood.histogram,BIN.Matrix)
#' quantiles.RSDA.KS<-quantiles.RSDA(pca.hist$sym.hist.matrix.PCA,100)
#' }
quantiles.RSDA.KS<-function(histogram.matrix,num.quantiles){
  historam.quantiles<-histogram.matrix
  dim.quantiles<-dim(historam.quantiles)
  max.matrix<-rep(-Inf,dim.quantiles[2])
  min.matrix<- -max.matrix

  for(i in 1:dim.quantiles[1]){
    for(j in 1:dim.quantiles[2]){
        breaks.act<-histogram.matrix[[j]][[i]]$breaks
        min.act<-min(breaks.act)
        max.act<-max(breaks.act)
        min.matrix[j]<-min(c(min.act,min.matrix[j]))
        max.matrix[j]<-max(c(max.act,max.matrix[j]))
    }
  }

  for(i in 1:dim.quantiles[1]){
    for(j in 1:dim.quantiles[2]){
      historam.quantiles[[j]][[i]]<-calculate.quantils.RSDA.KS(histogram.matrix[[j]][[i]],
                                                               num.quantiles,
                                                               min.matrix[j],
                                                               max.matrix[j])
    }
  }
  row.names(historam.quantiles)<-row.names(histogram.matrix)
  return(historam.quantiles)
}
