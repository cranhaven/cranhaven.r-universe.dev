#' quantiles.RSDA
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
#' Hardwood.quantiles.PCA<-quantiles.RSDA(pca.hist$sym.hist.matrix.PCA,3)
#' }

quantiles.RSDA<-function(histogram.matrix,num.quantiles){
  historam.quantiles<-histogram.matrix
  dim.quantiles<-dim(historam.quantiles)
  for(i in 1:dim.quantiles[1]){
    for(j in 1:dim.quantiles[2]){
      historam.quantiles[[j]][[i]]<-calculate.quantils.RSDA(histogram.matrix[[j]][[i]],num.quantiles)
    }
  }
  row.names(historam.quantiles)<-row.names(histogram.matrix)
  return(historam.quantiles)
}
