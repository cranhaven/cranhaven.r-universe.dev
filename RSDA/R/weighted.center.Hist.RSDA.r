#' weighted.center.Hist.RSDA
#'
#' @author Jorge Arce Garro
#' @param sym.histogram A Histogram matrix
#' @author Jorge Arce Garro
#' @return Matrix of Weighted Centers
#' @export
#'
#' @examples
#' \dontrun{
#' data(hardwoodBrito)
#' weighted.center.Hist.RSDA(hardwoodBrito)
#' }
weighted.center.Hist.RSDA<-function(sym.histogram){
  dim.hist<-dim(sym.histogram)
  sym.cols<-colnames(sym.histogram)
  sym.names<-row.names(sym.histogram)
  N<-dim.hist[1]
  M<-dim.hist[2]
  weighted.center<-matrix(0,nrow = N , ncol = M)

  for(i in 1:N){
    for(j in 1:M){
      breaks.act<-sym.histogram[[j]][[i]]$breaks
      props.act<-sym.histogram[[j]][[i]]$props
      N.breaks<-length(breaks.act)
      min.props<-breaks.act[-N.breaks]*props.act
      max.props<-breaks.act[-1]*props.act
      weighted.center[i,j]<-sum(max.props+min.props)/2
    }
  }

  weighted.center<-as.data.frame(weighted.center)
  colnames(weighted.center)<-sym.cols
  row.names(weighted.center)<-sym.names

  return(weighted.center)
}
