#' HistRSDAToEcdf
#'
#' @param h A matrix of histograms
#' @author Jorge Arce Garro
#' @importFrom stats approxfun
#' @return Transformation in Ecdf object
#' @export
#'
#' @examples
#' \dontrun{
#'  data("hardwoodBrito")
#'  Hardwood.histogram<-hardwoodBrito
#'  Hardwood.cols<-colnames(Hardwood.histogram)
#'  Hardwood.names<-row.names(Hardwood.histogram)
#'  M<-length(Hardwood.cols)
#'  N<-length(Hardwood.names)
#'  BIN.Matrix<-matrix(rep(3,N*M),nrow = N)
#'  pca.hist<-sym.histogram.pca(Hardwood.histogram,BIN.Matrix)
#'  Hardwood.quantiles.PCA.2<-quantiles.RSDA.KS(pca.hist$sym.hist.matrix.PCA,100)
#'  h<-Hardwood.quantiles.PCA.2[[1]][[1]]
#'  HistRSDAToEcdf(h)
#' }
HistRSDAToEcdf<-function(h)
{
  method = "constant"
  f = 0
  inverse = FALSE
  n<-length(h$breaks)
  x.vals <- h$breaks
  y.vals <- h$props
  if (inverse) {
    vals.tmp <- x.vals
    x.vals <- y.vals
    y.vals <- vals.tmp
  }
  rval <- approxfun(x.vals, y.vals, method = method, yleft = 0,
                    yright = 1, f = f, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}
