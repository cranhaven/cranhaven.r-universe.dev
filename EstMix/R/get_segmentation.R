#' calculate segmentation
#' @export
#' @keywords internal
get_segmentation <- function (BAF, LRR, chr, x, GT){

  p = 10^-4
  gt = (GT=='BB')*2+(GT=='AB')*1.5+(GT=='AA')-1;gt[gt==(-1)]=NA
  gaps = PSCBS::findLargeGaps(x=x,minLength=5e6,chromosome=chr)
  if(!is.null(gaps)) knownSegments = PSCBS::gapsToSegments(gaps)
  fit <- PSCBS::segmentByPairedPSCBS(CT=2*2^LRR,betaT=BAF,muN=gt,chrom=chr,knownSegments=knownSegments,tbn=FALSE,x=x,seed=1,alphaTCN=p*.9,alphaDH=p*.1)
  return (fit$output)

}
