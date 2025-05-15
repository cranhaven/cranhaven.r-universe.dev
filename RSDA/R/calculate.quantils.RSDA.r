#' quantiles.RSDA
#'
#' @param histogram.RSDA A histogram
#' @param num.quantils Number of quantiles
#'
#' @return Quantiles of a Histogram
#' @export
#'
calculate.quantils.RSDA<-function(histogram.RSDA,num.quantils)
{
  props.cum<-c(0,cumsum(histogram.RSDA$props))
  percentils.RSDA<-seq(0,1,1/num.quantils)
  breaks.percentils<-histogram.RSDA$breaks[1]
  N.percentils<-length(percentils.RSDA)
  indx<-2:(N.percentils)
  for(i in indx){
    indx.min<-min(which(props.cum >= percentils.RSDA[i]))
    percentils.act<-percentils.RSDA[i] - props.cum[indx.min-1]
    break.p<-percentils.act*(histogram.RSDA$breaks[indx.min] - histogram.RSDA$breaks[indx.min-1])/histogram.RSDA$props[indx.min-1]
    break.p<-break.p+histogram.RSDA$breaks[indx.min-1]
    breaks.percentils<-c(breaks.percentils,break.p)
  }
  return(
    list(
      breaks = breaks.percentils,
      props = percentils.RSDA
    )
  )
}
