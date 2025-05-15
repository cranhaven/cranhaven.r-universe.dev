calculate.quantils.RSDA.KS<-function(histogram.RSDA,num.quantils,min.act,max.act)
{
  breaks.percentils<- seq(from = min.act , to = max.act, by = (max.act-min.act)/num.quantils)
  N.percentils<-length(breaks.percentils)
  indx<-1:(N.percentils-1)
  props.calc<-rep(0,N.percentils-1)
  dim.Hist.act<-length(histogram.RSDA$props)
  for(i in indx){
    interval.act<- breaks.percentils[i:(i+1)]
    props.cum<-0
    for(j in 1:dim.Hist.act){
      interval.2<-histogram.RSDA$breaks[j:(j+1)]
      interception.act<-intersection.interval(interval.act,interval.2)
      props.cum<- props.cum + (interception.act[2]-interception.act[1])/(interval.2[2] - interval.2[1])*histogram.RSDA$props[j]
    }
    props.calc[i]<-props.cum
  }
  props.calc<- c(0,cumsum(props.calc))
  return(
    list(
      breaks = breaks.percentils,
      props = props.calc
    )
  )
}