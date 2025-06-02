
#' Title testMat
#'
#' @param a list of data tables of the checked thermograms using checkmat , obtained at different rates to change lines
#' @param l.lim left lim of running integral
#' @param r.lim  rigth lim of running integral
#' @param toselect vector
#'
#' @return data table ready to be used by all the methods for kinetic analysis included in the package
#' @export
#' @import data.table pracma
#' @examples
#' require(data.table)
#' npoints=1000
#' x=seq(1,npoints)
#' y=(dnorm(x, mean=npoints/2, sd=npoints/10))
#' x=seq(1,1000)
#' x2=seq(200,500,length.out=1000)
#' dat=data.frame(x,x2,y)
#' colnames(dat) <- c("time.seconds", "temperature.s","heat.flow")
#' dat=data.table(dat)
#' dat2=dat
#' dat$rates=20
#' dat2$rates=50
#' toTest=list(dat,dat2)
#' tested=testMat(toTest)



testMat<-function(a, l.lim=1, r.lim=NULL,toselect=c(0,1,2,0,0,0,3,4) ){

  heat.flow<-id<-rate<-time.minutes<-T.deg<-NULL
  if(is.null(r.lim)){r.lim<-NROW(a[[1]])}
  a.check<-lapply(seq(1,length(a)), function(x) checkmat(data.frame(a[[x]]),selected=toselect) )
  a.dt <-lapply(seq(1,length(a)), function(x) data.table(data.frame(a.check[[x]])))
  b<-rbindlist(a.dt)
  b$rate<-b$id
  a.peaks <- b[,list(res.list = list(findpeaks(heat.flow,sortstr=TRUE,npeaks=2))),by=id]
  a.peaks$rate<-a.peaks$id
  ref.peak=1
  a.peaks <- data.table(data.table(a.peaks$rate),rbindlist((lapply(a.peaks$res.list, function(x) data.table(t(x[ref.peak,]))))))
  colnames(a.peaks)<- c("rate","peak.value","ind.max","left.lim","right.lim")
  a.peaks.old <-a.peaks
  a.peaks$left.lim=l.lim
  a.peaks$right.lim=r.lim
  a.mat<- lapply(unique(b$rate),function(x) ri(b[b$rate==x]$time.seconds,b[b$rate==x]$heat.flow,a.peaks[rate==x]))
  a.mat <- lapply(a.mat, function(x) x$ds)
  ap<- data.table(b,rbindlist(a.mat))
  ap[,c("x","y"):=NULL]
  ap[,c("dadt"):=dadx(time.minutes,ri),by=list(rate)]
  ap$id_cycle=ap$id
  ap$time.minutes.zero<-ap$time.minutes
  ap$time.seconds.zero<-ap$time.seconds
  ap$y.loess=ap$heat.flow
  return(ap)
}
