#' Title running integral
#' @description calculate the running integral for the selected peak
#' @param x x axis for the intergration
#' @param y y axis for the intergration
#' @param pks selected peak
#' @param TAP if TRUE will apply a baseline using tangent area proportional (default=FALSE)
#' @param linear if TRUE  will apply a linear baseline (default=FALSE)
#' @param ... parameters in TAPPA function
#' @return \itemize{
#' \item ds data frame containing original x and y given as input
#' \item ri running integral
#' \item b.tap baseline calculate if the switch TAP is TRUE
#' \item y.tap = y - b.tap
#' }
#' @export
#'
#' @examples \donttest{
#' #' require(data.table)
#' require(MASS)
#' rates=c(0.5,1,2,5,10,20,50)
#' a<-lapply(rates, function(x) JMA(A=exp(35),Ea=120000,T0=0,T.end=300,q=x,npoints=5000,n=2))
#' a<-lapply(seq(1,length(a)), function(x) data.table(a[[x]]$time.s,a[[x]]$T.C,
#' a[[x]]$dadT, rates[[x]]))
#' lapply(seq(1,length(a)), function(x) setnames(a[[x]],
#' c("time.seconds","temperature.s","heat.flow","rates") ) )
#' a.dt <-lapply(seq(1,length(a)), function(x) data.table(data.frame(a.check[[x]])))
#' a<-rbindlist(a.dt)
#' a$rate<-a$id
#' a.peaks <- a[,.(res.list = list(findpeaks(heat.flow,sortstr=TRUE,npeaks=2))),by=id]
#' a.peaks$rate<-a.peaks$id
#' ref.peak=1
#' a.peaks <- data.table(data.table(a.peaks$rate),rbindlist((lapply(a.peaks$res.list,
#' function(x) data.table(t(x[ref.peak,]))))))
#' colnames(a.peaks)<- c("rate","peak.value","ind.max","left.lim","right.lim")
#' a.mat<- lapply(unique(a$rate),function(x)
#' ri(a[a$rate==x]$time.seconds,a[a$rate==x]$heat.flow,a.peaks[rate==x]))
#'}

ri <- function(x, y, pks, TAP = FALSE, linear=FALSE, ...) {

  ds <- data.frame(x,y)
  ds <- ds[complete.cases(ds), ]
  ds <- data.table(ds)

  ds[,"area":= 0]
  ds[,"ri" := 0]
  ds[,"b.TAP" := 0]
  ds[,"y.TAP":= 0]


  if (!is.null(pks))
  {

    left.lim <- as.numeric(pks[,4])
    right.lim <- as.numeric(pks[,5])
    med.peak <- as.numeric(pks[,3])

    xt <- ds[left.lim:right.lim,]$x
    yt <- ds[left.lim:right.lim,]$y


    y.TAP=yt

    ds[,"b.TAP"][left.lim:right.lim] <- 0

    if (TAP == TRUE)
    {
      print("TAP baseline active")
      b.TAP <- TAPPA(xt, yt,...)
      y.TAP <- yt - b.TAP
      ds[,"b.TAP"][left.lim:right.lim] <-b.TAP
    }


    if (linear == TRUE)
    {
      print("linear baseline active")
      p <- length(x)
      x.reg=c(x[left.lim],x[right.lim])
      y.reg=c(y[left.lim],y[right.lim])
      df=data.frame(x.reg,y.reg)
      mod=lm(y.reg~x.reg,data=df)
      b.TAP <- xt*mod$coeff[2]+mod$coeff[1]
      y.TAP <- yt - b.TAP
      y.TAP[which(yt-b.TAP<0)] <- b.TAP[which(yt-b.TAP<0)]

      ds[,"b.TAP"][left.lim:right.lim] <-b.TAP

    }


    dat <- data.frame(xt, y.TAP)
    dat <- dat[complete.cases(dat), ]

    ds[,"ri"][left.lim:right.lim]<-unlist((runningIntegral(dat$xt, dat$y.TAP))$r.i)
    ds[,"area"][left.lim:right.lim]<-unlist((runningIntegral(dat$xt, dat$y.TAP))$area)

    ds[,"y.TAP"][left.lim:right.lim] <-y.TAP



  }

  mylist <- list("ds"=ds, "ri"=ds$ri, "b.TAP"=ds$b.TAP, "y.TAP"=y.TAP, "area"=ds$area)
}

