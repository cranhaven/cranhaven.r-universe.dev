
#' Title plot.avrami
#' @description  template for plotting results from avrami function
#' @param out output from avrami function
#' @param skip plot symbols every nth points
#' @import colorRamps data.table graphics
#' @export
#'

plot_avrami <- function(out,skip=2)
				{
  rate<-x<-y<-color<-NULL

  #require("colorRamps")
  my.lib<-green2red(50)
				plot(0,0, xlim=c(0,max(out$xy$x)), ylim=c(0,1), xlab=expression(paste("t (minutes)")),ylab=expression(paste("X(t)")))

				out$xy$color=rep(seq(nrow(out$xy[,(.N), by=rate][,2])),unlist(out$xy[,(.N), by=rate][,2]))

				lapply(unique((out$xy$rate)), function(a) lines(out$xy[rate==a]$x,predict(out$mod[rate==a]$V1[[1]]),col=my.lib[out$xy$color]))

				out$xy[, points(x,y,pch = c(unique(color),rep(NA,unique(color)*skip)),cex=1.5), by=list(rate)]

				legend_text <- paste(as.character(unique((out$xy$rate))),"Celsius/min")
				Encoding(legend_text) <-  "bytes"
				legend("bottomright",legend_text,pch=floor(unique((out$xy$color))),bty = "n")
				}

#' Title plot lavrami
#' @description template for plotting results from avrami function
#' @param out  output from lavrami function
#' @param skip plot symbols every nth points
#' @import colorRamps data.table graphics
#' @export
#'

plot_lavrami <- function(out,skip=2)
				{
  rate<-x<-y<-color<-NULL
  #require("colorRamps")
  my.lib<-green2red(50)
				plot(0,0,type='n', xlim=c(min(out$xy$x),max(out$xy$x)), ylim=c(min(out$xy$y),max(out$xy$y)),xlab=expression(paste("log t")),ylab=expression(paste("log(-ln(1-X(t))")) )

				out$xy$color=rep(seq(nrow(out$xy[,(.N), by=rate][,2])),unlist(out$xy[,(.N), by=rate][,2]))

				lapply(unique((out$xy$rate)), function(a) lines(out$xy[rate==a]$x,predict(out$mod[rate==a]$V1[[1]]),col=my.lib[out$xy$color],lwd=3))
				out$xy[, points(x,y,pch = c(unique(color),rep(NA,unique(color)*skip)),cex=1.5), by=list(rate)]

				legend_text <- paste(as.character(unique((out$xy$rate))),"Celsius/min")
				Encoding(legend_text) <-  "bytes"
				legend("bottomright",legend_text,pch=floor(unique((out$xy$color))),bty = "n")

				}

#' Title plot.ozawa
#'
#' @description template for plotting results from avrami function
#' @param out from ozawa function
#' @import colorRamps data.table graphics
#' @export
#'
plot_ozawa <- function(out)
				{
  T.deg<-x<-y<-color<-NULL
  #require("colorRamps")
  my.lib<-green2red(50)
				plot(0,0,type='n', xlim=c(min(out$xy$x),max(out$xy$x)), ylim=c(min(out$xy$y),max(out$xy$y)),xlab=expression(paste("log",phi)),ylab=expression(paste("log(-ln(1-X(t))")) )


				out$xy$color=rep(seq(nrow(out$xy[,(.N), by=T.deg][,2])),unlist(out$xy[,(.N), by=T.deg][,2]))

				lapply(unique((out$xy$T.deg)), function(a) lines(out$xy[T.deg==a]$x,predict(out$mod[T.deg==a]$V1[[1]]),col=my.lib[out$xy$color],lwd=3))
				#lapply(unique((out$xy$T.deg)), function(a) lines(out$xy[T.deg==a]$x,predict(out$mod[T.deg==a]$V1[[1]]),col=hsv(0,1,1),lwd=3))
				out$xy[, points(x,y,pch = c(unique(color)),cex=1.5), by=list(T.deg)]
				legend_text <-paste(as.character(unique((out$xy$T.deg))),"Celsius")
				Encoding(legend_text) <-  "bytes"
				legend("bottomright",legend_text,pch=floor(unique((out$xy$color))),bty = "n")
				}

#' Title plot.mo
#'
#' @description template for plotting results from Mo function
#' @param out from mo function
#' @import colorRamps data.table graphics
#' @export
#'
plot_mo <- function(out)
				{
  rit<-x<-y<-color<-NULL
  #require("colorRamps")
  my.lib<-green2red(50)
				plot(0,0,type='n', xlim=c(min(out$xy$x),max(out$xy$x)), ylim=c(min(out$xy$y),max(out$xy$y)), xlab=expression(paste("log t ")),ylab=expression(paste("log",phi)) )
				out$xy$color=rep(seq(nrow(out$xy[,(.N), by=rit][,2])),unlist(out$xy[,(.N), by=rit][,2]))

				lapply(unique((out$xy$rit)), function(a) lines(out$xy[rit==a]$x,predict(out$mod[rit==a]$V1[[1]]),col=my.lib[out$xy$color],lwd=3))

				out$xy[, points(x,y,pch = c(unique(color)),cex=1.5), by=list(rit)]

				legend("bottomleft",paste(as.character(unique((out$xy$rit)*100)),"%"),pch=unique(out$xy$color),bty = "n")
				}


#' Title plot.fri
#'
#' @description template for plotting results from friedman function
#' @param out from friedman function
#' @import colorRamps data.table graphics
#' @export
#'

plot_fri <- function(out)
				{
  #require("colorRamps")

  rit<-x<-y<-color<-NULL

  my.lib<-green2red(50)
				plot(0,0,type='n', xlim=c(min(out$xy$x),max(out$xy$x)), ylim=c(min(out$xy$y),max(out$xy$y)),xlab="1/T (1000/K)",ylab=expression(paste("log (d",alpha,"/dt)")))

				out$xy$color=rep(seq(nrow(out$xy[,(.N), by=rit][,2])),unlist(out$xy[,(.N), by=rit][,2]))

				lapply(unique((out$xy$rit)), function(a) abline(out$mod[rit==a]$V1[[1]],col=my.lib[out$xy$color],lwd=3))

				out$xy[, points(x,y,pch = c(color),cex=1.5), by=list(rit)]

				legend("topright",paste(as.character(unique((out$xy$rit)*100)),"%"),pch=unique(out$xy$color),bty = "n", ncol=1)

}








