
image_TempDepthProfiles <- function(x, main=NULL, xlab='Date', ylab="Depth (m)", 
                                    cb.xlab=expression(paste("Temperature (",degree,"C)")), cex.cb.xlab=1, cex.cb.ticks=1,
                                    xlim, ylim, zlim, pal="jet", only.months, month.line=0, mars, axes=TRUE, do.colorbar=TRUE,...){
  cmap <- NULL
  if(is.character(pal) & length(pal) == 1){
    data("cmap", package='oceanmap', envir = environment())
    pal <- cmap[[pal]]
  }
  # figure()
  if(missing(xlim)) xlim <- range(x$Date)
  if(missing(ylim)) ylim <- rev(range(x$Depth))
  z <- t(x$Temperature_matrix)
  if(missing(zlim)) zlim <- range(z,na.rm = T)
  zticks <- pretty(zlim)
  zlim <- range(zticks)
  z[z < zlim[1]] <- zlim[1]
  z[z > zlim[2]] <- zlim[2]
  
  if(missing(mars)){
    mars <- c(5,4,4,9)
  }
  par(xaxs='i', yaxs='i', mar=mars)
  image(x$Date, x$Depth, z, main=main, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, zlim=zlim, col=pal, axes=F)
  xlim_num <- as.numeric(xlim)
  if(missing(only.months)) only.months <- (xlim_num[2]-xlim_num[1]) > 93
  if(axes) .dates.axis(xlim, only.months = only.months, month.line=month.line,do_mid.ticks=T) 
  if(do.colorbar) oceanmap::set.colorbarp(cbxp = c(84,86), cby=c(12,90), pal = pal, zlim=zlim, cb.xlab=cb.xlab, cex.cb.xlab=cex.cb.xlab, cex.cb.ticks=cex.cb.ticks,...)
}



.dates.axis <- function(dates, only.months=T, do.yaxis=TRUE, force.months=F, month.line=NA,do_mid.ticks=TRUE){
  par(new=T, xaxs="i", yaxs="i")
  
  if(is.character(dates)| is.factor(dates)) dates <- .fact2Date(dates)
  if(is.numeric(dates)) dates <- .num2date(dates)
  if(length(dates) == 2) dates <- .num2date(dates[1]:dates[2])
  if(only.months){
    months <- unique(format(dates, "%Y-%m"))
    labels <- ticks <- pretty(dates, length(months))
    
    plot(dates, rep(1, length(dates)), axes=F, cex=0, xlim=par()$usr[1:2], ylim=par()$usr[3:4], xlab="", ylab="")
    if(do.yaxis) axis(2, las=1, lwd = 0, lwd.ticks = 1) 
    if(any(diff(ticks) > 29 & diff(ticks) < 32)| force.months) {
      ticks2 <- ticks+14
      ticks <- ticks[which(ticks %in% dates)]
      labels <- .num2month(ticks, abbrev=TRUE)
      axis(1, at=dates[which(dates %in% ticks[which(ticks %in% dates)])], labels=rep("", length(ticks)), lwd = 0, lwd.ticks=1)
      
      i <- which(dates %in% ticks2)
      axis(1, at=dates[i], labels=labels[1:length(i)], lwd = 0, lwd.ticks = 0, line=month.line)      
    }else{      
      axis(1, at=dates[which(dates %in% ticks)], labels=labels, lwd = 0, lwd.ticks = 0)
    }
  }else{    
    par(new=TRUE)
    num.dates <- as.numeric(dates)
    plot(dates, rep(1, length(dates)), axes=F, cex=0, xlim=par()$usr[1:2], ylim=par()$usr[3:4], xlab="", ylab="")
    xticks <- pretty(dates)
    axis(1, at=as.numeric(xticks)+.5*do_mid.ticks, labels=as.Date(xticks), lwd = 0, lwd.ticks = 1)
    axis(2, las=1, lwd = 0, lwd.ticks = 1)
  }
  box()
}
