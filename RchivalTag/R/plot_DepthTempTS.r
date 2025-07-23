

plot_DepthTempTS <- function(ts_df, y="Depth", z="Temperature", xlim, ylim, zlim, show.colorbar=TRUE, 
                             pal="jet", cb.xlab, cb.xlab.line=0, pt.lwd, do_interp=TRUE, Return=FALSE, mars, tz="UTC",...){
  z0 <- z
  y0 <- y
  if(missing(mars)) mars <- c(5,4,4,5)
  par(mar=mars)
  a <- plot_DepthTS(ts_df, xlim = xlim, ylim = ylim, Return = TRUE, tz=tz,...)
  cmap <- NULL
  # usethis::use_data("cmap", package='oceanmap', overwrite = TRUE)
  data("cmap", package='oceanmap', envir = environment())
  cpal <- cmap[[pal]]
  z <- a[[z0]]
  zstep <- 0.1
  if(missing(cb.xlab)) cb.xlab <- expression("Temperature ("*degree*C*")")
  if(missing(zlim)) zlim <- range(pretty(z),na.rm=TRUE)
  add <- data.frame(z=seq(zlim[1],zlim[2],by=zstep))
  add$z <- round(add$z,1)
  add <- unique(add)
  add$col <- colorRampPalette(cpal)(nrow(add))
  
  if(!do_interp){
    if(missing(pt.lwd)) pt.lwd <- .7
    out <- merge(a,add,by.x=z0,by.y="z",all.x=TRUE)
    points(out$datetime,out[[y0]],col=out$col,pch=19,cex=pt.lwd)
  }else{
    tstep <- abs(pracma::Mode(as.numeric(a$datetime))) 
    if(tstep > 100) warning("Consider running plot_DepthTempTS_resampled or plot_DepthTempTS_resampled_PDT, which is more accurate for low resolution and transmitted time series data!")
    if(missing(pt.lwd)) pt.lwd <- .1
    x <- a$datetime
    y <- a[[y0]]
    z <- a[[z0]]
    input <- data.frame(x=x,y=y,z=z)
    ii <- which(!is.na(input$y) & !is.na(input$z))
    for(j in 2:length(ii)){
      if(ii[j-1] == ii[j]-1){
        # j <- 2
        yin <- c(input$y[ii[j-1]],input$y[ii[j]])
        zin <- c(input$z[ii[j-1]],input$z[ii[j]])
        if(yin[1]>yin[2]) ystep <- -.5
        if(yin[1]<=yin[2]) ystep <- .5
        yout <- seq(yin[1],yin[2],by = ystep)
        # if(any(diff(yout)>0.5)) stop()
        if(length(yout) >1) {
          intp <- approx(x = yin, xout = yout, y = zin)
          out <- merge(data.frame(x=seq(x[ii[j-1]],x[ii[j]],length.out = length(yout)),y=intp$x,z=round(intp$y,1)),add,by="z",all.x=T)
          head(out)
        }else{
          out <- merge(data.frame(x=c(x[ii[j-1]],x[ii[j]]),y=yin,z=zin),add,by="z",all.x=T)
        }
        points(as.numeric(out$x),out$y,col=out$col,pch=19,cex=pt.lwd)
        # u <- readline()
        # if(u == "s") stop()
        # lines(as.numeric(out$x),out$y,col=out$col)
      }
    }
  }
  # par(new=T)
  # oceanmap::empty.plot()
  # oceanmap::set.colorbar(cbpos = "r",pal = pal,zlim = zlim, cb.xlab = cb.xlab,cb.xlab.line=cb.xlab.line)
  if(show.colorbar) oceanmap::set.colorbarp(cbxp = c(90,92),cbyp = c(20,84),total.reg = F,pal = pal,
                                            zlim = zlim, cb.xlab=cb.xlab, cb.xlab.line=cb.xlab.line)
  
  if(Return) return(a)
}






plot_DepthTempTS_resampled_PDT <- function(ts_df, PDT, y="Depth", z="Temperature", xlim, ylim, zlim, show.colorbar=TRUE, 
                                           pal="jet", cb.xlab, cb.xlab.line=0, pt.lwd, do_interp=TRUE, Return=FALSE, mars, tz="UTC",...){
  
  if(!missing(xlim)){
    xlim0 <- xlim
    if(class(xlim)[1] == 'Date' | nchar(as.character(xlim[1])) == 10){
      xlim <- as.Date(xlim,tz=tz)
      main_xlim <- xlim
      if(length(xlim) == 1) xlim <- c(xlim, xlim)
      xlim[2] <- xlim[2]+1
      xlim <- paste(xlim, '00:00:00')
    }
    xlim <- .fact2datetime(xlim,tz = tz)
    if(length(xlim) == 1) xlim <- c(xlim, xlim[1]+24*60*60)
    if(length(xlim) > 2) xlim <- range(xlim)
    
    if(tz != "UTC"){
      dat_list <- list(ts_df,PDT)
      for(ds in 1:2){
        dset <- dat_list[[ds]]
        is.POSIXct <- function(x) inherits(x, "POSIXct")
        fields <- names(dset)[sapply(dset, is.POSIXct)]
        for(field in fields){
          LocalTime <- .fact2datetime(dset[[field]]) %>% lubridate::with_tz(tzone=tz)
          # LocalTime <- LocalTime - (as.numeric(as.Date(LocalTime,tz=tz)-as.Date(dset$datetime,tz = tz)))*24*60*60
          dset[[field]] <- LocalTime #RchivalTag:::.fact2datetime(as.character(LocalTime))
        }
        dset$date <- as.Date(dset$datetime,tz = tz)
        dat_list[[ds]] <- dset
      }
      ts_df <- dat_list[[1]]
      PDT <- dat_list[[2]]
    }
    
    ts_df <- ts_df[which(ts_df$datetime >= xlim[1] & ts_df$datetime < xlim[2]),]
    if(do_interp | tz == "UTC") PDT <- PDT[which(PDT$datetime >= xlim[1] & PDT$datetime < xlim[2]),]
    
    # reconvert to UTC
    if(tz != "UTC"){
      dat_list <- list(ts_df,PDT)
      for(ds in 1:2){
        dset <- dat_list[[ds]]
        fields <- names(dset)[sapply(dset, is.POSIXct)]
        for(field in fields){
          LocalTime <- .fact2datetime(dset[[field]]) %>% lubridate::with_tz(tzone=tz)
          # LocalTime <- LocalTime - (as.numeric(as.Date(LocalTime,tz="UTC")-as.Date(dset$datetime,tz = "UTC")))*24*60*60
          dset[[field]] <- LocalTime#RchivalTag:::.fact2datetime(as.character(LocalTime))
        }
        dset$date <- as.Date(dset$datetime,tz = "UTC")
        dat_list[[ds]] <- dset
      }
      ts_df <- dat_list[[1]]
      if(!do_interp) {
        PDT <- dat_list[[2]]
        PDT <- PDT[which(PDT$date %in% ts_df$date),]
      }
    }
    xlim <- xlim0
  }
  
  if(!do_interp){
    ts_df <- resample_PDT(ts_df, PDT)
    if(missing(pt.lwd)) pt.lwd <- .7
    a <- plot_DepthTempTS(ts_df,do_interp=F,y=y, z=z, xlim=xlim, ylim=ylim, zlim=zlim, pal=pal, 
                          cb.xlab=cb.xlab, cb.xlab.line=cb.xlab.line, pt.lwd=pt.lwd, Return=Return,tz=tz,...)
  }else{ ## requires PDT input in tz
    m <- interpolate_PDTs(PDT,verbose = F)
    M <- m$station.1$Temperature_matrix
    
    if(missing(mars)) mars <- c(5,4,4,5)
    par(mar=mars)
    a <- plot_DepthTS(ts_df, xlim = xlim, ylim = ylim, Return = TRUE, tz=tz,...)
    cmap <- NULL
    # usethis::use_data("cmap", package='oceanmap', overwrite = TRUE)
    data("cmap", package='oceanmap', envir = environment())
    cpal <- cmap[[pal]]
    if(missing(cb.xlab)) cb.xlab <- expression("Temperature ("*degree*C*")")
    
    if(missing(zlim)) zlim <- range(pretty(as.vector(M)),na.rm=TRUE)
    zstep <- 0.1
    add <- data.frame(z=seq(zlim[1],zlim[2],by=zstep))
    add$z <- round(add$z,1)
    add <- unique(add)
    add$col <- colorRampPalette(cpal)(nrow(add))
    
    if(missing(pt.lwd)) pt.lwd <- .1
    x <- a$datetime
    y0 <- y
    y <- a[[y0]]
    # z <- a[[z0]]
    input <- data.frame(x=x,y=y)
    ii <- which(!is.na(input$y))
    for(j in 2:length(ii)){ ## go through all records of y
      if(ii[j-1] == ii[j]-1){
        # j <- 2
        yin <- c(input$y[ii[j-1]],input$y[ii[j]])
        if(yin[1]>yin[2]) ystep <- -.5
        if(yin[1]<=yin[2]) ystep <- .5
        yout <- seq(yin[1],yin[2],by = ystep)
        xout <- seq(x[ii[j-1]],x[ii[j]],length.out=length(yout))
        # if(any(diff(yout)>0.5)) stop()
        
        zout <- rep(NA,length(yout))
        for(n in 1:length(yout)){
          A <- which(m$station.1$Date == a$date[j])
          B <- which(m$station.1$Depth == yout[n])
          zout[n] <- round(M[B,A],1)
        }
        out <- merge(data.frame(x=xout,y=yout,z=zout),add,by="z",all.x=T)
        
        points(as.numeric(out$x),out$y,col=out$col,pch=19,cex=pt.lwd)
        # u <- readline()
        # if(u == "s") stop()
        # lines(as.numeric(out$x),out$y,col=out$col)
      }
    }
    # par(new=T)
    # oceanmap::empty.plot()
    # oceanmap::set.colorbar(cbpos = "r",pal = pal,zlim = zlim, cb.xlab = cb.xlab,cb.xlab.line=cb.xlab.line)
    if(show.colorbar) oceanmap::set.colorbarp(cbxp = c(90,92),cbyp = c(20,84),total.reg = F,pal = pal,
                                              zlim = zlim, cb.xlab=cb.xlab, cb.xlab.line=cb.xlab.line)
  }
  if(Return) return(a)
}



plot_DepthTempTS_resampled <- function(ts_df, y="Depth", z="Temperature", bin_res=10, xlim, ylim, zlim, show.colorbar=TRUE, 
                                       pal="jet", cb.xlab, cb.xlab.line=0, pt.lwd, do_interp=TRUE, Return=FALSE, mars, tz="UTC",...){
  
  if(!missing(xlim)){
    xlim0 <- xlim
    if(tz != "UTC"){
      is.POSIXct <- function(x) inherits(x, "POSIXct")
      fields <- names(ts_df)[sapply(ts_df, is.POSIXct)]
      for(field in fields){
        LocalTime <- .fact2datetime(ts_df[[field]]) %>% lubridate::with_tz(tzone=tz)
        # LocalTime <- LocalTime - (as.numeric(as.Date(LocalTime,tz=tz)-as.Date(ts_df$datetime,tz = tz)))*24*60*60
        ts_df[[field]] <- LocalTime#RchivalTag:::.fact2datetime(as.character(LocalTime))
      }
      ts_df$date <- as.Date(ts_df$datetime,tz = tz)
    }
    
    if(class(xlim)[1] == 'Date' | nchar(as.character(xlim[1])) == 10){
      xlim <- as.Date(xlim,tz=tz)
      main_xlim <- xlim
      if(length(xlim) == 1) xlim <- c(xlim, xlim)
      xlim[2] <- xlim[2]+1
      xlim <- paste(xlim, '00:00:00')
    }
    
    xlim <- .fact2datetime(xlim,tz = tz)
    if(length(xlim) == 1) xlim <- c(xlim, xlim[1]+24*60*60)
    if(length(xlim) > 2) xlim <- range(xlim)
    ts_df <- ts_df[which(ts_df$datetime >= xlim[1] & ts_df$datetime <= xlim[2]),]
    
    # reconvert to UTC
    if(tz != "UTC"){
      is.POSIXct <- function(x) inherits(x, "POSIXct")
      fields <- names(ts_df)[sapply(ts_df, is.POSIXct)]
      for(field in fields){
        LocalTime <- .fact2datetime(ts_df[[field]]) %>% lubridate::with_tz(tzone=tz)
        # LocalTime <- LocalTime - (as.numeric(as.Date(LocalTime,tz="UTC")-as.Date(ts_df$datetime,tz = "UTC")))*24*60*60
        ts_df[[field]] <- LocalTime#RchivalTag:::.fact2datetime(as.character(LocalTime))
      }
      ts_df$date <- as.Date(ts_df$datetime,tz = "UTC")
    }
    xlim <- xlim0
  }
  
  
  
  if(!do_interp) {
    ts_df <- resample_DepthTempTS(ts_df)
    if(missing(pt.lwd)) pt.lwd <- .7
    a <- plot_DepthTempTS(ts_df,do_interp=F,y=y, z=z, xlim=xlim, ylim=ylim, zlim=zlim, pal=pal, 
                          cb.xlab=cb.xlab, cb.xlab.line=cb.xlab.line, pt.lwd=pt.lwd, Return=Return,tz=tz,...)
  }else{
    DepthTempTS_binned <- bin_TempTS(ts_df,res=bin_res)
    m <- interpolate_PDTs(DepthTempTS_binned,verbose = F)
    M <- m$station.1$Temperature_matrix
    if(missing(pt.lwd)) pt.lwd <- .1
    
    y0 <- y
    if(missing(mars)) mars <- c(5,4,4,5)
    par(mar=mars)
    a <- plot_DepthTS(ts_df, xlim = xlim, ylim = ylim, Return = TRUE, tz=tz,...)
    cmap <- NULL
    # usethis::use_data("cmap", package='oceanmap', overwrite = TRUE)
    data("cmap", package='oceanmap', envir = environment())
    cpal <- cmap[[pal]]
    if(missing(cb.xlab)) cb.xlab <- expression("Temperature ("*degree*C*")")
    
    if(missing(zlim)) zlim <- range(pretty(as.vector(M)),na.rm=TRUE)
    zstep <- 0.1
    add <- data.frame(z=seq(zlim[1],zlim[2],by=zstep))
    add$z <- round(add$z,1)
    add <- unique(add)
    add$col <- colorRampPalette(cpal)(nrow(add))
    
    if(missing(pt.lwd)) pt.lwd <- .1
    x <- a$datetime
    y <- a[[y0]]
    # z <- a[[z0]]
    input <- data.frame(x=x,y=y)
    ii <- which(!is.na(input$y))
    for(j in 2:length(ii)){ ## go through all records of y
      if(ii[j-1] == ii[j]-1){
        # j <- 2
        yin <- c(input$y[ii[j-1]],input$y[ii[j]])
        if(yin[1]>yin[2]) ystep <- -.5
        if(yin[1]<=yin[2]) ystep <- .5
        yout <- seq(yin[1],yin[2],by = ystep)
        xout <- seq(x[ii[j-1]],x[ii[j]],length.out=length(yout))
        # if(any(diff(yout)>0.5)) stop()
        
        zout <- rep(NA,length(yout))
        for(n in 1:length(yout)){
          A <- which(m$station.1$Date == a$date[j])
          B <- which(m$station.1$Depth == yout[n])
          
          if(length(A) > 0 & length(B) > 0 ) zout[n] <- round(M[B,A],1)
        }
        out <- merge(data.frame(x=xout,y=yout,z=zout),add,by="z",all.x=T)
        
        points(as.numeric(out$x),out$y,col=out$col,pch=19,cex=pt.lwd)
        # u <- readline()
        # if(u == "s") stop()
        # lines(as.numeric(out$x),out$y,col=out$col)
        head(out)
      }
    }
    # par(new=T)
    # oceanmap::empty.plot()
    # oceanmap::set.colorbar(cbpos = "r",pal = pal,zlim = zlim, cb.xlab = cb.xlab,cb.xlab.line=cb.xlab.line)
    if(show.colorbar) oceanmap::set.colorbarp(cbxp = c(90,92),cbyp = c(20,84),total.reg = F,pal = pal,
                                              zlim = zlim, cb.xlab=cb.xlab, cb.xlab.line=cb.xlab.line)
    
  }
  if(Return) return(a)
}
