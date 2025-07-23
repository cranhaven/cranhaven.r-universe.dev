
empty.plot_TS <- function(xlim, ylim, xticks_interval, ylab="", xlab, main="",
                          cex=1, cex.main=1.2*cex, cex.lab=1*cex, cex.axis=.9*cex, cex.axis2=1*cex, 
                          las=1, xaxs="i", yaxs="i", do_xaxis=TRUE, do_yaxis=TRUE,
                          plot_box=TRUE, bty="l", tz="UTC",...){
  if(missing(xlab)) xlab <- paste0("Time (",tz,")")
  
  if(class(xlim)[1] == 'Date' | nchar(as.character(xlim[1])) == 10){
    xlim <- as.Date(xlim,tz=tz)
    if(length(xlim) == 1) xlim <- c(xlim, xlim)
    xlim[2] <- xlim[2]+1
    xlim <- paste(xlim, '00:00:00')
  }
  xlim <- .fact2datetime(xlim,tz = tz)
  if(length(xlim) > 2) xlim <- range(xlim)
  if(length(xlim) == 1) xlim <- c(xlim, xlim[1]+24*60*60)
  
  date.range.l <- .num2datetime(as.numeric(xlim, tz = tz, hours.offset = 0))
  date.range <- as.Date(date.range.l,tz=tz)
  n <- length(date.range.l)
  xticks <- seq(date.range.l[1], date.range.l[n], by=1)#21600)
  days <- unique(as.Date(xticks,tz=tz))
  if(missing(xticks_interval)){
    xticks_interval <- 6
    if((as.numeric(xlim[2])-as.numeric(xlim[1]))/(24*60*60) <= 1) xticks_interval <- 3
  }
  xticks <- xticks[which(.datetime2hour(xticks,tz=tz)%%xticks_interval == 0 & .datetime2min.dc(xticks,tz=tz) == 0)]
  
  #   print(.datetime2hour(xticks))
  xtick.labels <- format(xticks, "%H")
  date.range.ll <- date.range.l
  
  xti <- which(xticks >= date.range.ll[1] & xticks <=date.range.ll[2])
  
  date.ticks <- xticks[which(xtick.labels == "12" & xticks >= date.range.ll[1])]
  ###' not correct midday:
  #   if(show.sun){
  #     si <- which(.datetime2hour.dc(xticks[xti]) == 12)
  #     for(sii in si) text(xticks[xti][sii], y=max(ylim)-10, labels = "\U25d7", srt="90", cex=6, col=colors()[143], xpd=F)
  #   }
  par(las=las, yaxs=yaxs, xaxs=xaxs, ...)
  plot(0, 1, axes=FALSE, xlab="", ylab=ylab, ylim=ylim, xlim=xlim, cex.lab=cex.lab,...)
  if(do_xaxis) axis(1, at=xticks[xti], labels=xtick.labels[xti], xpd=TRUE, pos=ylim[1], cex.axis=cex.axis, lwd=0, lwd.ticks = 1)
  if(do_xaxis) axis(1, at=date.ticks, labels=format(date.ticks, "%Y-%m-%d"), lwd=0, line=1, cex.axis=cex.axis2)
  if(do_yaxis) axis(2, lwd = 0, lwd.ticks=1)
  if(!missing(main)) title(main = main,cex.main=cex.main)
  if(plot_box) box(bty=bty)
}



plot_DepthTS <- plot_TS <- function(ts_df, y="Depth", xlim, ylim, xticks_interval,
                                    ylab=y, xlab, main, main.line=1, plot_info=TRUE, 
                                    ID, ID_label="Serial", #show.temp=F,
                                    plot_DayTimePeriods=FALSE, twilight.set="ast", 
                                    cex=1, cex.main=1.2*cex, cex.lab=1*cex, cex.axis=.9*cex, cex.axis2=1*cex, 
                                    type="l", las=1, xaxs="i", yaxs="i", plot_box=TRUE, bty="l", Return=FALSE, 
                                    tz="UTC", ...){
  if(missing(xlab)) xlab <- paste0("Time (",tz,")")
  if(missing(ID) & is.null(ts_df[[ID_label]])) {
    k <- c("DeployID","Ptt") %in% names(ts_df)
    if(any(k)) {
      ID_label <- c("DeployID","Ptt")[which(k)[1]]
      warning(paste0("ID_label 'Serial' not found! Resetting ID_label to '",ID_label,"'!"))
    }
    if(!all(k)) warning(paste0("no column corresponding to ID_label '",ID_label,"' found!"))
  }
  
  if(missing(ID)) ID <- unique(ts_df[[ID_label]])
  ID_main <- ""
  if(!is.null(ID)) ID_main <- paste("Tag ID", ID, "-")
  
  if(length(ID) > 1) {
    warning("multiple tags in data set: ", paste(ID, collapse=', '))
    main <- ''
  }
  
  if(!("datetime" %in% names(ts_df))) stop('no "datetime" vector provided! please revise.')
  
  ## check and fill potential data gaps:
  datetime_nm <- as.numeric(.fact2datetime(ts_df$datetime,tz="UTC"))
  tstep <- pracma::Mode(diff(datetime_nm))
  if(tstep == 0) stop("time step between first valid 'y'-records is 0. please revise!")
  add0 <- data.frame(datetime_nm=seq(datetime_nm[1],tail(datetime_nm, 1), by=tstep))
  add0$datetime <- .num2datetime(add0$datetime_nm,tz="UTC",hours.offset = 0)
  datetime_nm <- add0$datetime_nm <- c()
  ts_df$datetime <- as.character(ts_df$datetime); add0$datetime <- as.character(add0$datetime)
  ts_df <- merge(ts_df, add0, by="datetime",all=T)
  ts_df$datetime <- .fact2datetime(ts_df$datetime, date_format = "%Y-%m-%d %H:%M:%S",tz = "UTC")
  ts_df$date <- as.Date(ts_df$datetime)

  if(plot_DayTimePeriods){
    if('Lon' %in% names(ts_df) & 'Lat' %in% names(ts_df)){
      if(any(is.na(ts_df$Lon))){
        pos <- unique(ts_df[,c('date','Lon','Lat')])
        pos$Lon <- spline(1:nrow(pos), y = pos$Lon, xout = 1:nrow(pos))$y
        pos$Lat <- spline(1:nrow(pos), y = pos$Lat, xout = 1:nrow(pos))$y
        ts_df$Lon <- ts_df$Lat <- c() #ts_df$sunrise <- ts_df$sunset <- c()
        ts_df <- merge(ts_df, pos, by='date', all=TRUE)
      }
    }  
    dawn.set <- paste0('dawn.', twilight.set)
    dusk.set <- paste0('dusk.', twilight.set)
    if(!(all(c(dawn.set, dusk.set, 'sunrise','sunset') %in% names(ts_df)) | all(c('datetime','Lon','Lat') %in% names(ts_df)))){
      warning("input data does not include day period information. Hence setting plot_DayTimePeriods to FALSE. \nPlease add Lon/Lat information to depth time series and consider calling function 'get_DayTimeLimits' before running 'plot_TS' to increase performance speed.")
      plot_DayTimePeriods <- F
      }else{
      if(!all(c(dawn.set, dusk.set, 'sunrise','sunset') %in% names(ts_df))){
        warning("not all required day period information found. calling function: 'get_DayTimeLimits'. 
            Consider to run this function before calling 'plot_TS' to increase performance speed.")
        ts_df <- get_DayTimeLimits(ts_df) # get sunrise, sunset and dusk/dawn information
      }
    }
  }
  ## convert to preferred tz
  if(tz != "UTC"){
    is.POSIXct <- function(x) inherits(x, "POSIXct")
    fields <- names(ts_df)[sapply(ts_df, is.POSIXct)]
    for(field in fields){
      LocalTime <- .fact2datetime(ts_df[[field]]) %>% lubridate::with_tz(tzone=tz)
      # LocalTime <- ts_df[[field]] %>% lubridate::ymd_hms(tz="UTC") %>% lubridate::with_tz(tzone=tz)
      # LocalTime <- LocalTime - (as.numeric(as.Date(LocalTime,tz=tz)-as.Date(ts_df$datetime,tz = tz)))*24*60*60
      ts_df[[field]] <- LocalTime#RchivalTag:::.fact2datetime(as.character(LocalTime))
    }
    ts_df$date <- as.Date(ts_df$datetime,tz = tz)
  }
  
  if(!missing(xlim)){
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
  }else{
    xlim <- range(ts_df$datetime)
    xlim <- .fact2datetime(xlim,tz = tz)
    main_xlim <- xlim
  }
  
  if(is.null(ts_df[[y]]) & nrow(ts_df) > 0) stop(paste0("y-column '",y, "' not found, please revise!"))
  if(!missing(ylim)){
    if(any(ts_df[[y]] > max(ylim) | ts_df[[y]] < min(ylim),na.rm=T)){
      ts_df[[y]][which(ts_df[[y]] > max(ylim))] <- max(ylim)
      ts_df[[y]][which(ts_df[[y]] < min(ylim))] <- min(ylim)
      warning(paste("Cutting records outside of ylim-range"))
    }
  }else{
    ylim <- range(ts_df[[y]],na.rm=TRUE,warn=FALSE)
    if(all(ylim == 0)) ylim <- NA
  }
  
  if(nrow(ts_df) == 0 & !all(is.finite(ylim))){
    stop(paste0("no data within xlim-range (",paste(xlim,collapse = ":"),")"))
  }
  
  if(y == "Depth" & max(abs(ylim) == max(ylim))) ylim <- rev(range(c(0,max(ylim))))
  
  if(nrow(ts_df) == 0 & all(is.finite(ylim))){
    warning(paste0("no data within xlim-range (",paste(xlim,collapse = ":"),")"))
    empty.plot_TS(xlim,ylim)
  }else{
    
    ### fill potential data gaps
    if(!("date" %in% names(ts_df))) {
      warning('"date" vector missing and derived from provided "datetime" vector!')
      ts_df$date <- as.Date(ts_df$datetime,tz=tz)
    }
    
    ### plot emptyTS plot:
    par(las=las, yaxs=yaxs, xaxs=xaxs,...)  
    plot(ts_df$datetime, ts_df[[y]], axes=FALSE, lwd=0, cex=1, xlab="", ylab="", xlim=xlim, ylim=ylim, ...)
    
    xticks <- seq(xlim[1], xlim[2], by=1)#21600)
    days <- unique(as.Date(xticks,tz=tz))
    if(missing(xticks_interval)){
      xticks_interval <- 6
      if((as.numeric(xlim[2])-as.numeric(xlim[1]))/(24*60*60) <= 1) xticks_interval <- 3
    }
    xticks <- xticks[which(.datetime2hour(xticks,tz = tz)%%xticks_interval == 0 & .datetime2min.dc(xticks,tz = tz) == 0)]
    xtick.labels <- format(xticks, "%H")
    
    #### plot daytime periods:
    if(plot_DayTimePeriods){
      
      ts_df$dawn <- ts_df[[dawn.set]]
      ts_df$dusk <- ts_df[[dusk.set]]
      
      rect(xlim[1], ylim[1], xlim[2], ylim[2], col="grey", lwd=0)
      sunrise <- sunset <- dawn <- dusk <- NA
      for(d in days){
        k <- ts_df[which(ts_df$date == d), ]
        D <- .num2date(d)
        if(nrow(k) > 0){
          dusk <- mean(k$dusk)
          dawn <- mean(k$dawn)
          sunrise <- mean(k$sunrise)
          sunset <- mean(k$sunset)
        }else{
          dlong <- .date2datetime(D,tz = tz)
          k2 <- ts_df[which(abs(ts_df$date-D) == min(abs(ts_df$date-D)))[1],]
          refdate <- .date2datetime(k2$date,tz = tz,midday = F)
          sunrise <- k2$sunrise-refdate+dlong
          sunset <- k2$sunset-refdate+dlong
          dawn <- k2$dawn-refdate+dlong
          dusk <- k2$dusk-refdate+dlong
        }
        
        if(sunset > sunrise){
          rect(sunrise, ylim[1], sunset, ylim[2], col="white", lwd=0)
          rect(dawn, ylim[1], sunrise, ylim[2], col="grey90", lwd=0)
          rect(sunset, ylim[1], dusk, ylim[2], col="grey90", lwd=0)
          dawn.dusk <- F
        }else{
          rect(.date2datetime(D,tz = tz,midday = F), ylim[1], sunset, ylim[2], col="white", lwd=0)
          rect(sunrise, ylim[1], .date2datetime(D+1,tz = tz,midday = F), ylim[2], col="white", lwd=0)
          rect(dawn, ylim[1], sunrise, ylim[2], col="grey90", lwd=0)
          rect(sunset, ylim[1], dusk, ylim[2], col="grey90", lwd=0)
          dawn.dusk <- F
        }
        
        if(.num2date(d) == days[1] & as.Date(ts_df$sunset[1],tz=tz) > ts_df$date[1]){
          sunrise <- as.Date(ts_df$sunset[1],tz=tz)-24*60*60
          sunset <- sunset-24*60*60
          dawn <- dawn-24*60*60
          dusk <- dusk-24*60*60
          
          rect(xlim[1], ylim[1], sunset, ylim[2], col="white", lwd=0)
          rect(sunset, ylim[1], dusk, ylim[2], col="grey90", lwd=0)
        }
      }
    }else{
      warning('no geolocation data (Lon, Lat) or daytime period information provided! setting plot_DayTimePeriods to FALSE')
      plot_DayTimePeriods <- F
    }
    ts_df$dusk <- ts_df$dawn <- c()
    
    xti <- which(xticks >= xlim[1] & xticks <=xlim[2])
    axis(1, at=xticks[xti], labels=xtick.labels[xti], xpd=TRUE, pos=par()$usr[3], cex.axis=cex.axis, lwd=0, lwd.ticks = 1)
    
    ### show_time_limits
    # if(.datetime2min(xlim[1]) != 0)  axis(1, at=xlim[1], labels=format(xlim[1], "%H:%M"), xpd=TRUE, pos=par()$usr[3], cex.axis=cex.axis, lwd=0, lwd.ticks = 1)
    # if(.datetime2min(xlim[2]) != 0)  axis(1, at=xlim[2], labels=format(xlim[2], "%H:%M"), xpd=TRUE, pos=par()$usr[3], cex.axis=cex.axis, lwd=0, lwd.ticks = 1)
    
    date.ticks <- xticks[which(xtick.labels == "12" & xticks >= xlim[1])]
    axis(1, at=date.ticks, labels=format(date.ticks, "%Y-%m-%d"), lwd=0, line=1, cex.axis=cex.axis2)
    
    ###' not correct midday:
    #   if(show.sun){
    #     si <- which(.datetime2hour.dc(xticks[xti]) == 12)
    #     for(sii in si) text(xticks[xti][sii], y=max(ylim)-10, labels = "\U25d7", srt="90", cex=6, col=colors()[143], xpd=F)
    #   }
    
    
    # if(show.temp){ ### interpolate Temperature data and add it to plot:
    # Depth_res <- 1
    # maxDepth <- max(ylim)
    # depths <- seq(0,maxDepth,by=Depth_res) # sequence of depth values defining the vertical grid resolution
    # ts_df$datetimeh <- .date2datetime(ts_df$date,midday = F)+datetime2hour(ts_df$datetime)*3600
    # ts_df$datetimeh_id <- as.numeric(1+difftime(ts_df$datetimeh,min(ts_df$datetimeh),units = "hours"))
    # datetimeh_all <- seq(min(ts_df$datetimeh),max(ts_df$datetimeh),by=3600)
    # ndates <- length(datetimeh_all)
    # Temperature_matrix <- matrix(ncol=ndates, nrow=length(depths), NA) # date-depth matrix
    # smooth_hours <- 0
    # smh <- smooth_hours
    # 
    # for(d in unique(ts_df$datetimeh_id)){
    #   # d <- 1
    #   ts_df_sub <- ts_df[which(ts_df$datetimeh_id %in% seq(d-smh,d+smh,by=1)),]
    #   if(length(unique(ts_df_sub$Temp)) > 1){
    #     ts_df_sub$Temp <- ts_df_sub$Temp
    #     # k2 <- plyr::ddply(ts_df_sub[,which(names(ts_df_sub) %in% c('datetimeh_id','Depth','Temp'))],c("datetimeh_id","Depth"),function(x)c(Temp=mean(x$Temp)))
    #     # xx <- c(k2$datetimeh_id,k2$datetimeh_id+1)
    #     
    #     k2 <- plyr::ddply(ts_df_sub[,which(names(ts_df_sub) %in% c('date','Depth','Temp'))],c("date","Depth"),function(x)c(Temp=mean(x$Temp)))
    #     xx <- c(k2$date,k2$date+1)
    #     
    #     yy <- c(k2$Depth,k2$Depth)
    #     zz <- c(k2$Temp,k2$Temp)
    #     temp <- akima::interp(x=xx,y=yy,z=zz,linear=T,duplicate='mean',yo=depths) # interpolate data per day
    #     Temperature_matrix[,d] <- temp$z[1,]
    #   }
    # }
    # xi <- datetimeh_all
    # yi <- depths
    # zi <- Temperature_matrix
    # 
    # print(xi)
    #   
    #   
    #   inst.pkg(raster)
    #   f <- raster(zi[nrow(zi):1, ])
    #   extent(f) <- extent(c(range(as.numeric(xi)), range(yi)))
    #   
    #   data(cmap)
    #   par(new=TRUE)
    #   image(f, xlim=as.numeric(xlim), ylim=ylim, col=cmap$light.jet, axes=F, ylab="", xlab="", add=T)#, zlim=zlim)
    # }
    
    ### plot vertical track:
    par(new=T)
    plot(as.numeric(ts_df$datetime), ts_df[[y]], axes=FALSE, xlab="", ylab="", ylim=ylim, xlim=xlim, type=type,xpd=TRUE,...)
    
    axis(2, lwd = 0, cex.axis=cex.axis, lwd.ticks=1)
    if(plot_box) box(bty=bty)
    
    if(missing(main)) main <- paste(ID_main, paste0(main_xlim, collapse=" : "))
    if(plot_info) mtext(side=3, main, font=1.6, line=main.line, cex=cex.main)
    title(ylab = ylab,cex.lab=cex.lab)
    if(plot_info) title(xlab = xlab,cex.lab=cex.lab,line = 3)
    
    if(Return) return(ts_df)
  }
}

