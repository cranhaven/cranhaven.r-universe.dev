dy_DepthTS <- dy_TS <- function(ts_df, y="Depth", xlim, ylim, 
                                ylab=y, xlab, main,
                                ID, ID_label="Serial", 
                                plot_DayTimePeriods=TRUE, twilight.set="ast", 
                                color="darkblue",
                                doRangeSelector=TRUE, drawPoints=FALSE,pointSize=2,
                                tz="UTC", ...){
  
  if(missing(xlab)) xlab <- paste0("Time (",tz,")")
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
  
  
  if(missing(ID) & is.null(ts_df[[ID_label]])) {
    k <- c("DeployID","Ptt") %in% names(ts_df)
    if(any(k)) {
      ID_label <- c("DeployID","Ptt")[which(k)[1]]
      warning(paste0("ID_label 'Serial' not found! Resetting ID_label to '",ID_label,"'!"))
    }
    if(!all(k)) warning(paste0("no column corresponding to ID_label '",ID_label,"' found!"))
  }
  if(missing(ID)) ID <- unique(ts_df[[ID_label]])
  if(length(ID) > 1) {
    warning("multiple tags in data set: ", paste(ID, collapse=', '))
    main <- ''
  } 
  if(missing(main)) main <- paste("Tag ID", ID)
  
  if(missing(ylim)){
    if(y == "Depth") {ylim <- c(max(ts_df$Depth,na.rm = T),-1)
    }else{
      ylim <- range(ts_df$y) 
    }
  }
  if(!missing(xlim)) {
    if(class(xlim)[1] == 'Date' | nchar(as.character(xlim[1])) == 10){
      xlim <- as.Date(xlim)
      if(length(xlim) == 1) xlim <- c(xlim, xlim)
      xlim[2] <- xlim[2]+1
      xlim <- paste(xlim, '00:00:00')
    }
    xlim <- .fact2datetime(xlim,tz = "UTC")
    if(length(xlim) > 2) xlim <- range(xlim)
    if(length(xlim) == 1) xlim <- c(xlim, xlim[1]+24*60*60)
    
    ts_df <- ts_df[which(ts_df$datetime >= xlim[1] & ts_df$datetime <= xlim[2]),]
  }
  
  dat <- ts_df[,c("datetime",y)]
  dat$datetime <- as.POSIXct(dat$datetime,tz = "UTC")
  ds <- data.frame(dat[,y]); names(ds) <- y
  dat_xts <- xts::xts(ds,order.by=dat$datetime)
  
  # create dygraph
  dg <- dygraph(dat_xts, xlab=xlab, ylab=ylab, main=main, ...)
  
  
  
  # add shades
  if(plot_DayTimePeriods){
    dawn <- paste0("dawn.",twilight.set)
    dusk <- paste0("dusk.",twilight.set)
    shade_periods <- c("sunrise","sunset",dawn,dusk)
    if(!all(shade_periods %in% names(ts_df))) {
      if(all(c("Lon", "Lat", "datetime") %in% names(ts_df))){ 
        ts_df <- get_DayTimeLimits(ts_df)
      }else{
        plot_DayTimePeriods <- F
        warning("no geolocation data or datetime vector provided (Lon, Lat, datetime)! plot_DayTimePeriods omitted in current call. Please revise.")
      }
    }
  }
  if(plot_DayTimePeriods){
    shades <- unique(ts_df[,shade_periods])
    add_shades <- shades[1,]; add_shades <- add_shades-24*60*60
    shades <- rbind(add_shades, shades)
    shades_list <- list()
    j <- 1
    
    for(i in 1:nrow(shades)){
      add <- list(from=as.POSIXct(shades$sunrise[i],tz = "UTC"), to=as.POSIXct(shades$sunset[i],tz = "UTC"),color="white")
      shades_list[[j]] <- add
      j <- j+1
      add <- list(from=as.POSIXct(shades$dawn.ast[i],tz = "UTC"), to=as.POSIXct(shades$sunrise[i],tz = "UTC"),color="lightgrey")
      shades_list[[j]] <- add
      j <- j+1
      add <- list(from=as.POSIXct(shades$dusk.ast[i],tz = "UTC"), to=as.POSIXct(shades$sunset[i],tz = "UTC"),color="lightgrey")
      shades_list[[j]] <- add
      j <- j+1
    }
    shades_list
    
    dg <- dyShading(dg, from = ts_df$datetime[1] , to = tail(ts_df$datetime,1),color = "darkgrey" )
    for( period in shades_list ) {
      dg <- dyShading(dg, from = period$from , to = period$to,color = period$color)
      dg <- dyAnnotation(dg, x = mean(c(period$from,period$to)), text = period$label, attachAtBottom=T)
    }
  }
  label <- y
  if(y == "Depth") label <- paste(y,"(m)")
  dg <- dg %>% dyOptions(useDataTimezone = TRUE,colors=color,drawPoints = drawPoints, pointSize = pointSize, strokeWidth = 1) %>% dyAxis("y", label = label, valueRange = ylim)
  if(doRangeSelector) dg <- dg %>% dyRangeSelector()
  return(dg)
}
