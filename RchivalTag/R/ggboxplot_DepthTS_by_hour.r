ggboxplot_DepthTS_by_hour <- function(ts_df, ylim, min_perc=75,
                                      main, submain, add_ids_to_submain=FALSE, xlab,
                                      ID, ID_label="Serial", 
                                      plot_DayTimePeriods=TRUE, twilight.set="ast",
                                      box=TRUE, jitter=FALSE, 
                                      color_by=ID_label, cb.title=color_by,
                                      pal, opacity=0.1, tz="UTC"){
  df <- ts_df
  if(missing(ID) & is.null(ts_df[[ID_label]])) {
    k <- c("DeployID","Ptt") %in% names(ts_df)
    if(any(k)) {
      ID_label <- c("DeployID","Ptt")[which(k)[1]]
      warning(paste0("ID_label 'Serial' not found! Resetting ID_label to '",ID_label,"'!"))
    }
    if(!all(k)) warning(paste0("no column corresponding to ID_label '",ID_label,"' found!"))
  }else{
    if(!missing(ID)) df <- df[which(df[[ID_label]] %in% ID),]
  }
  
  dawn.set <- paste0('dawn.', twilight.set)
  dusk.set <- paste0('dusk.', twilight.set)
  if(!all(c("sunrise","sunset",dawn.set,dusk.set) %in% names(df))){
    warning("daytime period information not found (sunrise, sunset, dawn, dusk).\n Consider joining positioning data and run 'get_DayTimeLimits' to illustrate twilight and night shadings.")
    plot_DayTimePeriods <- FALSE 
  }
  
  
  if(nrow(df) == 0){
    ggobj <-  ggplot() + theme_minimal() 
  }else{
    
    if(missing(xlab)) xlab <- paste0("Time (",tz,")")
    if(!("datetime" %in% names(ts_df))) stop('no "datetime" vector provided! please revise.')
    
    ## check and fill potential data gaps:
    ts_df <- df
    ts_df_bkp <- ts_df
    ts_df <- c()
    ids <- unique(ts_df_bkp[[ID_label]])
    for(id in ids){
      tag_df <- ts_df_bkp[which(ts_df_bkp[[ID_label]] == id),]
      datetime_nm <- as.numeric(.fact2datetime(tag_df$datetime,tz="UTC"))
      tstep <- (pracma::Mode(diff(datetime_nm)))
      if(tstep == 0) stop("time step between first valid 'y'-records is 0. please revise!")
      add0 <- data.frame(datetime_nm=seq(datetime_nm[1],tail(datetime_nm, 1), by=tstep))
      add0$datetime <- .num2datetime(add0$datetime_nm,tz="UTC",hours.offset = 0)
      datetime_nm <- add0$datetime_nm <- c()
      tag_df$datetime <- as.character(tag_df$datetime); add0$datetime <- as.character(add0$datetime)
      add_tag <- merge(tag_df, add0, by="datetime",all=T)
      ts_df <- rbind(ts_df,add_tag)
    }
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
      if(all(c(dawn.set, dusk.set, 'sunrise','sunset') %in% names(ts_df)) | all(c('datetime','Lon','Lat') %in% names(ts_df))){
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
    df <- ts_df
    
    # sm <- df %>% group_by_(ID_label) %>% summarize(tstep=head(diff(as.numeric(datetime)),1))
    sm <- plyr::ddply(df,c(ID_label),function(x) c(tstep=head(diff(as.numeric(x$datetime)),1)))
    if(length(unique(sm$tstep)) > 1) {
      warning(message(paste("DepthTS data from several individuals contains different temporal resolutions.\nTrying to resample DepthTS data with resample_TS-function to common resolution\n", 
                            message(paste0(capture.output(sm), collapse = "\n")))))
      tstep <- max(sm$tstep)
      ## combine tagging data:
      df_bkp <- df
      df <- c()
      ids <- unique(df_bkp[[ID_label]])
      colnames <- names(df_bkp)
      for(id in ids){
        add <- df_bkp[which(df_bkp[[ID_label]] == id),]
        res <- diff(as.numeric(add$datetime))[1]
        if(res != tstep) add  <- resample_TS(add,tstep = tstep)[[1]][,colnames]
        df <- rbind(df,add)
      }
    }
    
    if(is.null(df[["date"]])) df$date <- as.Date(df$datetime)
    df$hour <- round(.datetime2hour.dc(df$datetime,tz = tz))
    df$hour[which(df$hour == 24)] <- 0
    df$hour <- factor(df$hour,levels=0:23)
    
    if(any(df$Depth < 0,na.rm = T)) df$Depth[which(df$Depth < 0)] <- 0
    df$DeployID <- df$Ptt <- df[[ID_label]]
    h <- ts2histos(df,tad_breaks = c(0,100,5000),min_perc = min_perc,aggregate_by = ID_label)
    h2 <- h$TAD$merged$df
    df$key <- paste(df[[ID_label]],df$date)
    df <- df[which(df$key %in% paste(h2[["DeployID"]],h2$date) & !is.na(df$Depth)),]
    # df <- df %>% filter(key %in% paste(h2[["DeployID"]],h2$date) & !is.na(Depth))
    dates <- df[which(!is.na(df$Depth)),]$date
    nrec <- length(unique(dates)) # days of data
    ids <- unique(df[[ID_label]])
    prefix <- "Tag ID"
    if(length(ids) > 1) prefix <- "Tag IDs"
    if(missing(main)) main <- paste(prefix, paste(ids,collapse=", "), "-", paste(range(dates),collapse=" : "))
    if(missing(submain)) {
      if(add_ids_to_submain){
        submain <- paste(paste(ids,collapse=", "),paste(nrec,"days of data"),sep=" | ")
      }else{
        submain <- paste(nrec,"days of data")
      }
    }
    
    if(jitter & opacity != 0) {
      outlier.shape <- NA
    }else{
      if(!missing(color_by)) warning("color_by argument ignored since jitter = FALSE")
      if(!missing(pal)) warning("color_by argument ignored since jitter = FALSE")
      outlier.shape <- 19
    }
    xlim <- c(0.5,24.5)
    
    if(missing(ylim)) ylim <- pretty(df$Depth)
    ylim <- rev(range(ylim))
    ylab <- "Depth (m)"
    
    ggobj <- ggplot(df) + 
      coord_cartesian(xlim =xlim, ylim =ylim) +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      geom_boxplot(aes_string(y="Depth",x="hour"),outlier.shape=NA)
    # ggobj <- ggobj + geom_rect(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill="grey", color=NA, alpha=.5, size=0)
    ggobj <- ggobj + 
      ylab(ylab) +
      xlab(xlab) +
      theme_classic(base_size=14) 
    ggobj2 <- ggobj
    if(plot_DayTimePeriods){
      ### calculate sunset/sunrise hours
      sunrise <- mean(.datetime2hour.dc(df$sunrise,tz=tz))+1 ## +1 is required because the index of the boxplots is 1:24 not 0:23
      sunset <- mean(.datetime2hour.dc(df$sunset,tz=tz))+1
      dawn <- mean(.datetime2hour.dc(df$dawn.ast,tz=tz))+1
      dusk <- mean(.datetime2hour.dc(df$dusk.ast,tz=tz))+1
      ggobj <- ggobj + annotate("rect", xmin=xlim[1], xmax=xlim[2], ymin=ylim[2], ymax=ylim[1], fill="grey")
      if(sunset[1] < sunrise[1]){
        ggobj <- ggobj +
          annotate("rect", xmin = sunrise, xmax = xlim[2], ymin=ylim[2], ymax=ylim[1], fill="white") + 
          annotate("rect", xmin = xlim[1], xmax = sunset, ymin=ylim[2], ymax=ylim[1], fill="white") 
      }else{
        ggobj <- ggobj +
          annotate("rect", xmin = sunrise, xmax = sunset, ymin=ylim[2], ymax=ylim[1], fill="white")
      }
      ggobj <- ggobj +
        annotate("rect", xmin = dawn, xmax = sunrise, ymin=ylim[2], ymax=ylim[1], fill="grey90") + 
        annotate("rect", xmin = sunset, xmax = dusk, ymin=ylim[2], ymax=ylim[1], fill="grey90")
    }
    ggobj <- ggobj + geom_boxplot(aes_string(y="Depth",x="hour"),outlier.shape=outlier.shape)
    
    ggobj <- ggobj + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           axis.ticks.length=unit(.25, "cm"))
    if(box) ggobj <- ggobj + theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))
    
    ggobj <- ggobj + labs(title = main, subtitle = submain)
    
    if(jitter & opacity != 0) {
      cmap <- NULL
      data(cmap, package='oceanmap', envir = environment())
      if(missing(pal)) pal <- "jet"
      if(length(pal) == 1) pal <- cmap[[pal]]
      pal <- colorRampPalette(cmap$jet)(length(unique(df[[color_by]])))
      ggobj <- ggobj + 
        geom_jitter(aes_string(y="Depth",x="hour",colour = color_by)) + #!!ensym(color_by))) +
        scale_color_manual(name=cb.title, values=alpha(pal,opacity))
    }
  }
  return(ggobj)
}