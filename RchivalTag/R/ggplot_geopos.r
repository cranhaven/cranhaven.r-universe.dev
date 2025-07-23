ggplot_geopos <- function(x, ggobj, xlim, ylim, zlim, standard_year=FALSE, full_year=standard_year, date_format, lang_format="en", tz="UTC", 
                          Breaks, cb.title, cb.date_format, cbpos, cb.height = 10, cb.xlab = "",
                          cb.reverse=FALSE, pal.reverse=cb.reverse, prob_lim=.75, color_by="date", pal, alpha=70, type="p", main ,lwd=1, size=2, 
                          shape=19, verbose= FALSE, ...){
  fill_scale <- F
  trans <- "identity"
  if(cb.reverse) trans <- 'reverse'
  if(missing(cbpos)) cbpos <- "r"
  cbpos_long <- c("left","right","top","bottom")
  cbpos <- cbpos_long[which(substr(cbpos_long,1,1) == cbpos)]
  if(alpha > 100){
    alpha <- 100
    warning("user specified alpha-value > 100%, resetting to 100%!")
  }
  
  if(missing(date_format) & is(x,"SpatialPolygonsDataFrame")) date_format <- "%d-%b-%Y %H:%M:%S"
  
  if(is.character(x)) {
    if(substr(x,nchar(x)-3,nchar(x)) == ".nc") date_format <- "%Y-%m-%d %H:%M:%S"
    x <- get_geopos(x, verbose=verbose, date_format=date_format)
  }
  
  if(missing(date_format)) date_format <- "%d-%b-%Y %H:%M:%S"
  if(missing(cb.date_format) & !standard_year) cb.date_format <- "%Y-%m-%d"
  
  cmap <- NULL
  data(cmap, package='oceanmap', envir = environment())
  if(missing(pal)) {
    pal <- "jet"
    pal_set <- T 
  }
  if(length(pal) == 1 & pal[1] %in% names(cmap)) pal <- cmap[[pal]]
  if(pal.reverse) pal <- rev(pal)
  if(is.data.frame(x)){
    pos <- x
    required_fields <- c('Serial','DeployID','Ptt','date','datetime','Lon','Lat')
    missing_fields <- required_fields[which(!(required_fields %in% names(pos)))]
    if(length(missing_fields) > 0) stop(paste('Missing fields in provided input data:',missing_fields, '\nPlease revise!'))
    
    if(missing(xlim)) xlim <- range(pos$Lon+c(.5,-.5))
    if(missing(ylim)) ylim <- range(pos$Lat+c(.5,-.5))
    if(missing(ggobj)) ggobj <- oceanmap::ggplotmap(xlim=xlim, ylim=ylim, ...)
    
    if(color_by %in% c("date")){
      
      all_year <- F
      if(standard_year) {
        if(missing(cb.title)) cb.title <- ""
        pos$date <- as.Date(paste0("0",substr(as.Date(pos$date),5,10)))
        if(missing(cb.date_format)) cb.date_format <- "%b"
        all_year <- T
        if(!missing(zlim)) {
          zlim <- as.Date(paste0("0",substr(as.Date(zlim),5,10)))
          all_year <- F
        }
      }
      if(missing(cb.title)) cb.title <- "Date"
      
      ### set common colors
      if(missing(zlim)) {
        zlim <- as.Date(range(pos$date))
      }else{
        zlim <- as.Date(zlim)
      }
      if(standard_year & full_year) {
        zlim <- as.Date(c("0-01-01","0-12-31")) 
        if(pal_set) pal <- cmap$year.jet
        if(pal.reverse) pal <- rev(pal)
      }
      dates <- zlim[1]:zlim[2]
      
      cols <- colorRampPalette(pal)(length(dates))
      Raster.cols <- .makeTransparent(cols,alpha = 255*alpha/100)#[1:100] #creates a color scale, add as many values as you want or use an existing scale
      df.col <- data.frame(datenm=dates, color = as.character(Raster.cols),stringsAsFactors = F) # merge colors and time steps
      df.col$color_full <- as.character(colorRampPalette(cmap$jet)(length(cols)))
      
      pos$datenm <- as.numeric(pos$date)
      pos$shape <- shape
      pos$size <- size
      cmb <- merge(pos,df.col,by="datenm",all=T,sort=F)
      cmb$text <- paste("datetime:", cmb$datetime)
      
      
      cmb <- cmb[which(!is.na(cmb$Lat) & !is.na(cmb$Lat)),]
      cmb$color_full <- as.character(cmb$color_full)

      ## find best ID label:
      ID_Labels <- as.data.frame(cmb[1,c("DeployID","Serial","Ptt")])
      ID_Labels <- names(ID_Labels[,which(apply(ID_Labels,2,function(x)!is.na(x)))])
      smLabels <- as.vector(apply(cmb[,c(ID_Labels)], 2, function(x) length(unique(x))))
      ID_Label <- ID_Labels[which(smLabels == max(smLabels))][1]
        
      # for(co in ID_Labels) cmb[[co]] <- paste0(co,": ",cmb[[co]],"\n")
      
      # cmb$ID_label <- apply( cmb[ , ID_Labels ] , 1 , paste , collapse = "" )
      
      cmb <- cmb[order(cmb[[ID_Label]],cmb$datetime),]
      
      ## add space after each track (to avoid interpolating lines between multiple tracks when standard_year=T)
      a <- cmb[1,]; a[,] <- NA
      cmb <- do.call(rbind, lapply(split(cmb, cmb[[ID_Label]]), function(i){
        add <- a
        add[[ID_Label]] <- i[[ID_Label]][1]
        rbind(add, i)
      }))

      d <- cmb[1,]; d[,] <- NA
      add <- rbind(cmb[2:nrow(cmb),],d)
      add <- add[,c(ID_Label,"Lon","Lat")]
      names(add) <- paste0(names(add),"2")
      cmb2 <- cbind(cmb,add)
      cmb2$datenm[which(cmb2[[ID_Label]] != cmb2[[ID_Label]])] <- NA
      cmb2 <- cmb2[which(!is.na(cmb2$datetime)),]
      
      if(type %in% c("p","b","pl")) ggobj <- ggobj + suppressWarnings(geom_point(cmb,mapping = aes_(x=~Lon,y=~Lat,colour=~datenm,group=as.name(ID_Label),text=~text),size=cmb$size,shape=cmb$shape)) # original, works with line color but not fill
      # if(type %in% c("p","b","pl")) ggobj <- ggobj + suppressWarnings(geom_point(cmb,mapping = aes_(x=~Lon,y=~Lat,color=~datenm,group=as.name(ID_Label),text=~text),size=cmb$size,shape=cmb$shape,fill=cmb$color))
      # if(type %in% c("p","b","pl")) ggobj <- ggobj + suppressWarnings(geom_point(cmb,mapping = aes_(x=~Lon,y=~Lat,group=as.name(ID_Label),text=~text),size=cmb$size,shape=cmb$shape,fill=cmb$color)) # color error
      if(type %in% c("l","b","pl")) ggobj <- ggobj + suppressWarnings(geom_segment(cmb2,mapping=aes_(x=~Lon,y=~Lat,xend=~Lon2,yend=~Lat2,
                                                                                                     colour=~datenm,group=as.name(ID_Label),text=~text),size=lwd))
      
    }else{
      if(missing(cb.title)) cb.title <- color_by
      
      tags <- unique(x[[color_by]])
      tags <- tags[order(tags)]
      cols <- colorRampPalette(pal)(length(tags))
      Raster.cols <- .makeTransparent(cols,alpha = 255*alpha/100)#[1:100] #creates a color scale, add as many values as you want or use an existing scale
      df.col <- data.frame(color = as.character(Raster.cols),stringsAsFactors = F) # merge colors and time steps
      df.col$color_full <- as.character(colorRampPalette(cmap$jet)(length(cols)))
      df.col[[color_by]] <- factor(tags,levels=tags)
      
      # pos$datenm <- as.numeric(pos$date)
      cmb <- merge(pos,df.col,by=color_by,all=T,sort=F)
      ID <- cmb$color_by <- factor(cmb[[color_by]],levels=levels(df.col[[color_by]] ))
      cmb$text <- paste("datetime:", cmb$datetime)
      
      if(type %in% c("p","b","pl")) ggobj <- ggobj + suppressWarnings(geom_point(cmb,mapping = aes_(x=~Lon,y=~Lat,color=ID,text=~datetime),size=size,shape=shape,fill=cmb$color))
      if(type %in% c("l","b","pl")) ggobj <- ggobj + suppressWarnings(geom_path(cmb,mapping = aes_(x=~Lon,y=~Lat,color=ID,text=~datetime),size=lwd)) 
    }
    b <- ggobj
    
  }else{
    if(is(x,"SpatialPolygonsDataFrame")){
      pos <- x
      required_fields <- c('Serial','DeployID','Ptt','datetime')
      missing_fields <- required_fields[which(!(required_fields %in% names(pos)))]
      if(length(missing_fields) > 0) stop(paste('Missing fields in provided input data:',missing_fields, '\nPlease revise!'))
      
      pos@data$fill=colorRampPalette(pal)(nrow(pos))
      pos$id <- 1:nrow(pos)
      pos$bg <- 1:nrow(pos)
      pols_df <- fortify(pos)
      
      cmb <- pols_df
      cmb$datetime <- tmp <- gsub("outer ","",cmb$id)
      if(any(nchar(tmp) == 10)){
        tmp[which(nchar(tmp) == 10)] <- paste(tmp[which(nchar(tmp) == 10)],"00:00:00")
      }
      
      cmb$date <- as.Date(strptime(tmp,format = date_format,tz = tz))
      if(is.na(cmb$date[1])){
        cmb$date <- as.Date(substr(tmp,1,10),format = "%Y-%m-%d")
      }
      if(is.na(cmb$date[1])){
        cmb$date <- as.Date(.fact2datetime(tmp,date_format = date_format,tz = tz))
      }
      
      all_year <- F
      if(standard_year) {
        if(missing(cb.title)) cb.title <- ""
        cmb$date <- as.Date(paste0("0",substr(as.Date(cmb$date),5,10)))
        if(missing(cb.date_format)) cb.date_format <- "%b"
        all_year <- T
        if(!missing(zlim)) {
          zlim <- as.Date(paste0("0",substr(as.Date(zlim),5,10)))
          all_year <- F
        }
      }
      if(missing(cb.title)) cb.title <- "Date"
      cmb$datenm <- as.numeric(cmb$date)
      
      if(missing(zlim)){
        date_range_nm <- range(cmb$datenm)
        zlim <- range(cmb$date)
      }else{
        date_range_nm <- as.numeric(as.Date(zlim)) 
      }
      
      ### set common colors
      tsteps <- min(date_range_nm):max(date_range_nm)
      cols <- colorRampPalette(pal)(length(tsteps))
      Raster.cols <- .makeTransparent(cols,alpha = 255*alpha/100)#[1:100] #creates a color scale, add as many values as you want or use an existing scale
      df.col <- data.frame(datenm=tsteps,stringsAsFactors = F) # merge colors and time steps
      df.col$color <- as.character(Raster.cols)
      
      # cmb <- merge(cmb, df.col, by="datenm", all=F, sort=F)
      
      info <- as.data.frame(pos[1,c("DeployID","Serial","Ptt","datetime")])
      info$datetime <- as.character(info$datetime)
      info <- info[,which(apply(info,2,function(x)!is.na(x)))]
      cmb$text <- paste(paste0(names(info),":"),info,collapse = "\n")
      
      if(missing(xlim)) xlim <- c(trunc(min(x$xmin)),ceiling(max(x$xmax)))
      if(missing(ylim)) ylim <- c(trunc(min(x$ymin)),ceiling(max(x$ymax)))
      
      if(missing(ggobj)) ggobj <- oceanmap::ggplotmap(xlim=xlim, ylim=ylim, ...)
      
      b <- ggobj + suppressWarnings(geom_polygon(data = cmb, aes_(x=~long,y=~lat,group = ~group,colour=~datenm,fill=~datenm,text=~text)))#,fill= cmb$color) 
      fill_scale <- T
      
    }
  }
  if(!missing(main)) b <- b + ggtitle(main)
  if(color_by %in% c("date")){
    if(missing(Breaks)) Breaks <- pretty(zlim)
    Breaks <- as.numeric(Breaks)
    # Breaks <- as.numeric(pretty(.as.Date_origin(cmb$datenm)))
    zlim <- as.numeric(as.Date(zlim))
    # if(all(format(.as.Date_origin(zlim),"%d") %in% c("01","15"))) {
    #   Breaks <- .as.Date_origin(zlim[1]:zlim[2])
    #   Breaks <- as.numeric(Breaks[which(format(Breaks,"%d") %in% c("01","15"))])
    # }
    # limits <- range(zlim)
    limits <- range(c(Breaks,zlim))
    
    # if(!all(range(cmb$datenm) %in% Breaks)) Breaks <- c(median(unique(cmb$datenm)),range(cmb$datenm))
    if(standard_year & all_year){
      Breaks <-.as.Date_origin(zlim[1]:zlim[2])
      Breaks <- Breaks[which(substr(Breaks,nchar(Breaks)-1,nchar(Breaks)) == "15")]
      Breaks <- as.numeric(Breaks)
      limits <- range(zlim)
    }
    # if(length(cmb$datenm) == 1) Breaks <- cmb$datenm
    Breaks <- Breaks[order(Breaks)]
    # 
    Labels <- as.character(.formatDate2cb.date_format(.as.Date_origin(Breaks), cb.date_format=cb.date_format))
    if(length(unique(as.character(Labels))) != length(Labels)) {
      Labels[seq(2,length(Labels),by=2)] <- ""
    }
    if(trans == "reverse"){
      Labels <- rev(Labels)
      Breaks <- rev(Breaks)
      limits <- rev(limits)
    }
    out <- b + suppressWarnings(scale_colour_gradientn(name = cb.title, 
                                                       colours=Raster.cols, 
                                                       labels=Labels,
                                                       breaks=Breaks,
                                                       limits=limits,
                                                       trans=trans))
    if(fill_scale) out <- out + suppressWarnings(scale_fill_gradientn(name = cb.title,
                                                                      colours=Raster.cols,
                                                                      labels=Labels,
                                                                      breaks=Breaks,
                                                                      limits=limits,
                                                                      trans=trans))
    
    out <- out + theme(legend.position = cbpos) + guides(color=guide_colourbar(barheight = cb.height))
  }else{
    out <- b + suppressWarnings(scale_colour_manual(values = df.col$color,guide = "legend",name=cb.title)) + theme(legend.position = cbpos, legend.key=element_blank()) #+ guides(color=guide_colourbar(barheight = cb.height))
  }
  return(out)
}


.as.Date_origin <- function(x){
  as.Date(x, origin = '1970-01-01')
}

.formatDate2cb.date_format <- function(Date, cb.date_format, english=T,abbrev=F){
  if(english) lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  x <- Date#as.character(Date)
  out <- format(x,format=cb.date_format)
  return(out)
}
