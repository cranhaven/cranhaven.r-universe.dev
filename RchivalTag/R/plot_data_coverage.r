

abacus_plot <- plot_data_coverage <- function(x, type, type2, meta, 
                               Identifier="Serial", fields=c("Serial","Ptt"),
                               date_range_std, show_fullmonths=TRUE,
                               zlim, mars, na.omit=TRUE,
                               do.arrows=TRUE, xpos.arrows=-.25, xpos.years=-.27, 
                               xpos.fields=c(-.01,-.12), ypos.fields,
                               main, cex.main=1.2,
                               cb.xlab, cex.cb.xlab=1,
                               cb.ticks, cex.cb.ticks=.9,
                               pal="jet", bg="grey"){

  meta_cols <- unique(c("dep.date","pop.date",Identifier,fields))
  miss <- which(!(meta_cols %in% names(meta)))
  if(length(miss) > 0) stop("meta file with missing columns: ",paste(meta_cols[miss],collapse=", "))
  
  meta <- meta[order(meta$dep.date),]
  meta$dep.year <- as.numeric(format(meta$dep.date,"%Y"))
  meta$pop.year <- as.numeric(format(meta$pop.date,"%Y"))
  
  cmap <- NULL
  data(cmap, package='oceanmap', envir = environment())
  if(length(pal) == 1) pal <- cmap[[pal]]
  
  if(missing(meta)) stop("meta data missing, please revise!")
  if(!is(meta, 'data.frame')) stop("meta data not of class 'data.frame'. Please revise!")
  meta[[Identifier]] <- as.character( meta[[Identifier]])
  identifiers <- meta[[Identifier]]
  
  year_diff <- meta$pop.year-meta$dep.year
  if(missing(date_range_std)){
    ydays.lim <- c(min(lubridate::yday(meta$dep.date)), max(lubridate::yday(meta$pop.date[which(year_diff %in% max(year_diff))])))
    date_range_std <- as.Date(c('0-1-01', paste0(max(year_diff),'-1-01')))+ydays.lim-1
    if(show_fullmonths){
      # date_range_std[2] <- date_range_std[2]+30
      # date_range_std <- as.character(date_range_std)
      date_range_std[1] <- lubridate::floor_date(date_range_std[1],unit="months") ###paste0(substr(date_range_std[1],1,nchar(date_range_std[1])-2),"01")
      date_range_std[2] <-  lubridate::ceiling_date(date_range_std[2],unit="months") ###paste0(substr(date_range_std[2],1,5),"01")
      # date_range_std <- as.Date(date_range_std)
    }
  }
  x_dates <- .num2date(date_range_std[1]:date_range_std[2])
  
  m <- m_hist <- m_ts <- m_lightlocs <- matrix(NA,ncol=length(x_dates),nrow=length(identifiers))

  for(i in 1:nrow(m)){
    identifier <- identifiers[i]
    a <- meta[which(meta[[Identifier]] == identifier),]
    dates <- .num2date(a$dep.date:a$pop.date)
    sm <- data.frame(date=dates,val=1,stringsAsFactors = F)
    year <- .date2year(sm$date)[1]
    sm$date <- gsub(year,"0000",sm$date)
    sm$date <- gsub(year+1,"0001",sm$date)
    sm$date <- gsub(year+2,"0002",sm$date)
    sm$date <- as.Date(sm$date)
    out <- merge(sm,data.frame(date=x_dates),all=T,by="date")
    m[(nrow(m)+1-i),] <- out$val
  }
  
  if(type == "ts"){
    if(missing(type2)) type2 <- "Depth"
    tst <- x
    tags <- names(tst)
    if(is.null(tags)) tags <- unlist(lapply(tst, function(l) l[[Identifier]][1]))
    
    for(i in 1:nrow(meta)){
      identifier <- identifiers[i]
      ii <- grep(identifier,tags)
      print(identifier)
      if(length(ii) > 1) stop("Not unique identifier! Please revise meta file!")
      if(length(ii) > 0){
        dat <- tst[[ii]]
        save(dat,type2,meta,Identifier,identifier,na.omit,x_dates,m_ts,m,i, file="~/Desktop/test.rd")
        load("~/Desktop/test.rd")
        sm0 <- plyr::ddply(dat,c("date"),function(x)c(ntot=nrow(x),ndat=nrow(x[!is.na(x[[type2]]),])))
        sm0$perc <- round(100*sm0$ndat/sm0$ntot,1)
        a <- meta[which(meta[[Identifier]] == identifier),]
        m_dates <- as.Date(a$dep.date:a$pop.date,origin="1970-01-01")
        sm <- merge(sm0, y =data.frame(date=m_dates), all=T, by="date")
        if(na.omit) sm$perc[which(is.na(sm$perc))] <- 0
        if(!na.omit) sm <- sm[which(sm$perc != 0),] 
        
        year <- .date2year(sm$date)[1]
        sm$date <- gsub(year,"0000",sm$date)
        sm$date <- gsub(year+1,"0001",sm$date)
        sm$date <- gsub(year+2,"0002",sm$date)
        sm$date <- as.Date(sm$date)
        out <- merge(sm,data.frame(date=x_dates),all=T,by="date")
        m_ts[(nrow(m)+1-i),] <- out$perc
      }
    }
  }
  
  if(tolower(type) %in% c("tad","tat")){
    if(missing(type2)) type2 <- "nperc_dat"
    
    tads <- x[[toupper(type)]]
    tags <- names(tads)
     for(i in 1:nrow(m)){
      # i <- 1
      identifier <- identifiers[i]
      ii <- grep(identifier,tags)
      if(length(ii) > 0){
        um <- matrix(unlist(strsplit(gsub("Ptt.","",gsub("DeployID.","",tags)),"\\_")),ncol=2,byrow = T)
        ii <- which(um[,1] == identifier | um[,2] == identifier)
        dat <- tads[[tags[ii]]]$df
        sm0 <- dat[,c("date",type2)]
        a <- meta[which(meta[[Identifier]] == identifier),]
        m_dates <- as.Date(a$dep.date:a$pop.date,origin="1970-01-01")
        sm <- merge(sm0, y =data.frame(date=m_dates), all=T, by="date")
        if(na.omit) sm[[type2]][which(is.na(sm[[type2]]))] <- 0
        year <- .date2year(sm$date)[1]
        sm$date <- gsub(year,"0000",sm$date)
        sm$date <- gsub(year+1,"0001",sm$date)
        sm$date <- gsub(year+2,"0002",sm$date)
        sm$date <- as.Date(sm$date)
        out <- merge(sm,data.frame(date=x_dates),all=T,by="date")
        m_hist[(nrow(m)+1-i),] <- out[[type2]]
      }
    }
  }
  # image(m_hist)
  
  if(type == "lightlocs"){
    if(missing(type2)) type2 <- "perc"
    lightlocs <- x
    ll_tags <- names(lightlocs)
    for(i in 1:nrow(m)){
      identifier <- identifiers[i]
      ii <- grep(identifier,ll_tags)
      if(length(ii) > 1) stop("Not unique identifier! Please revise meta file!")
      if(length(ii) > 0){
        dat <- lightlocs[[ll_tags[ii]]]
        sm0 <- dat[,c("date",type2)]
        a <- meta[which(meta[[Identifier]] == identifier),]
        m_dates <- as.Date(a$dep.date:a$pop.date,origin="1970-01-01")
        sm <- merge(sm0, y =data.frame(date=m_dates), all=T, by="date")
        if(na.omit) sm[[type2]][which(is.na(sm[[type2]]))] <- 0
        if(!na.omit) sm <- sm[which(sm[[type2]] != 0),] 
        
        year <- .date2year(sm$date)[1]
        sm$date <- gsub(year,"0000",sm$date)
        sm$date <- gsub(year+1,"0001",sm$date)
        sm$date <- gsub(year+2,"0002",sm$date)
        sm$date <- as.Date(sm$date)
        out <- merge(sm,data.frame(date=x_dates),all=T,by="date")
        m_lightlocs[(nrow(m)+1-i),] <- out[[type2]]
      }
    }
  } 
  ### start plotting:
  if(missing(zlim)) zlim <- c(0,100)
  if(missing(mars)) mars <- c(5,12,4,8)
  par(mar=mars)
  
  image(x_dates,(1:length(identifiers)+.5),t(m),axes=F,xlab="",ylab="",col = bg)
  if(type == "ts") image(x_dates,(1:length(identifiers)+.5),t(m_ts),axes=F,xlab="",ylab="",zlim=zlim,add=T,col=pal)
  if(tolower(type) %in% c("tad","tat")) image(x_dates,(1:length(identifiers)+.5),t(m_hist),axes=F,xlab="",ylab="",zlim=zlim,add=T,col=pal)
  if(type == "lightlocs") image(x_dates,(1:length(identifiers)+.5),t(m_lightlocs),axes=F,xlab="",ylab="",zlim=zlim,add=T,col=pal)
  
  .dates.axis(x_dates,do.yaxis = F)
  abline(h=seq(2,ncol(m)))
  
  ### add field1 and deployment year information
  ylim <- par()$usr[3:4]
  if(missing(ypos.fields)) ypos.fields <- ylim[2]+.3
  if(length(fields) > 0){
    for(i in 1:length(fields)){
      f <- fields[i]
      par(new=T, xaxs='i', yaxs='i', las=1)
      oceanmap::empty.plot(ylim=ylim, xlim=c(0,1))
      axis(2,pos=xpos.fields[i], at=ypos.fields, fields[i], xpd=T, font=2, lwd.ticks=0)
      axis(2,pos=xpos.fields[i], at=(1:length(identifiers))+.5, lwd=0, lwd.ticks = 0, labels = rev(meta[[f]]), font=4, las=1)
    }
  }
  
  if(do.arrows){
    dep.year <- meta$dep.year
    yy <- rev(dep.year)
    n <- length(yy); cf <- .1#(diff(par()$usr[3:4])/n)/2
    for(y in unique(yy)){
      p <- which(yy == y)
      text(xpos.years, mean(c(p,p+1)),y,xpd=T,font=2,srt=90)
      arrows(x0=xpos.arrows,y0=min(p)+cf,x1=xpos.arrows,y1=max(p)+1-cf,xpd=T,length=0.1,code=3,lwd=2.5)
    }
  }
  
  if(missing(cb.xlab)) cb.xlab <- "Data Coverage (%)"
  if(missing(main)) main <- type
  title(main,cex.main=cex.main)
  usr <- par()$usr
  if(missing(cb.ticks)) cb.ticks <- pretty(zlim)
  cb <- oceanmap::set.colorbar(cby=usr[3:4], cbx=c(usr[2]+.02*diff(usr[1:2]), usr[2]+.04*diff(usr[1:2])),
                         ticks=cb.ticks, zlim=zlim, cb.xlab = cb.xlab,pal = pal, cex.cb.ticks = cex.cb.ticks, cex.cb.xlab = cex.cb.xlab)
  
}








