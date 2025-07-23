interpolate_PDTs <- function(ts, Temp_field="MeanPDT", ID_key="Serial", #return_as_matrix=FALSE, 
                             Depth_res=.5, verbose=TRUE, Data_Source='station'){
  interpolate_TempDepthProfiles(ts=ts, Temp_field=Temp_field, ID_key=ID_key, #return_as_matrix=FALSE, 
                                Depth_res=Depth_res, verbose=verbose, Data_Source=Data_Source)
}

interpolate_TempDepthProfiles <- function(ts, Temp_field="Temperature", ID_key="Serial", #return_as_matrix=FALSE, 
                                          Depth_res=.5, verbose=TRUE, Data_Source='station'){ 
  
  var <- 'Depth'
  out <- list()
  #   out <- out_list <- list()
  
  if(is.null(ts[[ID_key]])) ts[[ID_key]] <- 1
  ts$Temperature <- ts[[Temp_field]]
  
  ids <- unique(ts[[ID_key]])
  id <- ids[1]
  for(id in ids){
    ts_sub <- ts[which(ts[[ID_key]] == id),]
    if(is.null(ts_sub$date)){
      ts_sub$date <- ts_sub$date.ch <- ndates <- dates <- dates.ch <- 1
    }else{
      ts_sub$date <- as.Date(ts_sub$date)
      date.range <- range(ts_sub$date)
      dates.daily <- dates <- seq(date.range[1],date.range[2],by=1)
      dates.ch <- as.character(dates)
      ts_sub$date.ch <- as.character(ts_sub$date)
      ndates <- length(dates)
    }
    
    maxDepth <- ceiling(max(ts_sub[[var]],na.rm=T)/10)*10
    depths <- seq(0,maxDepth,Depth_res) # sequence of depth values defining the vertical grid resolution
    Temperature_matrix <- matrix(ncol=ndates,nrow=length(depths),NA) # date-depth matrix
    
    sm <- c()
    for(sids in 1:ndates){
      d0 <- dates[sids]
      if(verbose) cat(paste(Data_Source, id,'- set:',d0,"\n"))

      d <- dates.ch[sids]
      i <- which(ts_sub$date.ch %in% d)
      if(length(i) > 1){
        k <- ts_sub[i,]
        k$Depth <- k[[var]]
        k2 <- plyr::ddply(k[,which(names(k) %in% c('date','Depth','Temperature'))],c("date","Depth"),function(x)c(Temperature=mean(x$Temperature)))
        
        ### old code depending on akima-package: (RchivalTag-package versions < 0.07) :
        # xx <- c(k2$date,k2$date+1)
        # yy <- c(k2$Depth,k2$Depth)
        # zz <- c(k2$Temperature,k2$Temperature)
        # temp <- akima::interp(x=xx,y=yy,z=zz,linear=T,duplicate='mean',yo=depths) # interpolate data per day
        # Temperature_matrix[,sids] <- temp$z[1,]
        
        ### new code: (RchivalTag-package versions >= 0.07) :
        temp <- try(approx(x = k2$Depth, xout = depths, y = k2$Temperature),silent = T)
        if(!is(temp, 'try-error')){
            

        Temperature_matrix[,sids] <- temp$y
        }
        #         dd <- paste0('Date_',format.Date(d,'%Y%m%d'))
        #         df <- data.frame(Depth=depths,Temperature=temp$z[1,],Date=as.Date(d))
        #         out_list[[paste0(Data_Source,'.',id)]][[dd]] <- df[which(!is.na(df$Temperature)),]
      }
      unique_Depths <- unique(k$Depth); unique_Depths <- unique_Depths[order(unique_Depths)]
      nrecs <- .switch_if(!is.null(k$nrecs), sum(k$nrecs), length(k$Depth))
      add <- data.frame(Date=d,nrecs=nrecs, Depths=paste0(unique_Depths,collapse=';'),stringsAsFactors = F)
      sm <- rbind(sm, add)
    }
    if(max(sm$nrecs) > 20) sm$Depths <- c()    
    out[[paste0(Data_Source,'.',id)]] <- list(Temperature_matrix=Temperature_matrix, Depth=depths, Date=dates, sm=sm)
  }
  #   if(!return_as_matrix) out <- out_list
  return(out)
}





