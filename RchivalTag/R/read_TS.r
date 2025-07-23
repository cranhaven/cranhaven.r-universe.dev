read_TS <- function(ts_file,header=TRUE,sep=",",skip=0,date_format,lang_format="en",tz="UTC"){
  ts_df <- read.table(ts_file, skip=skip,header = header, sep = sep,stringsAsFactors = F)

  if(ncol(ts_df) == 1) print(head(ts_df)) ## required for debugging!
  if(missing(date_format)) date_format <- "%d-%b-%Y %H:%M:%S"
  
  if("Date.Time" %in% names(ts_df)) {
    ts_df$datetime_tmp <- ts_df[["Date.Time"]]
    ts_df[["Date.Time"]] <- c()
  }
  
  if(all(c("Day","Time") %in% names(ts_df))) {
    ts_df$datetime_tmp <- paste(ts_df$Day,ts_df$Time)
    ts_df[["Day"]] <- ts_df[["Time"]] <- c()
  }
  
  if(all(c("Time") %in% names(ts_df))) { ## for archived WC datasets
    ts_df$datetime_tmp <- paste(ts_df$Time)
    ts_df[["Day"]] <- ts_df[["Time"]] <- c()
  }
  
  ii <- which(nchar(ts_df$datetime_tmp) == 1)
  if(length(ii) > 0){
    warning("Omitting records with no datetime information, as shown in the lines above for file: ",ts_file)
    ts_df <- ts_df[-ii,]
  }
  
  datetime <-.fact2datetime(ts_df$datetime_tmp, date_format=date_format, lang_format = lang_format,tz = tz)
  # ts_df$datetime_tmp <- c()
  datetime_nm <- ts_df$datetime_nm <- as.numeric(datetime)
  ts_df$LocationQuality <- ts_df$Latitude <- ts_df$Longitude <- ts_df$datetime_tmp <- c()
  
  date_range_nm <- as.numeric(.date2datetime(range(as.Date(datetime),na.rm=T), tz="UTC",midday = F))
  res <- median(diff(unique(datetime_nm),na.rm=T))
  add <- data.frame(datetime_nm=seq(date_range_nm[1],date_range_nm[2]+24*60*60,by=res))
  add <- data.frame(datetime_nm=add[1:(nrow(add)-1),])
  
  out <- merge(add,ts_df,by="datetime_nm",all.x=T)
  out$DeployID <- DeployID <- unique(ts_df$DeployID[1]); if(length(DeployID) > 1) stop("More than one DeployID found! please revise!")
  out$Serial <- Serial <- ts_df$Serial[1]; if(length(Serial) > 1) stop("More than one Serial found! please revise!")
  out$Ptt <- Ptt <- ts_df$Ptt[1]; if(length(Ptt) > 1) stop("More than one Ptt found! please revise!")
  out$datetime_nm <- .num2datetime(out$datetime_nm,tz = "UTC",hours.offset = F)
  names(out)[1] <- "datetime"
  head(out)
  
  ## convert LOTEK format
  iTemp <- grep("External_Temp",names(out))
  if(length(iTemp) > 0){
    out$Temperature <- out[,iTemp]
    out[,iTemp] <- c()
  }
  
  iDepth <- grep("Pressure",names(out))
  if(length(iDepth) > 0){
    out$Depth <- out[,iDepth]
    out[,iDepth] <- c()
  }
  
  ts_df$datetime_tmp <- c()
  tmp <- out
  out$DeployID <- out$Ptt <- c()
  if(is.null(tmp$Ptt)) tmp$Ptt <- NA
  if(is.null(tmp$DeployID)) tmp$DeployID <- NA
  
  
  #### check for reoccuring time steps:
  tdiff <- diff(as.numeric(out$datetime))
  i <- which(tdiff == 0) ## find duplicate datetime entries
  it <- 1
  while(length(i) > 0){
    # cat("iteration",it,"\n")
    out <- out[-i,] ## remove duplicate datetime entries
    tdiff <- diff(as.numeric(out$datetime))
    i <- which(tdiff == 0)
    it <- it+1
  }
  
  out <- data.frame(DeployID=tmp$DeployID[1], Ptt=tmp$Ptt[1], date=as.Date(out$datetime), out,stringsAsFactors = F)
  return(out)
}
