check_ts <- function(sstring='*.gz',folder=".",output=F){
  
  #### check if unique time series was selected
  check0 <- check <- check_gzfiles(sstring,folder)
  if(nrow(check0) > 1) {
    check[,6] <- paste0(check[,6],check[,8])
    check <- cbind(files=check[,7],check[,1:6])
    for(i in 1:ncol(check)) check[,i] <- as.character(check[,i])
    nn <- nchar(check[,1])
    for(i in 1:nrow(check)){
      check[i,1] <- paste0(paste(rep(" ",1+max(nn)-nn[i]),collapse=''),check[i,1])
    }
    check <- rbind(names(check),check)
    a <- paste(check[,1],"\t",check[,2],"\t",check[,3],"\t",check[,4],"\t",check[,5],"\t",check[,6],"\t",check[,7],"\t")
    stop("\nsearch string corresponds to multiple time series\n",paste(a,collapse='\n'))
  }
  
  #### check if daily time series selected
  if(check0$ts != "1d") warning("\n no daily time series selected\n")
    folder <- paste0(folder, "/"); folder <- gsub('//','/',folder)
    sstring <- paste0(folder,sstring)

  n <- Sys.glob(sstring)
  files.split <- name_split(gsub(folder,'',n))
  
  
  #### check for missing dates in the time series
  dates <- as.Date(files.split$date1,"%Y%m%d")
  date.range <- range(dates)
  all.dates <- as.Date(min(dates):max(dates),origin="1970-01-01")
  
  m.id <- which(!(all.dates %in% dates))
  missing.dates <- all.dates[m.id]
  
  cat(paste0('\ndate range:\n',paste(date.range,collapse=' : ')))
  cat(paste0('\n',length(missing.dates),' missing dates:\n',paste(missing.dates,collapse=', ')))
  
  if(output) return(missing.dates)
}

