read_PDT <- function(pdt_file, folder, sep=",",date_format,lang_format="en",tz="UTC"){
  
  options(warn=0)
  pdt_all <- c()
  if(!missing(folder)) pdt_file <- paste0(paste0(folder,"/"),pdt_file)
  
  for(f in pdt_file){
    cat('running pdt_file',f,"\n")
    
    #### check for header line:
    skip <- -1
    header_found <- F
    while(!header_found){
      skip <- skip +1
      header0 <- as.character(unlist(read.delim(f,sep=sep,header=F,nrows=1,skip=skip,stringsAsFactors=F)))
      header_found <- any(grepl("MaxTemp",header0))
    }
    
    #### recreate extended header for PDT file:
    repcols <- c("Depth1", "MinTemp1", "MaxTemp1", "%Ox1","Discont1")
    start_repcols <- c()
    keep <- c() ## check which of the repeated column names exist in the pdt header
    for(n in repcols) {
      start_repcols <- min(c(start_repcols,grep(n,header0)))
      if(any(grepl(n,header0))) keep <- c(keep, n)
    }
    repcols <- keep
    header <- header0[1:(start_repcols-1)]
    for(i in 1:50){
      add_cols <- paste0(gsub(1,i,repcols))
      header <- c(header,add_cols)
    }
    
    #### read in pdt data with extended header:
    add_pdt0 <- read.table(f,header=F,skip=skip+1,sep=sep,fill=T,col.names=header,stringsAsFactors = F)
    if(add_pdt0$DeployID[1] == "DeployID") add_pdt0 <- read.table(f,header=F,skip=skip+2,sep=sep,fill=T,col.names=header,stringsAsFactors = F)
    add_pdt0 <- cbind(pdt_file=f,add_pdt0)
    add_pdt0 <- add_pdt0[which(add_pdt0$NumBins != "NumBins"),]
    
    j <- which(grepl('Ox',names(add_pdt0))| grepl('Disc',names(add_pdt0)))
    if(length(j) > 0) add_pdt0 <- add_pdt0[,-j]
    
    ### reorganize and add required data
    for(i in 1:nrow(add_pdt0)){
      nbins <- add_pdt0$NumBins[i]
      #       print(round(100*i/nrow(add_pdt0),1))
      info <- add_pdt0[i,c("pdt_file","DeployID",'Ptt','Date',"NumBins")]
      for(j in 1:nbins){
        add_pdt <- cbind(info,
                         Depth= .fact2num(add_pdt0[[paste0("Depth",j)]][i]),
                         MinTemp= .fact2num(add_pdt0[[paste0("MinTemp",j)]][i]),
                         MaxTemp= .fact2num(add_pdt0[[paste0("MaxTemp",j)]][i]))
        pdt_all <- rbind(pdt_all,add_pdt)              
      }
    }
  }
  if(missing(date_format)) date_format <- "%H:%M:%S %d-%b-%Y"
  pdt_all$datetime <-.fact2datetime(pdt_all$Date, date_format=date_format, lang_format = lang_format,tz = tz)
  pdt_all$date <- as.Date(pdt_all$datetime)
  
  # check if date conversion worked out
  ii <- which(is.na(pdt_all$date))
  if(length(ii) > 0){
    warning("Column 'Date' of pdt_file(s):\n",paste(unique(pdt_all$pdt_file[ii]),collapse=",\n"),
         "\nnot in correct format (',date_format,').\nPlease choose a format corresponding to '",pdt_all$date[1],"'.")
  }else{
    pdt_all$Date <- c()
  }
  
  pdt_all <- pdt_all[which(!is.na(pdt_all$Depth)),]
  pdt_all$MeanPDT <- (pdt_all$MinTemp+pdt_all$MaxTemp)/2
  
  return(pdt_all)
}


