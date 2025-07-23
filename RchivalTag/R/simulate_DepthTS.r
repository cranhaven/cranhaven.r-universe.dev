simulate_DepthTS <- function(ts_df, ndays=10, gaps=TRUE, trate=90, random_Depth=TRUE, ref_Depth_lim=300, random_Depth_lim=c(100, 700)){
  days <- sample(unique(ts_df$date),size = ndays,replace = T)
  out <- c()
  dep_start <- sample((Sys.Date()-10*365):Sys.Date(),1)
  dates <- as.Date(dep_start:(dep_start+ndays-1),origin="1970-01-01")
  
  nmessage <- 0
  for(i in 1:ndays){
    d <- days[i]
    add <- ts_df[which(ts_df$date == d),c("DeployID", "Serial","Ptt", "date","datetime", "Depth")]
    add$datetime <- .fact2datetime(gsub(d,dates[i],add$datetime))
    ii <- which(add$Depth < ref_Depth_lim)
    if(length(ii) > 0 & random_Depth) {
      add$Depth[ii] <- add$Depth[ii]*runif(min = min(random_Depth_lim)/100,max=max(random_Depth_lim)/100,length(ii))*100/ref_Depth_lim
    }
    add$date <- dates[i]
    add$message <- ceiling((1:nrow(add))/48)+nmessage
    nmessage <- max(add$message)
    out <- rbind(out,add)
  }
  messages <- unique(out$message)
  nmessages_total <- length(messages)
  
  ngaps <- trunc(nmessages_total-nmessages_total*trate/100)
  gaps <- sample(messages,ngaps)
  out$Depth[which(out$message %in% gaps)] <- NA
  out$message <- c()
  out$Ptt <- sample(100000:180000,1)
  out$Serial <- out$DeployID <- paste0(sample(10:19,1),sample(LETTERS,1),sample(0:3,1),sample(100:999,1))
  
  return(out)
}