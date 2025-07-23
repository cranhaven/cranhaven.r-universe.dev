

ts2histos <- function(ts_df, tad_breaks=NULL, tat_breaks=NULL, split_by=NULL, aggregate_by="Ptt",min_perc, omit_negatives=TRUE){
  bin_prefix <- "Bin"
  output <- list()
  if(is.null(ts_df$DeployID)) ts_df$DeployID <- NA
  for(Type in c("TAD",'TAT')){
    bin_breaks <- NULL
    
    if(Type == "TAD"){
      field <- 'Depth'
      bin_breaks <- tad_breaks
    }else{
      field <- "Temperature"
      bin_breaks <- tat_breaks
    }
    
    if(field == 'Depth' & any(ts_df$Depth[!is.na(ts_df$Depth)] < 0) & omit_negatives) {
      warning('Depth records < 0 found and treated as 0')
      ts_df$Depth[which(ts_df$Depth < 0)] <- 0
    }
    
    if(!is.null(bin_breaks)){
      if(!omit_negatives & bin_breaks[1] <= 0){
        bin_breaks <- c(-5, bin_breaks)
      }else{
        bin_breaks <- bin_breaks[bin_breaks >=0]
        if(field != "Temperature") bin_breaks <- c(0, bin_breaks)
      }
      bin_breaks <- unique(bin_breaks)
      
      out <- c()
      aggregate_by <- c(aggregate_by, 'DeployID','date')
      if(!('date' %in% names(ts_df)) & ('Day' %in% names(ts_df))) aggregate_by[which(aggregate_by == "date")] <- "Day"
      for(vv in c(field, aggregate_by)) if(!(vv %in% names(ts_df))) stop('Could not find vector:\n', vv, '\nin the data frame provided. Please revise!')
      
      if(length(split_by) > 0) aggregate_by <- c(aggregate_by, split_by)
      #       for(Serial in unique(ts_df$Serial)){
      #         for(d in unique(ts_df$date[ts_df$Serial == Serial])){
      
      ts_df.x <- ts_df#[which(ts_df$Serial == Serial & ts_df$date == d), which(names(ts_df) %in% c(aggregate_by, field))]
      #           tstep <- as.numeric(median(difftime(ts_df.x$datetime[2:nrow(ts_df.x)], ts_df.x$datetime[1:(nrow(ts_df.x)-1)], units="secs")))[1]
      #           print(range(ts_df.x[[field]],na.rm=T))
      
      tres <- min(diff(as.numeric(ts_df$datetime)),na.rm=T)
      sm.df <- plyr::ddply(ts_df.x[,c(aggregate_by,field)], aggregate_by, function(x){
        
        afield <- x[[field]]
        duration <- length(afield)
        afield <- afield[!is.na(afield)]
        h <- hist(afield, breaks=c(bin_breaks), plot=F)
        sm <- round(h$counts*100/sum(h$counts),1)
        sm <- data.frame(matrix(c(sm, NA), nrow=1))
        names(sm) <- paste0(bin_prefix, 1:ncol(sm))
        #             sm$coverage_24h <- 100*sum(h$counts)/(24*60/(tstep/60))
        nrec <- sum(h$counts)
        avg <- mean(x[[field]],na.rm=T)
        SD <- sd(x[[field]],na.rm=T)
        sm <- cbind(Sum=100,sm,avg=avg,SD=SD,nrec=nrec,duration=duration)#NumBins=length(bin_breaks),
      })
      sm.df$nperc_dat <- round(100*sm.df$nrec/sm.df$duration,1)

      sm.df$datetime <- .date2datetime(sm.df$date,tz = "UTC",midday = F)
      if(is.null(sm.df$Ptt)) sm.df$Ptt <- NA
      if(is.null(split_by)) sm.df$tstep <- 24
      out <- rbind(out,  sm.df)
      #         }
      #       }
      output[[Type]][["merged"]] <- list(df=out,bin_breaks=bin_breaks)
      if(!is.null(split_by)) output[[Type]][["merged"]]$split_by <- split_by
    }
  }
  
  df_combined <- c()
  for(Type in c("TAD","TAT")){
    df <- output[[Type]][["merged"]]$df
    if(!is.null(df)){
      if(!missing(min_perc)) {
        if(min_perc ==0){
          warning("Returning raw data")
        }else{
          
          h <- ddply(df[,c("DeployID","Ptt","nperc_dat")],c("DeployID","Ptt"),function(x){
            ii <- x$nperc_dat < min_perc
            c(kept=nrow(x[!ii,]),omitted=nrow(x[ii,]))})
          warning(message(paste0("Omitted the following number of ",Type," entries based on min_perc=",min_perc," argument!\n",paste0(capture.output(h), collapse = "\n"))))
          df <- df[which(df$nperc_dat >= min_perc),]
          output[[Type]][["merged"]]$df <- df
        }
      }
      
      df$Type <- Type
      df_combined <- rbind(df_combined,df[,c("DeployID","Ptt","nperc_dat","Type")])
    }
  }
  h <- ddply(df_combined,c("DeployID","Ptt","Type"),function(x){n=nrow(x); c(nrecs0_25=nrow(x[which(x$nperc_dat <= 25),]),
                                                                             nrecs0_50=nrow(x[which(x$nperc_dat <= 50),]),
                                                                             nrecs0_75=nrow(x[which(x$nperc_dat <= 75),]),
                                                                             nrecs0_90=nrow(x[which(x$nperc_dat <= 90),]),
                                                                             nrecs_all=n,
                                                                             perc0_25=round(100*nrow(x[which(x$nperc_dat <= 25),])/n,1),
                                                                             perc0_50=round(100*nrow(x[which(x$nperc_dat <= 50),])/n,1),
                                                                             perc0_75=round(100*nrow(x[which(x$nperc_dat <= 75),])/n,1),
                                                                             perc0_90=round(100*nrow(x[which(x$nperc_dat <= 90),])/n,1)
                                                                             )})
  if(any(h$perc0_90 > 50) & missing(min_perc)) stop(paste("High percentage of missing data in at least one individual (e.g. nrecs0_25 and perc0_25 correspond to the number and percentage of days or daytime periods with less than 25% of missing data). \nPlease revise (e.g. filter with 'min_perc' argument)!\n", 
                                   message(paste0(capture.output(h), collapse = "\n"))))
  return(output)
}


.ts2tad <- function(ts_df, tad_breaks=NULL, split_by=NULL, aggregate_by='Ptt'){
  out <- ts2histos(ts_df, tad_breaks=tad_breaks, tat_breaks=NULL, split_by=split_by, aggregate_by=aggregate_by)
  output <- out$TAD$merged
  return(output)
}

.ts2tat <- function(ts_df, tat_breaks=NULL, split_by=NULL, aggregate_by='Ptt'){
  out <- ts2histos(ts_df, tat_breaks=tat_breaks, tad_breaks=NULL, split_by=split_by, aggregate_by=aggregate_by)
  output <- out$TAT$merged
  return(output)
}

