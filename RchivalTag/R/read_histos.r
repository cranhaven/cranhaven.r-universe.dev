###' todo:
#' merge histos -> seperate function. allow user to select new bins (omit individuals that do not have these bin breaks)

read_histos <- function(hist_file, date_format, lang_format="en", tz="UTC", dep.end,Serial,
                        force_24h=TRUE, min_perc, omit_negatives=TRUE, right_truncate=TRUE){
  hist_dat <- NULL
  if(is.character(hist_file)) {
    x <- readLines(hist_file)
    ii <- which(nchar(x) > 0)
    x <- x[ii]
    hist_dat <- read.table(textConnection(x), header = TRUE, sep = ",")
  }
  if(!missing(Serial)) hist_dat$DeployID <- Serial
  if(is.data.frame(hist_file)) hist_dat <- hist_file
  if(is.null(hist_dat)) stop('Provided hist_file not in allowed formats (data.frame or character string indicating the name of the file to read). Please revise.')
  identifiers <- c('Serial',"DeployID",'Ptt')
  identifiers <- identifiers[which(identifiers %in% names(hist_dat))]
  head(hist_dat)
  
  ID <- c()
  for(id in identifiers){
    ID <- paste(ID, paste(id,hist_dat[[id]],sep="."),sep=.switch_if(id==identifiers[1],'','_'))
  }
  hist_dat <- data.frame(ID=ID,hist_dat,stringsAsFactors = F)
  
  hist_list <- list()
  for(Type in c('TAD','TAT')){
    
    raw_limits <- hist_dat[which(hist_dat$HistType == paste0(Type,"LIMITS")),]
    bin_cols <- which(names(hist_dat) == 'Bin1'):ncol(hist_dat)
    limits <-  plyr::ddply(raw_limits,c("ID",identifiers),function(x) {
      for(i in bin_cols) {
        if(grepl(">",x[,i])) x[,i] <- .switch_if(Type == "TAD",5000,164.75)
        x[,i] <- .fact2num(x[,i]) 
      }
      #   bin.cols <- grep("Bin",names(hist_dat))
      n <- which(!is.na(x[,bin_cols]))
      c(bin_breaks = paste(x[,bin_cols[n]],collapse="; "),
        nbins=length(n))
    })
    
    ### check if identifiers are ok!
    check_limits <-  plyr::ddply(limits, identifiers,function(x)c(occasions=nrow(x)))
    if(any(check_limits$occasions > 1)){
      stop("multiple ",  paste0(Type,"LIMITS"), " found for an unique ",  .switch_if(length(identifiers) > 1, "combination of ","identifier "),paste(identifiers,collapse=", "),
           ". Consider to add column 'Serial' in input dataframe.\n", 
           .print_df(check_limits))
    }
    
    id <- ID[1]
    for(id in unique(ID)){
      ii <- which(limits$ID == id)
      if(length(ii) == 0){
        warning("No ",Type, "-breaks found for tag: ", gsub('\\.',' ',gsub("_"," - ",id)),"\nThis tag was skipped!")
      }else{
        bb <- c(-5,as.numeric(strsplit(limits$bin_breaks[ii],'; ')[[1]]),.switch_if(Type=="TAD",5000,164.75))
        
        hist_list[[Type]][[id]]$bin_breaks <- bb
        add0 <- hist_dat[which(hist_dat$ID == ID & hist_dat$HistType == Type),]
        
        if(missing (date_format)) date_format <- '%H:%M:%S %d-%b-%Y'
        add0$datetime <- .fact2datetime(as.character(add0$Date),date_format = date_format,lang_format = lang_format,tz = tz)
        if(any(is.na(add0$datetime))) stop('Date-vector not in correct format (',date_format,')! Please choose a format corresponding to "',add0$Date[1],'".')
        add0$date <- as.Date(add0$datetime)
        if(!missing(dep.end)){
          add0 <- add0[which(add0$date < dep.end),]
        }
        
        info <- add0[,which(names(add0) %in% c(identifiers,'date','datetime',"Sum"))]
        
        nbins <- length(bb)
        
        madd0 <- add0[,which(names(add0) %in% paste0("Bin",1:nbins))]
        for(icol in 1:ncol(madd0)) madd0[,icol] <- .fact2num(madd0[,icol])
        
        madd0[which(is.na(madd0),arr.ind = T)] <- 0
        
        if(right_truncate){
          lim <- .switch_if(Type == "TAD",2000,45)
          alimits <- which(bb >= lim)
          if(length(alimits) > 1){
            madd0[,alimits[1]] <- rowSums(madd0[,alimits])
            madd0 <- madd0[,-alimits[2:length(alimits)]]
            bb <- bb[-alimits[2:length(alimits)]]
            bb[alimits[1]] <- lim
          }
          if(length(alimits) == 1){
            bb[alimits[1]] <- lim
          }
        }
        
        
        stats <- .get_histos_stats(madd0, bb, omit_negatives)
        hist_list[[Type]][[id]]$bin_breaks <- stats$bin_breaks; 
        add_final <- stats$df
        out <- data.frame(info, add_final, stringsAsFactors = F)
        out$duration <- out$nrec <- NA
        
        if(nrow(out)>1){

          hc <- .diff.time(out$datetime,units = "hours")
          hc <- hc[which(hc %in% c(4,6,12,24))]
          # hc <- .datetime2hour.dc(out$datetime)
          # print(hc)
          # 
          # hc <- hc[which(abs(diff(hc)) %in% c(6,12,24))]
          # 
          out$tstep <- tstep <- abs(pracma::Mode(hc))*60*60 # from the pracma-package

          tstep2 <- diff(as.numeric(c(out$datetime)))
          kk <- which(tstep2 < out$tstep[1])
          if(length(kk) > 0) out$tstep[kk] <- tstep2[kk]
          tstart <- as.numeric(c(out$datetime))
          tend <- as.numeric(c(out$datetime))+out$tstep
          
          out$nperc_dat <-  round(100*(tend-tstart)/(60*60)/24,1)
          out$nperc_dat
          # out$nperc_dat[1] <- 100*(24-.datetime2hour.dc(out$datetime[1]))/24
          out$duration <- tstep <- tstep/(60*60)
          out$tstep <- out$tstep/(60*60)
          out$nrec <- round(tend-tstart,1)/(60*60)
          
          if(out$nperc_dat[1] < 0){
            hc <- .datetime2hour.dc(out$datetime)
            ik <- which(hc[2:(length(hc)-1)] == hc[1])
            if(length(hc[2:(length(hc)-1) == hc[1]])){
              out$nperc_dat[1] <- out$nperc_dat[ik[1]+1]
              out$nrec[1] <- out$nrec[ik[1]+1]
            }
          }
        }else{
          tstart <- as.numeric(c(out$datetime))
          tend <- as.numeric(.date2datetime(out$date[1]+1, midday = F, tz = "UTC"))
          out$nperc_dat <- round(100*(tend-tstart)/(60*60)/24,1)
          tstep <- (tend-tstart)/(60*60)[1]
        }
        
        if(tstep < 24 & force_24h & nrow(out) > 1 & !missing(min_perc)){
          if(min_perc > 0){
            warning("Original ",Type, " histogram data with incompatible time step of ", tstep,"h. Summing up data to 24h intervals\n")
            out2 <- c()
            
            for(d in as.character(unique(out$date))){
              i <- which(out$date == d)

              x <- out[rep(i,out$nperc_dat[i]*100),grep("Bin",names(out))]
              add <- out[i[1],]
              add[,grep("Bin",names(out))] <- apply(x,2,FUN = mean)
              add[which(is.na(add),arr.ind = T)] <- 0
              info <- add[,1:grep("Bin",names(out))[1]-1]
              bb <- hist_list[[Type]][[id]]$bin_breaks
              
              stats <- .get_histos_stats(add[,grep("Bin",names(out))], bb, omit_negatives)
              # hist_list[[Type]][[id]]$bin_breaks <- stats$bin_breaks
              add <- cbind(info,stats$df)
              add$nperc_dat <- sum(out$nperc_dat[i])

              if(any(add$nperc_dat > 120)) stop("Error in combining data. Reaching > 100%")
              if(any(add$nperc_dat > 100)) add$nperc_dat[which(add$nperc_dat > 100)] <- 100
              add$nrec <- sum(out$nrec[i])
              add$duration <- sum(out$duration[i])
              add$tstep <- 24
              
              out2 <- rbind(out2,add)
            }
            out <- out2
          }
        }
        
        hist_list[[Type]][[id]]$df <- out
      }
    }
  }
  
  df_combined <- c()
  for(id in unique(ID)){
    for(Type in c("TAD","TAT")){
      df <- hist_list[[Type]][[id]]$df
      if(!is.null(df)){
        if(!missing(min_perc)) {
          if(min_perc ==0){
            warning("Returning raw data")
          }else{
            
            h <- plyr::ddply(df[,c("DeployID","Ptt","nperc_dat")],c("DeployID","Ptt"),function(x){
              ii <- x$nperc_dat < min_perc
              c(kept=nrow(x[!ii,]),omitted=nrow(x[ii,]))})
            warning(message(paste0("Omitted the following number of ",Type," entries based on min_perc=",min_perc," argument!\n",paste0(capture.output(h), collapse = "\n"))))
            df <- df[which(df$nperc_dat >= min_perc),]
            hist_list[[Type]][[id]]$df <- df
          }
        }
        
        df$Type <- Type
        df_combined <- rbind(df_combined,df[,!grepl("Bin",names(df))])
      }
    }
  }
  h <- plyr::ddply(df_combined[,c("DeployID","Ptt","nperc_dat","Type")],c("DeployID","Ptt","Type"),function(x){n=nrow(x); c(p0_25=round(100*nrow(x[which(x$nperc_dat <= 25),])/n,1),
                                                                                                                            p0_50=round(100*nrow(x[which(x$nperc_dat <= 50),])/n,1),
                                                                                                                            p0_75=round(100*nrow(x[which(x$nperc_dat <= 75),])/n,1),
                                                                                                                            p0_90=round(100*nrow(x[which(x$nperc_dat <= 90),])/n,1))})
  if(any(h$p0_90 > 50) & missing(min_perc)) stop(paste("High percentage of missing data in at least one individual. please revise (e.g. filter with 'min_perc' argument)!\n", 
                                                       message(paste0(capture.output(h), collapse = "\n"))))
  return(hist_list)
}


.print_df <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}


combine_histos <- function(hist_list1,hist_list2){
  for(Type in c('TAD','TAT')){
    nn1 <- names(hist_list1[[Type]])
    nn2 <- names(hist_list2[[Type]])
    
    sstp <- paste("Grouped or merged", Type, 'data of seperate lists can not be combined since double occurences of unique tags can not be verified. Please rerun on ungrouped or unmerged data!')
    if(any(grepl('group',nn1) | grepl('merged',nn1))) {
      warning('ungrouping hist_list1')
      hist_list1 <- unmerge_histos(hist_list1)
      nn1 <- names(hist_list1[[Type]])
    }
    if(any(grepl('group',nn2) | grepl('merged',nn2))){
      warning('ungrouping hist_list2')
      hist_list2 <- unmerge_histos(hist_list2)
      nn2 <- names(hist_list2[[Type]])
    }
    
    if(any(nn2 == nn1)){
      ii <- which(nn2 %in% nn1)
      wwarn <- paste(Type,'-data from tags with ids:\n',paste(nn2[ii],collapse=",\n"), '\nfound in hist_list2, existed already in hist_list1 and will be skipped.')
      options(warning.length = max(c(nchar(wwarn)+10,100)))
      warning(wwarn)
      nn2 <- nn2[-ii]
    }
    for(n in nn2){
      hist_list1[[Type]][[n]] <- hist_list2[[Type]][[n]]
    }
  }
  return(hist_list1)
}


unmerge_histos <- function(hist_list){
  hist_list_new <- list()
  for(Type in c('TAD','TAT')){
    #     print(Type)
    nn <- names(hist_list[[Type]])
    for(n in nn){
      #       n <- nn[1]
      hist_dat <- hist_list[[Type]][[n]]$df
      identifiers <- c('Serial',"DeployID",'Ptt')
      identifiers <- identifiers[which(identifiers %in% names(hist_dat))]
      
      IDs <- c()
      for(id in identifiers){
        IDs <- paste(IDs, paste(id,hist_dat[[id]],sep="."),sep=.switch_if(id==identifiers[1],'','_'))
      }
      
      hist_dat$ID <- IDs
      for(ID in unique(IDs)){
        #         print(ID)
        #         ID <- IDs[1]
        add <- hist_dat[which(hist_dat$ID == ID),]
        add$ID <- c()
        if(ID %in% names(hist_list_new[[Type]])) stop('tag with identifier:\n', gsub('\\.',' ',gsub('_',' - ',ID)),'\nwith more than 1 occurence! Please check list manually!')
        hist_list_new[[Type]][[ID]]$df <- add
        hist_list_new[[Type]][[ID]]$bin_breaks <- hist_list[[Type]][[n]]$bin_breaks
      }
    }
  }
  return(hist_list_new)
}



rebin_histos <- merge_histos <- function(hist_list, tad_breaks=NULL, tat_breaks=NULL, force_merge=FALSE){
  hist_list_new <- list()
  Type <- 'TAD'
  for(Type in c('TAD','TAT')){
    vlim <- .switch_if(Type == "TAD",c(0,5000),c(0,45))
    IDs <- names(hist_list[[Type]])
    if(length(IDs) != 0){
      cat('\n\nmerging',Type,'data:')
      
      #       mm <- matrix(F,ncol = length(vlim[1]:vlim[2]), nrow=length(IDs))
      if(length(IDs) > 1){
        add <- c()
        for(ID in IDs){
          ID_limits <- hist_list[[Type]][[ID]]$bin_breaks
          
          for(ii in 1:length(ID_limits)){
            if(ii == 1 | ID_limits[ii] < vlim[1]) ID_limits[ii] <- vlim[1]
            if(ii == length(ID_limits) | ID_limits[ii] > vlim[2]) ID_limits[ii] <- vlim[2] ## setting last bin break to max(vlim)
          }
          add <- rbind(add, data.frame(ID=ID,bin_breaks= paste(ID_limits,collapse="; "),stringsAsFactors = F))
        }
        
        add.bkp <- add
        add$bin_breaks <- gsub('0; 0; ','0; ',add$bin_breaks)
        add$bin_breaks <- gsub('45; 45','45',add$bin_breaks)
        add$bin_breaks <- gsub('5000; 5000','5000',add$bin_breaks)
      }else{
        ID_limits <- hist_list[[Type]][[IDs]]$bin_breaks
        add.bkp <- add <- data.frame(ID=IDs,bin_breaks= paste(ID_limits,collapse="; "),stringsAsFactors = F)    
        if(is.null(tat_breaks) & Type == "TAT") tat_breaks <- ID_limits
        if(is.null(tad_breaks) & Type == "TAD") tad_breaks <- ID_limits  
      }    
      
      grouped <- plyr::ddply(add,c("bin_breaks"),function(x)c(n_tags=nrow(x),tags=paste(x$ID,collapse="; "))) ## unique unmerged bins
      cat("\nFound the following unique bin breaks for",paste0(Type,"-data:\n"),.print_df(grouped[,1:2]))
      grouped$ID <- paste0("group",1:nrow(grouped))
      
      #     if(force_merge){
      #       if(!is.null(tat_breaks)) warning('Forcing merge on all groups! Ignoring provided ')
      #     }
      
      new_breaks <- tat_breaks
      if(Type == "TAD")  new_breaks <- tad_breaks
      #     if(!is.null(new_breaks)) force_merge <- F
      
      ### option 1: merge groups with common bin_breaks
      if(is.null(new_breaks) & !force_merge){
        for(j in 1:nrow(grouped)){
          IDs <- add$ID[which(add$bin_breaks == grouped$bin_breaks[j])]
          add_group <- c()
          for(ID in IDs){
            hist_dat_id <- hist_list[[Type]][[ID]]$df
            bb_id <- as.numeric(strsplit(add.bkp$bin_breaks[which(add.bkp$ID == ID)],'; ')[[1]])
            #           bb_id[2] <- 0
            info <- hist_dat_id[,which(!grepl('Bin', names(hist_dat_id)))]
            madd0 <- hist_dat_id[,grep('Bin', names(hist_dat_id))]
            
            bb_id_unique <- unique(bb_id); 
            nbbs <- length(bb_id_unique)
            madd <- madd0[,1:nbbs]; madd[,] <- NA
            for(h in 1:nbbs){
              m <- madd0[,which(bb_id == bb_id_unique[h])]
              if(is.data.frame(m)){
                madd[,h] <- rowSums(m,na.rm = T)
              }else{
                madd[,h] <- m
              }
            }
            add_id <- cbind(info,madd)
            add_group <- rbind(add_group,add_id)
          }
          hist_list_new[[Type]][[paste0("group",j)]]$df <- add_group
          hist_list_new[[Type]][[paste0("group",j)]]$bin_breaks <- bb_id_unique
        }
      }
      
      ### option 2: remerge groups with user-specified bin_breaks
      if(!is.null(new_breaks) & !force_merge){
        
        common_bin_breaks <- new_breaks
        bb <- strsplit(grouped$bin_breaks,'; ')
        IDs <- c()
        delete_groups <- c()
        for(bid in 1:nrow(grouped)){
          if(all(common_bin_breaks %in% as.numeric(bb[[bid]]))){
            add_IDs <- strsplit(grouped$tags[bid],'; ')[[1]]
            IDs <- c(IDs,add_IDs)
            delete_groups <- c(delete_groups,grouped$ID[bid])
          }
        }
        for(gg in delete_groups) hist_list_new[[Type]][[gg]] <- c()
        hist_list_new <- .run_merge_hists(IDs=IDs, Type, common_bin_breaks, add, add.bkp, hist_list, hist_list_new)
        
      }
      
      ### option 3: force merging on common bin breaks
      if(force_merge){
        if(!is.null(new_breaks)){
          common_bin_breaks <- new_breaks
          bb_ids <- strsplit(add$bin_breaks,'; ')
          warn_ids <- c()
          for(ii in 1:length(bb_ids)){
            if(!all(common_bin_breaks %in% bb_ids[[ii]])) warn_ids <- c(warn_ids, ii)
          }
          wwarn <- paste0("user-specified ",tolower(Type),'_breaks not found for tags with ID codes:\n',paste(gsub('\\.', ' ', gsub('_',' - ',add$ID[warn_ids])),collapse="\n"),'\nThese tags were omitted!')
          options(warning.length = max(c(nchar(wwarn)+10,100)))
          warning(wwarn)
          
        }else{
          
          nn <- unique(.fact2num(unlist(strsplit(grouped$bin_breaks, '; '))))
          nn <- nn[order(nn)]
          nn
          mm <- as.data.frame(matrix(F,ncol = length(nn), nrow=length(IDs)))
          names(mm) <- paste0('bb',nn)
          oc <- data.frame(ID=IDs,mm,stringsAsFactors = F)
          #       head(oc)
          
          i <- 1
          for(i in 1:nrow(oc)){
            ID <- oc$ID[i]
            bins <- strsplit(add.bkp$bin_breaks[which(add.bkp == ID)],'; ')[[1]]
            bbs <- paste0("bb",bins)
            bb <- bbs[1]
            for(bb in bbs) oc[[bb]][i] <- T
          }
          
          common_bbs <- c()
          for(i in 2:ncol(oc)) if(all(oc[,i])) common_bbs <- c(common_bbs, names(oc)[i])
          common_bin_breaks <- as.numeric(gsub('bb','',common_bbs))
        }
        hist_list_new <- .run_merge_hists(IDs=add$ID, Type, common_bin_breaks, add, add.bkp, hist_list, hist_list_new)
      }
      hist_list[[Type]] <- hist_list_new[[Type]]    
    }
  }
  return(hist_list)
}


.run_merge_hists <- function(IDs, Type, common_bin_breaks, add, add.bkp, hist_list, hist_list_new){
  cat('\nForcing merge on common', Type, 'bin_breaks:\n', common_bin_breaks)
  
  if(length(common_bin_breaks) < 3){
    warning('Less than three common bin_breaks for',paste0(Type,'-data. Can not merge!'))
  }else{
    add_all <- c()
    for(ID in IDs){
      hist_dat_id <- hist_list[[Type]][[ID]]$df
      bb_id <- as.numeric(strsplit(add.bkp$bin_breaks[which(add.bkp$ID == ID)],'; ')[[1]])
      #           bb_id[2] <- 0
      info <- hist_dat_id[,which(!grepl('Bin', names(hist_dat_id)) | names(hist_dat_id) == 'NumBins')]
      madd0 <- hist_dat_id[,which(grepl('Bin', names(hist_dat_id)) & !(names(hist_dat_id) %in% 'NumBins'))]
      
      nbbs <- length(common_bin_breaks)
      madd <- madd0[,1:nbbs]; madd[,] <- NA
      for(h in 1:nbbs){
        m <- madd0[,which(bb_id >= common_bin_breaks[h] & bb_id < c(common_bin_breaks,max(common_bin_breaks)+1)[h+1])]
        if(is.data.frame(m)){
          madd[,h] <- rowSums(m,na.rm = T)
        }else{
          madd[,h] <- m
        }
      }
      add_id <- cbind(info,madd)
      add_all <- rbind(add_all,add_id)
    }
    
    hist_list_new[[Type]][['merged']]$df <- add_all
    hist_list_new[[Type]][['merged']]$bin_breaks <- common_bin_breaks
  }
  return(hist_list_new)
}


.get_histos_stats <- function(df, bin_breaks, correct_negative_values){
  
  df_old <- df
  bin_breaks_old <- bb <- bin_breaks
  
  ii <- which(bb <= 0)
  if(length(ii) > 1){
    df[,ii[1]] <- rowSums(df[,ii])
    df <- df[,-ii[2:length(ii)]]
    names(df) <- paste0("Bin",1:ncol(df))
    bin_breaks <- c(0,bb[-ii])
  }
  if(length(ii) == 1){
    bin_breaks[1] <- 0
  }
  
  df[,length(bin_breaks)] <- NA
  nbins <- length(bin_breaks)-1
  vbins <- paste0("Bin",1:nbins)
  mids <- bin_breaks[1:nbins]+diff(bin_breaks)/2
  
  df$SD <- df$avg <- NA
  for(i in 1:nrow(df)){
    s <- c()
    for(j in 1:length(vbins)){
      t <- df[[vbins[j]]][i]*86 # theoreticaly 8640 depth records per day if sampled every 10s
      s <- c(s,rep(mids[j],t))
    }
    df$avg[i] <- mean(s,na.rm=T)
    df$SD[i] <- sd(s,na.rm=T)
  }
  
  out <- list(df=df,bin_breaks=bin_breaks)
  if(!correct_negative_values){
    if(all(df_old[,ncol(df_old)] == 0)) df_old[,ncol(df_old)] <- NA
    df_old$avg <- df$avg
    df_old$SD <- df$SD
    out <- list(df=df_old,bin_breaks=bin_breaks_old)
  }
  return(out)
}


# .get_histos_stats <- function(df, bin_breaks, keep_WC_format){
#   df_old <- df
#   bin_breaks <- bin_breaks[2:(length(bin_breaks))]
#   df[,1] <- df[,2]+df[,1]
#   df[,2:(length(bin_breaks)-1)] <- df[,3:length(bin_breaks)]
#   df[,length(bin_breaks)] <- NA
#   nbins <- length(bin_breaks)-1
#   vbins <- paste0("Bin",1:nbins)
#   mids <- bin_breaks[1:nbins]+diff(bin_breaks)/2
#   
#   df$SD <- df$avg <- NA
#   for(i in 1:nrow(df)){
#     s <- c()
#     for(j in 1:length(vbins)){
#       t <- df[[vbins[j]]][i]*86 # theoreticaly 8640 depth records per day if sampled every 10s
#       s <- c(s,rep(mids[j],t))
#     }
#     df$avg[i] <- mean(s,na.rm=T)
#     df$SD[i] <- sd(s,na.rm=T)
#   }
#   
#   df_new <- df
#   if(keep_WC_format){
#     df_old$avg <- df$avg
#     df_old$SD <- df$SD
#     df_new <- df_old
#   }
#   return(df_new)
# }