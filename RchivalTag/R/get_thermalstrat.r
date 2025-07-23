get_thermalstrat <- function(x, dz=20, strat_lim=100, na.rm=FALSE, verbose=TRUE, Depth_res, all_info=FALSE){
  
  ### script follows max. negative slope method as explained in Fielder, 2010
  ### Temperature data must be lineary interpolated before execution
  #library(plyr)
  if(!is.list(x)){
    stop('x in wrong input format. Please revise (consder to run "interpolate_TempDepthProfiles" first')
  }
  
  Temperature_matrix <- x$Temperature_matrix  
  
  if(is.null(Temperature_matrix)){
    out <- c()
    for(id in names(x)){
      tag_thermo <- get_thermalstrat(x[[id]], Depth_res=Depth_res, dz=dz, strat_lim=strat_lim, na.rm=na.rm, verbose=verbose,all_info=all_info)
      tag_thermo$ID <- id
      out <- rbind(out, tag_thermo)
    }
  }else{
    
    Depth <- x$Depth
    if(is.null(x$Date) & !is.null(x$Day)) x$Date <- x$Day
    was.Date <- (class(x$Date) == "Date")[1]
    if(missing(Depth_res)) Depth_res <- unique(diff(Depth))
    dZ <- dz/Depth_res
    
    tcline <- tgrad <- c()
    
    matrix.test <- suppressWarnings(apply(Temperature_matrix, 2, max, na.rm=T))
    ii <- which(is.finite(matrix.test))
    
    ndays <- ncol(Temperature_matrix)
    mld <- mld_0.5 <- mld_0.8 <- tgrad <- tcline <- maxDepth_interp <- rep(NA, ncol(Temperature_matrix)) # dummy vectors to be filled up
    out <- c()
    for(i in ii){
      if(verbose) cat(paste0(round(i*100/ndays, 1), "% done\n"))
      j <- which(!is.na(Temperature_matrix[, i]))
      maxDepth_interp[i] <- max(j)*Depth_res
      if(length(j) > dZ){
        j <- j[1:(length(j)-dZ)] # define single window steps
        j <- j[which(j > 0)]
        grads <- c()
        for(jj in j){ # move window along Temperature profile
          dT <- Temperature_matrix[jj, i]-Temperature_matrix[jj+dZ, i]
          mean.depth <- mean(c(Depth[jj], Depth[jj+dZ]))
          add <- data.frame(dT=dT, Depth=mean.depth)
          grads <- rbind(grads, add)
        }    
        d <- mean(grads$Depth[which(grads$dT == max(grads$dT))])
        tgrad[i] <- max(grads$dT)
        tcline[i] <- d
        
        d2 <- min(grads$Depth[which(grads$dT == max(grads$dT))]) # select depth of beginning of maximum gradient
        mld[i] <- d2-dz/2
      }
      temp_interp2 <- interpolate_TempDepthProfiles(ts = data.frame(Depth=Depth[j], Temp=Temperature_matrix[j, i], date=x$Date[i]),
                                                    Temp_field = 'Temp', #return_as_matrix = T,
                                                    Depth_res = 1, verbose = F)[[1]]
      temp_mld <- data.frame(Depth=temp_interp2$Depth, Temp=temp_interp2$Temperature_matrix)
      temp_mld <- temp_mld[which(!is.na(temp_mld$Temp)), ]
      
      ct0.5 <- abs(temp_mld$Temp-(temp_mld$Temp[1]-0.5))
      kk <- which(ct0.5 == min(ct0.5))
      if((length(ct0.5) > 0) & (length(kk) > 0)) mld_0.5[i] <- temp_mld$Depth[kk][1]  # temperature criterion of Monterey and Levitus (1997)
      
      ct0.8 <- abs(temp_mld$Temp-(temp_mld$Temp[1]-0.8))
      kk <- which(ct0.8 == min(ct0.8))
      if((length(ct0.8) > 0) & (length(kk) > 0)) mld_0.8[i] <- temp_mld$Depth[kk][1]  # temperature criterion of Kara et al. (2000, 2003)          
      
      #     plot(grads$dT, grads$Depth, ylim=rev(range(grads$Depth)), main='Temp gradient per Depth')
      #     abline(h=tcline)
      
      #     plot(Temperature_matrix[, i], Depth, ylim=rev(range(Depth[j])), main='Temp per Depth')
      #     abline(h=tcline)
    }
    
    out0 <- data.frame(Date=x$Date, maxDepth_interp=maxDepth_interp, tgrad=tgrad, tcline=tcline, dz=dz,stringsAsFactors = F)
    out0$Date <- as.character(out0$Date)
    
    if(all_info){
      x$sm$Date <- as.character(x$sm$Date)
      out0 <- merge(x$sm, out0, by="Date",all=T)
      if(max(out0$nrecs) > 20) out0$Depths <- c()
    }
    
    out0$mld <- mld
    out0$mld_0.5 <- mld_0.5
    out0$mld_0.8 <- mld_0.8
    
    di <- which(Depth <= strat_lim) # NA not treated ...
    if(ncol(Temperature_matrix) > 1) {
      SI <- apply(Temperature_matrix[di, ], 2, function(x)sd(x, na.rm=T))
    }else{
      SI <- sd(Temperature_matrix[di], na.rm=T)
    }
    
    strat <- data.frame(Date=x$Date, strat_index=SI, strat_lim=strat_lim,stringsAsFactors = F)
    strat$Date <- as.character(strat$Date); 
    out <- merge(out0, strat, by='Date', all.y=T)
    if(was.Date) out$Date <- as.Date(out$Date)
    if(na.rm) out <- out[which(!is.na(out$tcline)), ]
  }
  ### mixed layer is just depicting the upper point, tcline should be half way to the tcline
  return(out)
}