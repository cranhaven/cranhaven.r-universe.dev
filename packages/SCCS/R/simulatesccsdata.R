
simulatesccsdata <- function(nindivs, astart, aend, adrug, aedrug, expogrp=c(0), eexpo, washout=NULL, ewashout=NULL,  agegrp=NULL, eage=NULL) {
  
  if (!is.list(eexpo) & length(expogrp)!=length(eexpo)){
    stop("Please provide true relative incidence value for each exposure group")
  } 
  
  
  if (length(astart)==1){
    start <- rep(astart-1, nindivs)
  } else {
    start <- astart-1
  }
  
  if (length(aend)==1){
    end <- rep(aend, nindivs)
  } else {
    end <- aend
  }
  
  
  adrug <- as.matrix(adrug)
  aedrug <- as.matrix(aedrug)
  
  if (nrow(adrug)==1){
    
    adrug <- as.matrix(rep(adrug, nindivs))
  } else {
    
    adrug <- adrug
  }
  
  adrug <- adrug - 1
  
  if (nrow(aedrug)==1){
    
    aedrug <- as.matrix(rep(aedrug, nindivs))
  } else {
    
    aedrug <- aedrug
  }
  
  for (i in 1:ncol(adrug)) {
    
    adrug[,i] <- pmax(adrug[,i], start)
    adrug[,i] <- pmin(adrug[,i], end)
  }
  
  for (i in 1:ncol(aedrug)) {
    
    aedrug[,i] <- pmax(aedrug[,i], start)
    aedrug[,i] <- pmin(aedrug[,i], end)
  }
  
  if (is.list(eexpo) & ncol(adrug)!=length(eexpo)){
    stop("Please provide true relative incidence value for each adrug")
  }
  
  
  if (length(agegrp)==0){
    agegrp <- agegrp
  } else {
    agegrp <- agegrp - 1
  }
  
  if (is.null(washout)) {
    washout <- 0
  } else if(length(washout)==1) {
    
    washout <- 0
    
  } else {
    washout <- washout-1
    washout[length(washout)] <- washout[length(washout)]+1
  }
  
  expo1 <- matrix(0, nrow(adrug), ncol(adrug)*length(expogrp))
  expo2 <- matrix(0, nrow(aedrug), ncol(aedrug)*length(washout))
  expo  <- matrix(0, nrow(adrug), ncol(adrug)*length(expogrp) + ncol(aedrug)*length(washout))
  
  for (i in 1:ncol(adrug)) {
    for (j in 1:length(expogrp)) {
      expo1[,((1*j)+length(expogrp)*(i-1))] <- adrug[,i] + expogrp[j]
      
    }
  }
  
  for (i in 1:ncol(aedrug)) {
    for (j in 1:length(washout)) {
      expo2[,((1*j)+length(washout)*(i-1))] <- aedrug[,i] + washout[j]
      
    }
  }
  
  for (i in 1:ncol(adrug)) {
    expo[,(1+(length(expogrp) + (length(washout)))*(i-1)):((length(expogrp) + (length(washout)))*i)] <- cbind(expo1[,(1+length(expogrp)*(i-1)):(length(expogrp)*i)], expo2[,(1+length(washout)*(i-1)):(length(washout)*i)])
    
  }
  
  
  for(i in 2:ncol(expo)){
    expo[,i-1] <- ifelse(expo[,i] < expo[,i-1], expo[,i], expo[,i-1])
  }
  
  
  #------------------------------------#
  obslength <- (min(aend)-min(astart)) + 1
  
  for (i in 1:ncol(expo)){
    expo[,i] <- pmin(end , expo[,i])
    expo[,i] <- pmax(start , expo[,i])
  }
  
  #-----------------#
  # exposure levels #
  #-----------------#
  
  expolevdiff <- seq(1:((length(expogrp)+length(washout)-1)*ncol(adrug)))     # Exposure levels if different parameters are required for the multiple exposures
  
  for (i in 1:ncol(adrug)) {
    expolevdiff <- append(expolevdiff, 0, after=(length(expogrp)+length(washout)-1)*(i)+(i-1))
  }
  
  expolevsame <- rep(c(seq(1:(length(expogrp)+length(washout)-1)), 0),times=ncol(adrug))  # Exposure levels if same parameters are needed for the different exposures
  
  
  #-------------------------------------- New May 8 2017, Deciding weather sameexpopar= T or F
  
  if (!is.list(eexpo) & length(eexpo)==length(expogrp)){
    sameexpopar=TRUE
    
  } else {
    
    sameexpopar=FALSE
  }
  
  #--------------------------------------#
  
  
  expolev <- if (sameexpopar==TRUE) {
    expolev <- expolevsame
    
    expolev
  } else {
    expolev <- expolevdiff
    
    expolev
  }
  
  # ------------------------------------New May 8 
  ebeta11<-NULL
  if(sameexpopar==TRUE) {
    ebeta1 <- c(eexpo, ewashout)
    
  } else {
    
    for (i in 1:ncol(adrug)){
      
      ebeta11[[i]] <- c(eexpo[[i]], ewashout[[i]])
      
    }
    
    ebeta1 <- unlist(ebeta11)
  }
  
  # --------------------------------------#
  
  exfacs <- expolev
  
  exlist <- expo
  
  ageeffect <- function(agegrp, eage){
    if (is.null(agegrp) & !is.null(eage)){
      
      cutp <- seq((min(start)+1), max(end)) # each time point between 0 and obslength inclusive
      cutr <- rep(cutp, times=nindivs) # Repeat each cuts number of cases times
      #-------------------------------------#
      eagenew <- rep(c(1, eage), nindivs)
      
      # expand into a vector: individual identifier and interval lengths
      # cuts represents the start of a new time window
      startob <- rep(start+1, each=length(cutp))
      endob <- rep(end, each=length(cutp))
      
      adrug <- apply(adrug, 2, function(x) rep(x, each=length(cutp)))
      
      aedrug <- apply(aedrug, 2, function(x) rep(x, each=length(cutp)))
      
      #-------------------------------------------------------------------------------------------#
      
      for (i in 1:ncol(adrug)){
        adrug[,i]<- ifelse(adrug[,i]==endob|aedrug[,i]==startob, NA, adrug[,i])
      }
      
      for (i in 1:ncol(aedrug)){
        aedrug[,i]<- ifelse(aedrug[,i]==startob|adrug[,i]==endob |is.na(adrug[,i]), NA, aedrug[,i])
      }
      
      #-------------------------------------------------------------------------------------------#
      
      indiv <- rep(1:nindivs, each = length(cutp))
      #agegr <-seq(0, obslength) 
      
      #interval <- rep(0, length(cutr))
      
      interval <- ifelse((cutr-startob)<0 | (cutr-endob)>0, 0, 1)
      
      cutr <- cutr[interval!=0]
      indiv <- indiv[interval != 0]
      adrug <- as.matrix(adrug[interval != 0,])
      aedrug <- as.matrix(aedrug[interval != 0,])
      startob <- startob[interval != 0]
      endob <- endob[interval != 0]
      interval <- interval[interval != 0]
      eagenew <- eagenew[interval != 0]
      # age groups
      #agegr <-seq(0, obslength) 
      #agegr <- cut(cuts, breaks = cutp, right  = FALSE, labels = FALSE)
      
      
      
      # exposure groups
      exgr <- rep(0, length(cutr))
      for(i in 1:length(exfacs)) {
        exgr <- ifelse(cutr > exlist[,i][indiv], exfacs[i], exgr)
      }
      
      # event rates per period, given age and exposure effects 
      nevents <- rep(0, length(cutr))
      rate <- rep(1, length(cutr))[indiv]
      for (i in 1:length(ebeta1)) {
        
        rate <- ifelse(exgr == i, rate * ebeta1[i], rate)
      }
      
      rate <- rate*eagenew
      #for(i in 1:length(eage)){
      # rate <- ifelse(cutr == i-1, rate * eage[i], rate)
      #}
      
      # rate <- rate*basel
      
      # multinomial events assigned to time windows, given marginal total
      
      # tot <- rep(0, nindivs)
      #  for(i in 1:nindivs) {
      #  tot[i] <- sum(rate[indiv == i])
      # }
      
      tot <- rowsum(rate, indiv)
      
      mnprob <- rate/tot[indiv]
      
      #----------------------------#
      # True relative incidence    #
      #----------------------------#
      
      # Simulating data sets from multinomial given exposure status and fit SCCS models, repeat the steps z times
      
      
      for(i in 1:nindivs) {
        nevents[indiv == i] <- as.vector(rmultinom(1, 1, mnprob[indiv == i]))
      }
      simdata <- data.frame(cbind(indiv=indiv[nevents==1], astart=startob[nevents==1], adrug=adrug[nevents==1,], aedrug=aedrug[nevents==1,], aend=endob[nevents==1], aevent=cutr[nevents==1]))
      #simdata$astart <- simdata$astart + 1; simdata[,3:(ncol(adrug)+2)] <- simdata[,3:(ncol(adrug)+2)] + 1
      simdata$astart <- simdata$astart; simdata[,3:(ncol(adrug)+2)] <- simdata[,3:(ncol(adrug)+2)] + 1
      
      for (i in 1:ncol(adrug)) {
        colnames(simdata)[i+2] <- paste("adrug",i, sep="")
      }
      
      for (i in 1:ncol(aedrug)) {
        colnames(simdata)[i+2+ncol(adrug)] <- paste("aedrug",i, sep="")
      }
      
      return(simdata)
      
    } else {
      ageb <- agegrp
      eage <- c(1,eage)
      
      # vector of factors for risk intervals
      # leave out first (this will take level 1)
      
      # all cut points as a matrix
      
      if (is.null(ageb)) {
        agebb <- ageb
        
      } else {
        agebb <- matrix(rep(ageb, each=length(start)), ncol=length(ageb))
      }
      
      cutp <- cbind(start, end, agebb, exlist)
      for(i in 3:(length(ageb)+2)){
        cutp[,i] <- pmax(start, cutp[,i])
        cutp[,i] <- pmin(end, cutp[,i])
      }
      for(i in 1:nindivs){cutp[i,] <- sort(cutp[i,])}
      
      # expand into a vector: individual identifier and interval lengths
      # cuts represents the start of a new time window # ex2 and ex3 are not included in the following list as we don't need them in the data set retrieved to fit smooth exposure effect
      startob <- rep(start, each=ncol(cutp))
      endob <- rep(end, each=ncol(cutp))
      # st_risk <- rep(st_risk, each=ncol(cutp))
      adrug <- as.matrix(apply(adrug, 2, function(x) rep(x, each=ncol(cutp))))
      # end_risk <- rep(end_risk, each=ncol(cutp))
      aedrug <- as.matrix(apply(aedrug, 2, function(x) rep(x, each=ncol(cutp))))
      
      #-------------------------------------------------------------------------------------------#
      for (i in 1:ncol(adrug)){
        adrug[,i]<- ifelse(adrug[,i]==endob|aedrug[,i]==startob, NA, adrug[,i])
      }
      
      for (i in 1:ncol(aedrug)){
        aedrug[,i]<- ifelse(aedrug[,i]==startob|adrug[,i]==endob |is.na(adrug[,i]), NA, aedrug[,i])
      }
      
      #-------------------------------------------------------------------------------------------#
      
      
      indiv <- rep(1:nindivs, each = ncol(cutp))
      cuts <- as.vector(t(cutp))
      
      interval <- rep(0, length(cuts))
      for(i in 1:length(cuts)-1) {
        
        interval[i] <- max(cuts[i + 1] - cuts[i], 0)
      }
      # cute represents the end of a time window
      
      cute <- rep(0, length(cuts))
      
      for (i in 1:length(cuts)) {
        cute[i] <- cuts[i] + interval[i]
      }
      
      # remove cut points with 0 interval
      
      cuts <- cuts[interval!=0]
      cute <- cute[interval != 0]
      indiv <- indiv[interval != 0]
      adrug <- as.matrix(adrug[interval != 0,])
      aedrug <- as.matrix(aedrug[interval != 0,])
      startob <- startob[interval != 0]
      endob <- endob[interval != 0]
      interval <- interval[interval != 0]
      
      # age groups
      agegr <- cut(cuts, breaks = c(min(start), ageb, max(end)), right  = FALSE, labels = FALSE) # --------------------#
      
      # exposure groups
      exgr <- rep(0, length(cuts))
      for(i in 1:length(exfacs)) {
        exgr <- ifelse(cuts >= exlist[,i][indiv], exfacs[i], exgr) # was >= now only > to make risk period to be (])
      }
      
      # event rates per period, given age and exposure effects
      nevents <- rep(0, length(cuts))
      rate <- rep(1, length(cuts))[indiv]
      for(i in 1:length(ebeta1)) {   # New code added to account for multiple exposures
        rate <- ifelse(exgr == i, rate * ebeta1[i], rate)
      }
      
      for(i in 1:length(eage)){
        rate <- ifelse(agegr == i, rate * eage[i], rate)
      }
      rate <- rate * interval
      
      # multinomial events assigned to time windows, given marginal total (1 for all individuals in this case as events are independent)
      
      tot <- rep(0, nindivs)
      for(i in 1:nindivs) {
        tot[i] <- sum(rate[indiv == i])
      }
      
      mnprob <- rate/tot[indiv]
      
      
      #----------------------------#
      # True relative incidence    #
      #----------------------------#
      
      # Simulating data sets from multinomial given exposure status 
      
      for(i in 1:nindivs) {
        nevents[indiv == i] <- as.vector(rmultinom(1, 1, mnprob[indiv == i]))
      }
      
      # Simulate event day within each interval where an event occured
      # i.e from a uniform distribution between the two limits of each interval (cuts and cute)
      
      evday <- rep(0, length(cuts))
      # data <- cbind(indiv=rep(indiv[nevents!=0],nevents[nevents!=0]) , startob=startob[nevents!=0], st_risk=st_risk[nevents!=0], end_risk=end_risk[nevents!=0], endob=endob[nevents!=0], expo=exgr[nevents!=0], cuts=cuts[nevents!=0], cute=cute[nevents!=0] eventday=evday[nevents!=0])
      
      
      simdata <-data.frame(cbind(expo=exgr[nevents!=0], cuts=cuts[nevents!=0],cute=cute[nevents!=0], indiv=indiv[nevents!=0], astart=startob[nevents!=0], adrug[nevents!=0,],aedrug[nevents!=0,], aend=endob[nevents!=0], aevent=evday[nevents!=0]))
      
      for(i in 1:nrow(simdata)){
        simdata$aevent[i] <- ceiling(runif(1, simdata$cuts[i] , simdata$cute[i]))
      }
      
      simdata <- data.frame(simdata[,4:ncol(simdata)])
      simdata$astart <- simdata$astart + 1; simdata[,3:(ncol(adrug)+2)] <- simdata[,3:(ncol(adrug)+2)] + 1
      
      for (i in 1:ncol(adrug)) {
        colnames(simdata)[i+2] <- paste("adrug",i, sep="")
      }
      
      for (i in 1:ncol(aedrug)) {
        colnames(simdata)[i+2+ncol(adrug)] <- paste("aedrug",i, sep="")
      }
      
      #------------------------------------------#
      # Put NA's for the adrug ==aedrug==aend    #
      #------------------------------------------#
      
      return(simdata)
      
    }
  }
  
  simdata <- ageeffect(agegrp, eage)
  
  return(simdata)
  
}
