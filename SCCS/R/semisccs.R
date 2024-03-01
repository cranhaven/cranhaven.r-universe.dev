# This function fits the semi-parametric SCCS method where the   #
# age related relative incidence function is left unspecified    #
#          Farrington C.P and Whitaker H.J 2006                  #

semisccs <- function(formula, indiv, astart, aend, aevent, adrug, aedrug, expogrp = list(), washout = list(), 
                     sameexpopar = list(), dataformat="stack", data) {
  
  
  if (dataformat!="multi" & dataformat!="stack"){
    
    stop("Please input dataformat as multi or stack")
  }
  
  yon <- deparse(substitute(adrug)) 
  yon1 <- as.formula(paste("z", "~", yon)) 
  adrugcolnames <- all.vars(yon1, functions = FALSE, unique = TRUE)[-1] 
  # colname  <- deparse(substitute(adrug))
  adrug  <- eval(substitute(adrug), data, parent.frame())
  
  # Changing adrug to a list if given as cbind(adrug1, adrug2,...) or adrug not as a list
  
  if ((dataformat=="multi" & !is.null(ncol(adrug)))) {
    adrug <- data.frame(adrug)
    adrug <- list(adrug) 
  } else if (dataformat=="stack" & !is.null(ncol(adrug))){
    adrug <- data.frame(adrug)
    adrug1 <- list()
    for (i in 1:ncol(adrug)){
      adrug1[[i]] <- adrug[,i]
    }
    adrug <- adrug1
  } else if (length(adrugcolnames)==1 & length(adrug)!=1) {
    adrug <- list(adrug)
    
  } else {
    adrug <- adrug
  }
  
  
  for (i in 1:length(adrug)){
    adrug[[i]] <- data.frame(adrug[[i]])
  }
  
  ncoladrug <- NULL
  for (i in 1:length(adrug)){
    ncoladrug[i] <- ncol(adrug[[i]]) 
  }
  
  
  for (i in 1:length(adrug)) {
    colnames(adrug[[i]]) <- adrugcolnames[c(1, cumsum(ncoladrug)+1)[-(length(ncoladrug)+1)][i]:cumsum(ncoladrug)[i]]
  }
  
  colname  <- adrugcolnames
  
  
  
  indiv  <- eval(substitute(indiv), data, parent.frame())
  astart <- eval(substitute(astart), data, parent.frame())
  aend   <- eval(substitute(aend), data, parent.frame())
  aevent <- eval(substitute(aevent), data, parent.frame())
  # adrug  <- eval(substitute(adrug), data, parent.frame())
  aedrug <- eval(substitute(aedrug), data, parent.frame())
  
  
  # Changing aedrug to a list if given as cbind(aedrug1, aedrug2,...) or aedrug not as a list
  
  if ((dataformat=="multi" & !is.null(ncol(aedrug)))) {
    aedrug <- data.frame(aedrug)
    aedrug <- list(aedrug) 
  } else if (dataformat=="stack" & !is.null(ncol(aedrug))){
    aedrug <- data.frame(aedrug)
    aedrug1 <- list()
    for (i in 1:ncol(aedrug)){
      aedrug1[[i]] <- aedrug[,i]
    }
    aedrug <- aedrug1
  } else if (length(adrugcolnames)==1 & length(aedrug)!=1) {
    aedrug <- list(aedrug)
    
  } else {
    aedrug <- aedrug
  }
  
  # ------------------Getting fixed covariates from the formula ------------#
  
  qq <- all.vars(as.formula(formula))[-c(which(all.vars(as.formula(formula))=="event"))]
  
  if (length(qq)==0) {
    cov <- cbind()
  }   else {
    cova <- qq[is.na(match(qq, colname))]
    cov <- data.frame(data[, cova])
    colnames(cov) <- cova
  }
  
  # adrug <- (adrug)-1 this is done to include the day of exposure starts in the risk period ie [adrug, aedrug]
  for (i in 1:length(adrug)) {
    adrug[[i]] <- adrug[[i]]-1
    
  }
  
  for (i in 1:length(aedrug)) {
    aedrug[[i]] <- data.frame(aedrug[[i]])
  }
  
  
  
  # Exposure periods cut points ###
  if (length(expogrp)==0) {
    for (i in 1:length(adrug)){
      expogrp[[i]] <- 0
    }
    
  } else if(length(adrug)==1 & length(expogrp)>1) {
    expogrp <- list(c(expogrp))
  } else {
    expogrp <- expogrp
  }
  
  # Washout period cut points ###
  
  
  if (length(washout)==0) {
    for (i in 1:length(adrug)){
      washout[[i]] <- 0
    }
    
  } else if(length(adrug)==1 & length(washout)>1) {
    washout <- list((c(washout)-1))
    washout[[1]][length( washout[[1]])] <- washout[[1]][length( washout[[1]])]+1
    
  } else {
    for (i in 1:length(adrug))  
      washout[[i]][1:(length(washout[[i]])-1)] <- washout[[i]][1:(length(washout[[i]])-1)]-1
  }
  
  # sameexpopar ###
  
  if (length(sameexpopar)==0) {
    for (i in 1:length(adrug)){
      sameexpopar[[i]] <- TRUE
    }
  } else {
    
    sameexpopar <- sameexpopar 
  }
  
  
  # combine all the variables but exposures
  data1 <- data.frame(unique(cbind(indiv, aevent, astart, aend, cov)))
  data1$astart <- data1$astart-1
  
  # New 06-11-2021 sorting data1 by indiv
  data1 <- data1[order(data1$indiv), ]
  #---------------------------------------# 
  
  if (dataformat=="stack") {
    # List of all adrug matrices
    adrug_all <- list()
    
    for (i in 1:length(adrug)) {
      adrug_all[[i]] <- adrug_matrix(indiv, aevent, adrug[[i]]) 
    }
    
    # List of all aedrug matrices
    
    aedrug_all <- list()
    
    for (i in 1:length(aedrug)) {
      aedrug_all[[i]] <- adrug_matrix(indiv, aevent, aedrug[[i]]) 
    }
    
  } else if (dataformat=="multi") {
   # adrug_all <- adrug
   # aedrug_all <- aedrug
    
    # 06-11-2021 changed the above two lines (184 and 185) as below
    adrug_all <- list()
    for (i in 1:length(adrug)) {
      adrug_all[[i]] <- cbind(indiv, aevent, adrug[[i]])
      adrug_all[[i]] <- data.frame(adrug_all[[i]][order(adrug_all[[i]][,1],adrug_all[[i]][,3]), -c(1,2)])
    }
    aedrug_all <- list()
    for (i in 1:length(aedrug)) {
      aedrug_all[[i]] <- cbind(indiv, aevent, aedrug[[i]])
      aedrug_all[[i]] <- data.frame(aedrug_all[[i]][order(aedrug_all[[i]][,1],aedrug_all[[i]][,3]), -c(1,2)])
      
    } 
    
    
  } else {
    stop("dataformat should be multi or stack")
  } 
  # adrug and aedrug + expogrps and washout periods
  expo1 <- list()   # for exposure groups
  expo2 <- list()   # for washout periods 
  expo  <- list()    # expo1 and expo2 both together
  
  # filling values of expo1
  
  for (i in 1:length(adrug)) {
    
    expo1[[i]] <- matrix(, nrow(adrug_all[[i]]), ncol(adrug_all[[i]])*length(expogrp[[i]]))
    
  } 
  
  
  for (i in 1:length(adrug)) {
    for (k in 1:ncol(adrug_all[[i]])) {
      for (j in 1:length(expogrp[[i]])) {
        expo1[[i]][, ((1 * j) + length(expogrp[[i]]) * (k - 1))] <- adrug_all[[i]][, 
                                                                                   k] + expogrp[[i]][j]
      }
    }
  }
  
  # filling values of expo2 
  
  for (i in 1:length(adrug)) {
    
    expo2[[i]] <- matrix(, nrow(aedrug_all[[i]]), ncol(aedrug_all[[i]])*length(washout[[i]]))
    
  } 
  
  
  for (i in 1:length(adrug)) {
    for (k in 1:ncol(aedrug_all[[i]])) {
      for (j in 1:length(washout[[i]])) {
        expo2[[i]][, ((1 * j) + length(washout[[i]]) * (k - 1))] <- aedrug_all[[i]][, 
                                                                                    k] + washout[[i]][j]
      }
    }
  }
  
  
  # Combine expo1 and expo2 inside expo
  
  for (i in 1:length(adrug)) {
    expo[[i]] <- matrix(, nrow(adrug_all[[i]]), ncol(adrug_all[[i]]) * length(expogrp[[i]]) + 
                          ncol(aedrug_all[[i]]) * length(washout[[i]]))
  }
  
  
  for (i in 1:length(adrug)) {
    for (k in 1:ncol(adrug_all[[i]])) {
      expo[[i]][, (1 + (length(expogrp[[i]]) + (length(washout[[i]]))) * (k - 
                                                                            1)):((length(expogrp[[i]]) + (length(washout[[i]]))) * k)] <- cbind(expo1[[i]][, 
                                                                                                                                                           (1 + length(expogrp[[i]]) * (k - 1)):(length(expogrp[[i]]) * 
                                                                                                                                                                                                   k)], expo2[[i]][, (1 + length(washout[[i]]) * (k - 1)):(length(washout[[i]]) * 
                                                                                                                                                                                                                                                             k)])
    }
  }
  
  
  #      data1 <- data.frame(unique(cbind(indiv, aevent, astart, aend)))
  
  # Changing NA's to aend 
  for (i in 1:length(expo)) {
    for (j in 1:ncol(expo[[i]])){
      expo[[i]][,j] <- ifelse(is.na(expo[[i]][,j]), data1$aend, expo[[i]][,j])
    }
  }
  ##########################################################################################
  
  
  # exposure risk periods new periods take precedence
  
  for (i in 1:length(adrug)) {
    for (k in 2:ncol(expo[[i]])) {
      expo[[i]][, k - 1] <- ifelse(expo[[i]][, k] < expo[[i]][, k - 1], expo[[i]][, 
                                                                                  k], expo[[i]][, k - 1])
    }
  }
  
  
  # Replace values greater than aend and less than astart by aend and astart respectively
  
  
  for (i in 1:length(expo)) {
    for (j in 1:ncol(expo[[i]]))
      expo[[i]][, j] <- pmin(data1$aend, expo[[i]][, j])
    expo[[i]][, j] <- pmax(data1$astart, expo[[i]][, j])
  }
  
  # Setting exposure levels
  
  if (dataformat=="stack"){
    expolev <- list()
    
    for (i in 1:length(adrug)) {
      expolev[[i]] <- rep(c(seq(1:(length(expogrp[[i]]) + length(washout[[i]]) - 
                                     1)), 0), times = ncol(adrug_all[[i]]))
    }
  } else {
    
    expolevdiff <- list()
    expolevsame <- list()
    
    # differenet exposure parameters
    
    for (i in 1:length(adrug)){
      expolevdiff[[i]] <- seq(1:((length(expogrp[[i]]) + length(washout[[i]]) - 
                                    1) * ncol(adrug_all[[i]]))) 
      
    }
    
    for (i in 1:length(adrug)) {
      for (k in 1:ncol(adrug_all[[i]])){
        expolevdiff[[i]] <- append(expolevdiff[[i]], 0, after = (length(expogrp[[i]]) + length(washout[[i]]) - 1) * (k) + (k - 1))
        
      }
    }
    
    # Same exposure parameters 
    for (i in 1:length(adrug)) {
      expolevsame[[i]] <- rep(c(seq(1:(length(expogrp[[i]]) + length(washout[[i]]) - 
                                         1)), 0), times = ncol(adrug_all[[i]]))
    }
    
    
    expolev <- list()
    
    for (i in 1:length(adrug)) {
      
      expolev[[i]] <- if (sameexpopar[i] == TRUE) {
        expolev[[i]] <- expolevsame[[i]]
        #return
        #expolev[[i]]
      } else {
        expolev[[i]] <- expolevdiff[[i]]
        #return
        #expolev[[i]]
      }
    }       
  }
  
  ncolexpo <- NULL
  for (i in 1:length(adrug)){
    ncolexpo[i] <- ncol(expo[[i]]) 
  }
  
  
  
  fdata <- data.frame(data1) # ****** New *****
  fdata$indiv1 <- seq(1:nrow(fdata))          # ****** New ***** to make repeat events as different cases because # in SCCS repeated events are independent 
  ncases <-nrow(fdata) # number of cases  (as all events have been given separate indiv)
  
  
  # *******************************************************************#  
  
  eventday1 <- sort(unique(fdata$aevent)) # sort the distinct event days 
  n <- length(eventday1)  # Number of distict event days
  eventday1 <- rep(eventday1, times=ncases) # Replicate all the event days as much as the number of events
  
  diagnosis <-rep(fdata$aevent, each=n) # Event days for each  individual replicated
  
  # Number of events per row
  
  event <- rep(0, times = ncases*n)
  event <- ifelse(eventday1==diagnosis, 1, event)
  
  # Age indicators
  # age <- rep(0:(n-1), times = ncases)
  
  # Risk periods
  # expo <- rep(expo, each=n)
  
  #expof <- matrix(0, ncases*n, ncol(expo))
  # for (i in 1:ncol(expof)) {
  #   expof[,i] <- rep(expo[,i], each=n)
  # }
  
  # ex1 <- rep(fdata$st_risk, each=n)
  # ex2 <- rep(fdata$end_risk, each=n)
  
  
  # Idividual id numbers
  
  indivL <- rep(fdata$indiv1, each=n)
  
  
  keep <- fdata$astart[indivL]>=eventday1|fdata$aend[indivL]<eventday1
  indivL <- indivL[keep==0]
  eventday1 <- eventday1[keep==0]
  event <- event[keep==0]
  
  
  # risk <- rep(0, times = ncases*n)
  # risk <- rep(0, times = lenghth(event))
  
  # Setting exposure levels 
  exgr <- matrix(0, nrow=length(event), ncol=length(adrug))
  exgr <- data.frame(exgr)
  
  for (i in 1:length(adrug)) {
    for (k in 1:ncol(expo[[i]])){
      exgr[,i] <- ifelse(eventday1 > expo[[i]][, k][indivL], expolev[[i]][k], exgr[,i])
    }
  }
  
  for (i in 1:ncol(exgr)){
    exgr[,i] <- as.factor(exgr[,i])
  }
  
  colnames(exgr) <- adrugcolnames[(c(0,cumsum(ncoladrug))+1)[-(ncol(exgr)+1)]] 
  
  exgr1 <- cbind(exgr, trial=rep(1, time=nrow(exgr)))
  
  # finaldata <- cbind(indiv, eventday1, expof, event, age, risk)
  finaldata <- cbind(indivL=indivL, age=eventday1, event=event, exgr1)    
  finaldata  <- finaldata[,-ncol(finaldata)]
  rownames(finaldata) <- seq(1:nrow(finaldata))
  rownames(data1) <- seq(1:nrow(data1))
  
  
  finaldata <- data.frame(cbind(finaldata, apply(data1, 2, function(x) rep(x, 
                                                                           times = data.frame((table(finaldata$indivL)))$Freq))))
  
  data2 <- data.frame(unique(finaldata))
  data3 <- data.frame(data2[order(data2$indivL, data2$age),]) # sorting the data by indiv and age to get rid of repeated age groups for an individual
  
  finaldata<- data.frame(unique(data3))
  
  
  
  fmla <- paste(formula, "+", "factor(age)", "+", "strata(indivL)")
  fmla1 <- as.formula(paste("event~", fmla[3]))
  mod <- clogit(formula = fmla1, data = finaldata)
  summary <- summary(mod)
  return(summary)
  
  # model <- clogit(event ~ exposure + strata(indiv) + factor(day) , data = finaldata)
  # result <- summary(model)
  # ri <- c((result$coef)[1,2],result$coef[1,3])
  #return
  # ri
  # result
}