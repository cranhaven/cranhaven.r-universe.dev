

######################################################################
##
## File Name: allRand.R
## Description: complete randomization
## Date Created: 12/6/2024
## Last Updated: 4/16/2025
##
######################################################################

# FUNCTION: allRand
# DESCRIPTION: Complete randomization of aliquots.
# dataR = data for randomization
# batchTot = c(batchTot1, batchTot2) size of two plates, just use one plate per
# batch, batch size inclusive of QC samples
# numQC = number of QC samples per batch
# withinN = number of samples away that the QC samples must be from each other
# numMatch = number of QC samples per plate if two plates per batch or per batch
# if one plate per batch
# chkRep = check if there is a repeat of the groups within the batches

#' Data Randomization
#'
#' @description
#' Randomizes aliquots to batches.
#'
#' @param dataR Data for randomization.
#' @param batchTot c(batchTot1, batchTot2) sizes of plates, just use one plate per batch, batch size inclusive of QC samples.
#' @param numQC Number of QC samples per batch.
#' @param withinN Number of samples away that the QC samples must be from each other.
#' @param numMatch Number of QC samples from a single mother within a batch.
#' @param chkRep Check if there is a repeat of the groups within the batches.
#' @return A dataset with serum order randomized.
#' @examples
#' serumRand <- allRand(dataR=serumMaster3,batchTot=c(40,44), numQC=2,withinN=2,numMatch=2,chkRep=1)
#' @export
allRand <- function(dataR, batchTot, numQC, withinN, numMatch,chkRep){
 QCsamp=ccID=randW=caseControl=acrs=rn=batchN=ct=loc=loc2=NULL
  #separate the files (dataR)
  len <- length(batchTot)
  dataRS <- dataR %>% filter(QCsamp == 0) %>%
    group_by(ccID) %>%
    mutate(randW=runif(1)) %>%
    ungroup() %>%
    arrange(randW,ccID) %>%
    ungroup() %>%
    select(-randW) %>%
    mutate(rn = row_number())

  dataRQ <- dataR %>% filter(QCsamp == 1) %>%
            arrange(ccID,caseControl) %>%
            mutate(rn=row_number())
  batchTotC <- rep(NA,len)
  ctq <- rep(NA,len)

  tot <- sum(unlist(batchTot)) - numQC*len
  battot <- ceiling(nrow(dataRS)/tot)
  lenL <- nrow(dataRS) - (battot-1)*tot
  for (k in 1:len){
    lenL <- lenL - (batchTot[k]-numQC)
    if (lenL >= (-1*(batchTot[k]-numQC))){
      battot2 <- battot
      if (lenL >= 0){
        partl <- batchTot[k]-numQC
      }else{
        partl <- lenL+batchTot[k]-numQC
      }
    }else{
      battot2 <- battot-1
      partl <- batchTot[k]-numQC
    }
    batchTotC[k] <- (batchTot[k]-(numQC))*(battot2-1)+partl
    ctq[k] <- (battot2)*numQC
  }

  #check the across points and test that these are not across the "split" points and
  #re-randomize as necessary
  if (len > 1){
    iter1 <- 0
  repeat{
    iter1 <- iter1 + 1
    if (iter1 > 1000){stop("Function does not converge.")}
    #calculate across
    acr <- 0
    mins <- 1
    for (z in 1:(len-1)){
      maxs <- batchTotC[z] + mins-1
      dataRS2 <- dataRS %>%
        mutate(acrs = case_when(rn %in% c(maxs,(maxs+1)) ~ 1,
                                TRUE ~ 0)) %>%
                 filter(acrs == 1)
      if (dataRS2$ccID[1] == dataRS2$ccID[2]){
        acr = 1
      }
      mins = mins+batchTotC[z]
    }

    #assess if across (acr) or re-randomize
    if (acr == 0){
      break
    }else{
      dataRS <- dataRS %>%
        group_by(ccID) %>%
        mutate(randW=runif(1)) %>%
        ungroup() %>%
        arrange(randW,ccID) %>%
        ungroup() %>%
        select(-randW) %>%
        mutate(rn=row_number())
    }
  }#end repeat
  }#end if

  #call the allRandS function
  #create a single file from all of the randS outputs
  m <- 1
  minq <- 1
  for (i in 1:len){
    batchTotA = batchTot[i]
    max <- batchTotC[i] + m-1
    maxq <- ctq[i] + minq-1
    dataRAS <- dataRS %>% filter((rn <= max)&(rn >= m))
    dataRAQ <- dataRQ %>% filter((rn <= maxq)&(rn >= minq))
    dataRA <- rbind(dataRAS,dataRAQ) %>% select(-rn)
    tempC <- allRandS(dataR=dataRA, batchTot=batchTotA,numQC,withinN,numMatch,chkRep)
    tempC <- tempC %>% mutate(ct = i)
      if (i == 1){
        tempFinA <- tempC
      }else{
        tempFinA <- rbind(tempFinA,tempC)
      }
    m = m+batchTotC[i]
    minq = minq+ctq[i]
  }
  tempFinA <- tempFinA %>%
              arrange(batchN,ct,loc)  %>%
              rename(loc2=loc) %>%
              mutate(loc=0)
  ms <- 0
  for (s in 1:nrow(tempFinA)){
    if (s != 1){
      if (tempFinA$ct[s] == (tempFinA$ct[s-1]+1)){
        ms <- ms+tempFinA$loc2[s-1]
      }else if (tempFinA$ct[s] == 1){
        ms <- 0
      }
    }
    tempFinA$loc[s] = tempFinA$loc2[s]+ms
  }
   tempFinA <- tempFinA %>%
             select(-loc2,-ct)

   dataRQTp <- dataRQ[(maxq+1):nrow(dataRQ),] %>%
                select(-rn) %>%
              mutate(batchN = NA, loc = NA)
   bat <- max(tempFinA$batchN)
   lcs <- tempFinA %>% filter(batchN == bat)
   lc <- max(lcs$loc)
   totA <- sum(unlist(batchTot))
   for (v in 1:nrow(dataRQTp)){
     if (lc == totA){
       bat = bat+1
       lc = 1
     }else{
       lc = lc+1
     }
     dataRQTp$batchN[v] <- bat
     dataRQTp$loc[v] <- lc
   }
  tempFinA <- rbind(tempFinA,dataRQTp) %>% arrange(batchN,loc)

  return(tempFinA)

} #end allRand function
