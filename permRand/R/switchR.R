

######################################################################
##
## File Name: switchR.R
## Description: switching to accommodate certain rules
## Date Created: 12/6/2024
## Last Updated: 5/23/2025
##
######################################################################

## FUNCTION: switchR
## Description: Minimizes switches without complete re-randomizing.
#dataIn: randomized dataset
#numqc: number of QC samples per set
#numqcM: number of qc matching samples
#batchS: new batch size

#' Switching Generating Function
#'
#' @description
#' Minimizes switches without completely re-randomizing the locations.
#'
#' @param dataIn Randomized dataset.
#' @param numqc Number of QC samples per set.
#' @param numqcM Numberof QC matching samples.
#' @param batchS New batch size.
#' @return A dataset with switches indicated.
#' @examples
#' serumSwitch <- switchR(dataIn=serumRand,numqc=2,numqcM=2,batchS=43)
#' @export
switchR <- function(dataIn, numqc, numqcM, batchS){
  batchN=loc=rack=QCsamp=dataI=.=batchN.y=serumID=batchN.x=studyID=NULL
  event=ccID=caseControl=outoforder=maxR=NULL

#assign to size of aliquots
#reassign batches to the samples
temp123 <- dataIn %>%
          arrange(batchN,loc) %>%
          mutate(rowN=row_number()) %>%
          select(-batchN, -loc,-rack,-row,-col) %>%
          mutate(batchN = -99, loc = -99)

#reassign batches to the samples
for (i in(1:nrow(temp123))){
  if (i == 1){
    s <- 1
    p <- 1
  } else if (s >= batchS){
    s <- 1
    p <- p+1
  } else{
    s <- s+1
  }
    temp123$batchN[i] <- p
    temp123$loc[i] <- s
}

#identify if any CC samples are across batches and switch if across batches
dataT <- ccsamp(temp123)

#identify QC batches with problems
dataT0 <- dataT %>%
  filter(QCsamp == 1)

dataOrig = data.frame(serumID=character(),batchN=numeric(),loc=numeric(),round=numeric())
count <- 1
dataU <- 1

iter5 <- 0
repeat{

  iter5 <- iter5 + 1
  if (iter5 > 1000){stop("Function does not converge.")}

dataPb <- probData(dataT0,numqc,numqcM)
#output key:
#dataI = 0, keep where it is;
#dataI = 1, donor;
#dataI = 2, recipient;
#outputs a list of QC samples with every one identified according to the
#code above

#count how many still need to be "fixed"
dataU <- dataPb %>% filter(dataI == 2) %>% nrow(.)
if (dataU == 0){
  break
}

#1. generate what data needs to be matched
dataPF <- toadd_m1(dataPb,numqc,numqcM)

#2. match to appropriate sets
dataMa <- toadd_m2(dataPb,dataPF)

#3. search if can match within the "unmatched" list
dataM <- toadd_m3(dataMa,dataPF,numqcM,dataPb)

dataOrig <- dataM %>%
            rename(batchN=batchN.y) %>%
            mutate(round = count) %>%
            select(round, serumID,batchN,loc) %>%
            rbind(.,dataOrig)

dataM1 <- dataM %>%
          rename(batchN = batchN.x) %>%
          mutate(loc=NA) %>%
          select(serumID, studyID, event, ccID, caseControl, QCsamp,
                 batchN,loc,outoforder)

dataT0 <- dataPb %>%
          filter(!(serumID %in% dataM1$serumID),is.na(serumID)==FALSE) %>%
          select(-dataI) %>%
          rbind(.,dataM1)
count <- count+1

## dataT0 is the output with the "shuffled" QC samples
## dataOrig tracks the original locations where everything is stored
}

## create corrected dataset
dataC <- dataT %>% filter(QCsamp == 0) %>%
  rbind(.,dataT0)

## process the dataOrig locations so only one location per "switch", ie, if a
## QC serum sample is used more than once...
dataOrigC <- dataOrig %>%
            group_by(serumID) %>%
            mutate(maxR = max(round)) %>%
            ungroup() %>%
            filter(round == maxR) %>%
            select(-maxR, -round)

## Find an appropriate match for the QC samples among the serum samples
## in the target batch
iter6 <- 0
repeat{
  iter6 <- iter6 + 1
  if (iter6 > 1000){stop("Function does not converge.")}
dataOuts <- swQC(dataC,dataOrigC)
dataProb <- dataOuts %>% filter(is.na(loc) == TRUE)
if (nrow(dataProb) == 0){
  break
}
dataOrigC <- dataOrigC %>% filter(serumID %in% dataProb$serumID)
dataC <- dataOuts
}

return(dataOuts)

} #end switchR
