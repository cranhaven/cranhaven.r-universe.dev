######################################################################
##
## File Name: allRand_fcns.R
## Description: contains support functions used within the
## randomization function, allRand
## Date Created: 3/24/2025
## Last Updated: 4/15/2025
##
######################################################################

## FUNCTION: assignBatch
## DESCRIPTION: assigns batches and locations to samples

assignBatch <- function(temp1,batchTotA,numQC1){
  temp1 <- temp1 %>%
          mutate(batchN=0,loc=0)

  for (i in 1:nrow(temp1)){
    if (i == 1){
      s <- 1
      b <- 1
    }else if ((i != 1)&(s < (batchTotA-numQC1))){
      s <- s+1
    }else{
      s <- 1
      b <- b+1
    }
    temp1$batchN[i] <- b
    temp1$loc[i] <- s
  }
  return(temp1)
}

######################################################################
## FUNCTION: chkAcr
## DESCRIPTION: checks if case-control pairs are across batches

chkAcr <- function(temp2){
  batchN=loc=prob=NULL

  temp2 <- temp2 %>% arrange(batchN, loc) %>% mutate(prob=0)

  for (i in 1:(nrow(temp2))){
    if ((i != 1)&&(temp2$batchN[i-1] != temp2$batchN[i])&&(temp2$ccID[i-1] == temp2$ccID[i])){
      temp2$prob[i] <- 1
    }
  }
  temp2a2 <- temp2 %>% filter(prob == 1)

  return(temp2a2)
}

######################################################################
## FUNCTION: replaceChr
## DESCRIPTION: switch pair with unpaired aliquots

replaceChr <- function(temp2z,temp2a1){
  ccID=.=batchN=ctsCCBT=ctsCCBa=ctsCCB=randS=serumID=randW=NULL
  prob=loc=final1=final0=NULL

  #create a matrix of those needing to be switched and possible matches for the
  #switch
  temp2a <- temp2a1 %>%
              filter(row_number() == 1)
  temp2aa <- temp2a %>%
              select(ccID) %>%
              merge(.,temp2z,by="ccID") %>%
              group_by(batchN) %>%
             mutate(ctsCCBa = n()) %>%
              ungroup() %>% mutate(ctsCCBT = n()) %>%
              mutate(ctsCCB = ctsCCBT - ctsCCBa) %>%
              ungroup() %>% select(-ctsCCBT, -ctsCCBa)
  temp2c <- temp2z %>% group_by(ccID) %>%
              mutate(ctsCCB = n()) %>% ungroup()
  temp2ab <- temp2aa %>%
              select(batchN,ctsCCB) %>% unique() %>%
              merge(.,temp2c,by=c("ctsCCB","batchN"))

  #if no possible matches, then put between this spot and end
  if (nrow(temp2ab) == 0) {

    temp55 <- temp2a %>%
        select(ccID) %>%
        merge(.,temp2z,by="ccID") %>%
        mutate(randS = runif(1)*(1-randS)+randS)
      temp23 <- temp2z %>%
                filter(!(serumID %in%temp55$serumID)) %>%
                rbind(.,temp55) %>%
                arrange(randS,randW)

  }else{

    temp2ab <- temp2ab %>%
                group_by(ccID) %>%
                mutate(switch=runif(1)) %>%
                ungroup() %>%
                arrange(switch) %>%
                filter(switch == min(switch)) %>%
                select(-switch) %>% mutate(prob = 0)
    temp2aa <-  temp2aa %>% mutate(prob = 1) %>%
                rbind(.,temp2ab) %>%
                select(-ctsCCB) %>%
                arrange(prob) %>%
                mutate(final1 = randS[1]) %>%
                arrange(desc(prob))  %>%
                mutate(final0 = randS[1]) %>%
                mutate(randS = case_when(prob == 0 ~ final0,
                                         TRUE ~ final1)) %>%
                select(-loc,-final1,-final0,-batchN,-prob)

    temp23 <- temp2z %>%
              filter(!serumID %in%(temp2aa$serumID)) %>%
              select(-batchN, -loc) %>%
              rbind(.,temp2aa) %>%
              arrange(randS, randW)
  }


  return(temp23)
}


######################################################################
## FUNCTION: repCC
## DESCRIPTION: assigns batches and ensures sets are not across batches

repCC <- function(ztemp,zbatchTot,znumQC){

  temp25 <- assignBatch(ztemp,zbatchTot,znumQC)
  temp2a2 <- chkAcr(temp25)

  iter2 <- 0
  repeat{
    iter2 <- iter2 + 1
    if (iter2 > 1000){stop("Function does not converge.")}

    if (nrow(temp2a2) == 0) {
      break
    }
    temp23 <- replaceChr(temp25,temp2a2)
    temp25 <- assignBatch(temp23,zbatchTot,znumQC)
    temp2a2 <- chkAcr(temp25)
  }
  return(temp25)

} #end repCC


######################################################################

## FUNCTION: batchOrd()
## DESCRIPTION: assigns batches to the QC samples
## numMatcher = number of QC samples that should be within the sets

batchOrd <- function(dataQCs, numMatcher, numQCs, numBatchs,chkRep){

  serumID=ccID=randF=grp=rand2=studyID=batchN=grpM=cts=idL=rand3=.=rowN3=rowN2=NULL
  #assign to groups for assignment to batches
  #assign these groups to batches
  dataQC1 <- dataQCs %>%
              group_by(serumID) %>%
              mutate(randF = runif(1)) %>%
              ungroup() %>%
            arrange(ccID,randF) %>%
            mutate(grp = ceiling(row_number()/numMatcher)) %>%
            select(-randF) %>%
            group_by(grp) %>%
            mutate(rand2 = runif(1)) %>%
            ungroup() %>%
            arrange(rand2) %>%
            mutate(batchN = ceiling(row_number()/numQCs)) %>%
            mutate(batchN = case_when(batchN > numBatchs ~ numBatchs,
                                      TRUE ~ batchN))

  #ensure groups within batches are not repeated
  if (chkRep == 1){
  dataQC2 <- dataQC1 %>%
            group_by(studyID) %>%
            mutate(grpM = strsplit(studyID,'Q')[[1]][1]) %>%
            ungroup() %>%
            group_by(batchN,grpM) %>%
            mutate(cts = n()) %>%
            ungroup() %>%
            filter((cts > numMatcher)&(batchN != numBatchs)) %>%
            group_by(batchN) %>%
            mutate(idL = ceiling(row_number()/numMatcher)) %>%
            filter(idL != 1) %>%
            ungroup() %>%
            mutate(rowN2 = row_number())
  if (nrow(dataQC2) == 0){
  dataQC3 <- dataQC1 %>%
            group_by(studyID) %>%
              mutate(grpM = strsplit(studyID,'Q')[[1]][1]) %>%
              ungroup() %>%
              filter((!(grpM %in% dataQC2$grpM))&(!(batchN %in% dataQC2$batchN))) %>%
              group_by(grp) %>%
              mutate(rand3 = runif(1)) %>%
              arrange(rand3) %>%
              ungroup() %>%
              mutate(rowN3 = row_number())

  dataQC4 <- dataQC2 %>%
                select(-batchN) %>%
              merge(.,dataQC3 %>% select(rowN3,batchN), by.x="rowN2",by.y="rowN3",all.x=TRUE) %>%
              select(-rowN2,-cts,-idL,-grpM)
  dataQC4b <- dataQC3 %>%
                select(-batchN) %>%
              merge(.,dataQC2 %>% select(rowN2,batchN),by.x="rowN3",by.y="rowN2",all.y=TRUE) %>%
              select(-rowN3,-rand3,-grpM)
  dataQC4 <- rbind(dataQC4,dataQC4b)
  tempA <- dataQC1 %>% filter(!(grp %in% dataQC4$grp)) %>%
              rbind(.,dataQC4)
  }else{
    tempA <- dataQC1
  }
  }else{
    tempA <- dataQC1
  }

  return(tempA)

} #end batchOrd

######################################################################
## FUNCTION: chkWith
## DESCRIPTION: makes sure that the QC variables are not within a certain
## distance
## withN = distance to check

chkWith <- function(dataI,withN){
  batchN=QCsamp=loc=ccID=studyID=randS1=.=randS=randW=NULL

  batchM <- max(dataI$batchN)
  iter3 <- 0
  repeat{
    iter3 <- iter3 + 1
    if (iter3 > 1000){stop("Function does not converge.")}
  dataO <- dataI %>%
            group_by(batchN) %>%
            mutate(loc=row_number()) %>%
            ungroup()

  dataOS <- dataO %>%
            filter(QCsamp == 1) %>%
            group_by(batchN) %>%
            mutate(diff = abs(loc - lag(loc))) %>%
            filter((diff < withN)&(batchN != batchM))
  if (nrow(dataOS) == 0){
    break
  }
  dataOSb <- unique(dataOS$batchN)

  #re-randomize those with the "problem" batches
  dataI2 <- dataI %>%
              filter(batchN %in% dataOSb) %>%
              group_by(ccID) %>%
              mutate(randS = runif(1)) %>%
              ungroup() %>%
              group_by(studyID) %>%
              mutate(randS1 = runif(1)) %>%
              ungroup() %>%
              mutate(randS = case_when(QCsamp == 1 ~ randS1,
                                       TRUE ~ randS)) %>%
              select(-randS1)
  dataI <- dataI %>%
              filter(!(batchN %in% dataOSb)) %>%
              rbind(.,dataI2) %>%
              arrange(batchN,randS,randW)


  }

  return(dataO)
} #end chkWith
