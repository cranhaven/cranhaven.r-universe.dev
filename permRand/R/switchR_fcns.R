

######################################################################
##
## File Name: switchR_fcns.R
## Description: contains support functions used within the
## switch randomization function, switchR
## Date Created: 4/23/2025
## Last Updated: 6/12/2025
##
######################################################################

## FUNCTION: distFcc
## DESCRIPTION: switch with those within a batch by those that are closest

## NOTE: put this at end of the ccsamp function
distFcc <- function(dataNM){
  QCsamp=ccID=batchN=loc=locO=countA=.=dist=rows=batchNM=locM=NULL
  countN=change=serumID=loc2=NULL

  dataNMF <- dataNM %>%
    filter(QCsamp == 0)

  #check if needed, and which need to be across
  dataOne <- chkAcr(dataNMF)
  if (nrow(dataOne) == 0){
    dataOM <- dataNM
  }else{

 for (l in 1:nrow(dataOne)){
    dataNMF <- dataNM %>%
              filter(QCsamp == 0)

  #create a matrix of those that need to be switched with counts
  dataQ <- dataNMF %>%
            filter(ccID %in% dataOne$ccID[l]) %>%
            group_by(batchN) %>%
            mutate(count=n(),rows = row_number()) %>%
            ungroup() %>%
            mutate(countN=n()-count)

  dataLoc <- dataQ %>% select(batchN,loc) %>%
            group_by(batchN) %>%
            mutate(locI = case_when(min(loc) == 1 ~ 1,
                                    TRUE ~ 0)) %>%
             arrange(batchN,loc) %>%
            mutate(loc0 = case_when((locI == 0)&(row_number()==1) ~ loc)) %>%
              arrange(batchN,desc(loc)) %>%
            mutate(locO = case_when((locI == 1)&(row_number()==1) ~ loc,
                                    TRUE ~ loc0)) %>%
            ungroup() %>%
            filter(is.na(locO) == FALSE) %>% select(batchN, locO)


  #look for places that match those need to be switch in adjacent matrix
  #calculate the distance and sort by shortest distance
  dataL <- dataNMF %>%
          filter((batchN %in% dataQ$batchN)&(!(ccID %in% dataOne$ccID))) %>%
          group_by(batchN,ccID) %>%
          mutate(countA = n()) %>%
          ungroup() %>%
          filter(countA %in% dataQ$countN) %>%
          merge(.,dataLoc,by='batchN') %>%
          mutate(dist=abs(locO-loc)) %>%
          arrange(dist) %>%
          filter(row_number() == 1)
  dataL2 <- dataNMF %>% filter(ccID %in% dataL$ccID) %>%
    mutate(count = n(),rows=row_number())

  #pick the first one and put at end of the matrix, and put all others up by one
  dataL3 <- dataQ %>%
            select(batchN,loc,count,rows) %>%
            rename(locM = loc,batchNM = batchN) %>%
            merge(.,dataL2,by=c('count','rows')) %>%
            filter(batchN != batchNM) %>%
            select(-batchN,-loc,-count,-rows) %>%
            rename(loc=locM,batchN=batchNM)

  dataLF <- dataL2 %>%
              select(batchN,loc,count,rows) %>%
              rename(locM=loc,batchNM = batchN) %>%
            merge(.,dataQ,by=c('count','rows'),all.y=TRUE) %>%
            mutate(change = case_when((is.na(batchNM)==FALSE)&(batchNM != batchN) ~ 1,
                   TRUE ~ 0)) %>%
            mutate(batchN = case_when(change==1 ~ batchNM,
                                      TRUE ~ batchN),
                   loc = case_when(change==1 ~ NA,
                                      TRUE ~ loc)) %>%
            mutate(loc = case_when((change==1)&(min(loc,na.rm=T) == 1) ~ max(loc,na.rm=T) + rows,
                                   (change==1)&(min(loc,na.rm=T) != 1) ~ min(loc,na.rm=T) - rows,
                                   TRUE ~ loc)) %>%
          select(-batchNM,-locM,-count,-rows,-countN,-change) %>%
          rbind(.,dataL3) %>% mutate(outoforder=1)

  #now, move all of the ones in the batch that are left
  dataLF2 <- dataL2 %>%
              select(batchN,loc,count) %>%
              rename(locO = loc) %>%
              filter(row_number()==1) %>%
              merge(.,dataNM,by='batchN') %>%
              filter(!(serumID %in% dataLF$serumID)) %>%
              mutate(loc2 = case_when((min(loc)==1)&(loc>locO) ~ loc-count,
                                   (min(loc)!=1)&(loc<locO) ~ loc+count,
                                  TRUE ~ loc)) %>%
              mutate(outoforder = case_when(loc != loc2 ~ 1,
                                  TRUE ~ 0)) %>%
              mutate(loc = loc2) %>%
              select(-locO,-count,-loc2) %>%
              rbind(.,dataLF)

  dataNM <- dataNM %>%
              filter(!(serumID %in% dataLF2$serumID)) %>%
              rbind(.,dataLF2)
  }
  dataOM <- dataNM %>% arrange(batchN,loc)
  }
  return(dataOM)
}

#####################################################################

## FUNCTION: ccsamp
## DESCRIPTION: identify if case-control pairs are across batches and fix if they are

ccsamp <- function(dataSi){
  batchN=loc=QCsamp=ccID=total=pattern=batchS=Batch_A=Batch_B=.=NULL
  num=ct=group=rand=rn=serumID=rowN=NULL

  dataF <- dataSi %>% mutate(outoforder=0)
  iter4 <- 0
  repeat {
    iter4 <- iter4 + 1
    if (iter4 > 1000){stop("Function does not converge.")}
    #find case-control pairs
    dataS <- dataF %>%
      arrange(batchN,loc) %>%
      filter(QCsamp == 0)

    #find sets that are across batches
    dataP <- chkAcr(dataS)

    if (nrow(dataP) == 0){
      break
    }
    #create matrix with the entire information
    dataS2 <- dataS %>%
      filter(ccID %in% dataP$ccID)
    dataSset <- dataS2 %>%
      group_by(batchN, ccID) %>%
      mutate(pattern = n()) %>%
      ungroup() %>%
      group_by(ccID) %>%
      mutate(total = n()) %>%
      ungroup() %>%
      select(ccID, total, pattern) %>%
      unique() %>%
      arrange(ccID,total,pattern) %>%
      group_by(ccID) %>%
      mutate(batchS = paste('Batch_',chartr("123456789","ABCDEFGHI",row_number()),sep="") )%>%
      ungroup() %>%
      pivot_wider(values_from=pattern, names_from=batchS) %>%
      mutate(Batch_B = total-Batch_A)
    dataSs <- dataSset %>%
      group_by(total,Batch_A,Batch_B) %>%
      summarize(num = n()) %>%
      ungroup() %>%
      mutate(pattern = row_number())
    dataSset <- dataSset %>%
      merge(.,dataSs,by=c("Batch_A","Batch_B","total")) %>%
      select(-num)

    #find sets that can be switched and match the patterns needed in dataS2
    dataSw <- dataS %>%
      filter(!(ccID %in% dataS2$ccID)) %>%
      group_by(ccID) %>%
      mutate(ct = n()) %>%
      ungroup() %>%
      select(ccID,batchN,loc,ct) %>%
      arrange(batchN, loc)

    for (i in 1:nrow(dataSs)){
      dataT <- dataSs %>% filter(pattern == i)
      dataT2 <- dataSw %>%
        filter(ct %in% c(dataT$Batch_A,dataT$Batch_B)) %>%
        mutate(group = -99) %>%
        arrange(batchN,loc)
      gn <- 1
      for (k in 2:nrow(dataT2)){
        if ((dataT2$loc[k-1] == (dataT2$loc[k]-1))&(dataT2$group[k-1] == -99)){
          if ((((dataT2$ct[k-1] == dataT$Batch_A)&(dataT2$ct[k] == dataT$Batch_B))|
               ((dataT2$ct[k-1] == dataT$Batch_B)&(dataT2$ct[k] == dataT$Batch_A)))&
              (dataT2$batchN[k-1]==dataT2$batchN[k])){
            #dataT2$group[k-1] = gn
            #dataT2$group[k] = gn
            ccID1 <- dataT2$ccID[k-1]
            ccID2 <- dataT2$ccID[k]
            dataT2 <- dataT2 %>% mutate(group=case_when(ccID %in% c(ccID1,ccID2) ~ gn,
                                                        TRUE ~group))
            gn = gn + 1
          }
        }
      }

      #keep only those that match
      dataT3 <- dataT2 %>%
        filter(group != -99) %>%
        group_by(group) %>%
        mutate(rand = runif(1)) %>%
        ungroup() %>%
        select(ccID,group,rand, ct) %>%
        distinct(.) %>%
        merge(.,dataS,by=c('ccID')) %>%
        arrange(rand, ct) %>%
        mutate(rn=row_number())
      dataS3 <- dataSset %>%
        select(ccID, pattern) %>%
        merge(.,dataS2,by='ccID') %>%
        filter(pattern == i) %>%
        group_by(batchN,ccID) %>%
        mutate(ct = n()) %>%
        ungroup() %>%
        arrange(batchN,loc) %>%
        mutate(group=-99)
      for (j in 1:nrow(dataS3)){
        if (j == 1){
          gn2 <- 1
        } else if ((j != 1)&(dataS3$ccID[j-1] != dataS3$ccID[j])){
          gn2 <- gn2 + 1
        }
        dataS3$group[j] <- gn2
      }
      dataS3 <- dataS3 %>%
        arrange(group,ct) %>%
        mutate(rn=row_number()) %>%
        select(-group,ct)
      #match based on patterns
      dataTT1 <- dataT3 %>%
        select(rn,batchN,loc)
      dataF1 <- dataS3 %>%
        select(-batchN,-loc) %>%
        merge(.,dataTT1,by=c("rn")) %>%
        select(-pattern)
      dataTT2 <- dataT3 %>%
        select(-batchN,-loc)
      dataF2 <- dataS3 %>%
        select(rn,batchN,loc) %>%
        merge(.,dataTT2,by=c("rn")) %>%
        select(-group,-rand)
      dataF3 <- rbind(dataF1,dataF2) %>%
        select(-rn,-ct) %>% mutate(outoforder = 1)
      if (i == 1){
        dataFF <- dataF3
      }else{
        dataFF <- rbind(dataFF,dataF3)
      }
      dataSw <- dataSw %>%
        filter(!(ccID %in% dataFF$ccID))
    }

    dataF <- dataF %>%
      filter(!(serumID %in% dataFF$serumID)) %>%
      rbind(.,dataFF) %>%
      arrange(batchN,loc)

    #use the distance function
    dataF <- distFcc(dataF)
  }
dataF <- dataF %>% select(-rowN)
  return(dataF)
} #end ccsamp

#####################################################################

## FUNCTION: probData
## DESCRIPTION: find batches that have problems with QC samples

probData <- function(dataI,numqc1,numqcM1){
  batchN=ccID=.=ctQC=gct=nQC=randQ=nBatch=NULL

  dataBN <- data.frame(batchN=1:max(dataI$batchN))

  #identify those with no problems
  dataI2 <- dataI %>%
        group_by(batchN) %>%
        mutate(nBatch = n()) %>%
        ungroup() %>%
        group_by(batchN,ccID) %>%
        mutate(nQC = n(),ctQC = row_number()) %>%
        ungroup() %>%
        merge(.,dataBN,by='batchN',all=TRUE) %>%
        mutate(dataI = case_when((nBatch == numqc1)&(nQC == numqcM1) ~ 0,
                                 TRUE ~ -99))

  dataC <- dataI2 %>% filter(dataI == 0)

  #identify those with "extra", and mark which are extra

  dataP <- dataI2 %>%
            filter(dataI != 0) %>%
            arrange(batchN,ccID,ctQC) %>%
            mutate(gct = -99)
  if (nrow(dataP) == 0){stop("No available samples to be switched.")}
  for(i in 1:nrow(dataP)){
    if (i == 1){
      l = 1
    }else if ((i != 1)&(dataP$batchN[i-1] != dataP$batchN[i])){
      l = 1
    }else if ((i != 1)&(dataP$batchN[i-1] == dataP$batchN[i])&(dataP$ctQC[i]%%numqcM1 == 1)){
      l <- l+1
    }
    dataP$gct[i] <- l
  }

  dataP <- dataP %>%
          group_by(batchN,gct) %>%
           mutate(nQC = n(),ctQC = row_number(),randQ = runif(1)) %>%
          ungroup() %>%
          arrange(batchN,desc(nQC),randQ) %>% mutate(gct = -99)

  for(i in 1:nrow(dataP)){
    if (i == 1){
      l = 1
    }else if ((i != 1)&(dataP$batchN[i-1] != dataP$batchN[i])){
      l = 1
    }else if ((i != 1)&(dataP$batchN[i-1] == dataP$batchN[i])&(dataP$ctQC[i]%%numqcM1 == 1)){
      l <- l+1
    }
    dataP$gct[i] <- l
  }

  grpPerBatch <- numqc1/numqcM1

  dataP2 <- dataP %>%
            mutate(dataI = case_when((gct <= grpPerBatch)&(nQC == numqcM1) ~ 0,
                                    gct > grpPerBatch ~ 1,
                                    (gct <= grpPerBatch)&(nQC != numqcM1) ~ 2)) %>%
            select(-gct,-randQ) %>%
            rbind(.,dataC) %>%
            select(-nBatch,-nQC,-ctQC)

  return(dataP2)

} #end probData

######################################################################

## FUNCTION: toadd_m1
## DESCRIPTION: determine what needs to be added to complete each of the QC sets
## for the batches

toadd_m1 <- function(dataII,numqcC,numqcMM){
  dataI=batchN=ccID=nBatch=nGrp=.=NULL
grpPerBatch <- numqcC/numqcMM

 #recipient
 dataII2 <- dataII %>%
            filter(dataI == 2) %>%
            group_by(batchN) %>%
            mutate(nBatch = n()) %>%
            ungroup() %>%
            group_by(batchN,ccID) %>%
            mutate(nGrp = n()) %>%
            ungroup() %>%
            mutate(nGrp = case_when(is.na(serumID)==TRUE ~ 0,
                                    TRUE ~ nGrp),
                   nBatch = case_when(is.na(serumID)==TRUE ~ grpPerBatch,
                                      TRUE ~ nBatch)) %>%
            select(batchN, nBatch, nGrp,ccID) %>%
            unique(.)

 #add number per group
 dataII2 <- dataII2 %>%
            mutate(nGrp = numqcMM - nGrp)
 for(l in 1:nrow(dataII2)){
   if (grpPerBatch - dataII2$nBatch[l] > 0){
      rowAdd <- data.frame(batchN=dataII2$batchN[l],nBatch=grpPerBatch - dataII2$nBatch[l],
                           nGrp=numqcMM, ccID='')
      dataII2 <- rbind(dataII2,rowAdd)}
 }
dataII2 <- dataII2 %>%
            arrange(batchN)

  return(dataII2)
} #end toadd_m1

######################################################################

  ## FUNCTION: toadd_m2
  ## DESCRIPTION: identify matches to batches

  toadd_m2 <- function(dataIIn,dataIIn2){
    dataI=ccID=rowN=nGrp=.=serumID=randS=randW=numL=rn=rs=NULL
    nGrp.x=nGrp.y=batchN.x=NULL

    #donor
    dataII1 <- dataIIn %>%
      filter(dataI == 1) %>%
      mutate(rowN = row_number()) %>%
      group_by(ccID) %>%
      mutate(nGrp = n(),randS =runif(1)) %>%
      ungroup() %>%
      group_by(rowN) %>%
      mutate(randW = runif(1)) %>%
      ungroup()

    dataUP <- dataIIn2 %>%
              select(nGrp) %>%
              unique(.) %>%
              arrange(desc(nGrp))

  for (m in 1:nrow(dataUP)){
    if (m != 1){
      dataII1 <- dataII1 %>%
                  filter(!(serumID %in% dataMPF$serumID))
    }
      dataII11 <- dataII1 %>%
                  filter(nGrp >= dataUP$nGrp[m]) %>%
                  arrange(randS,randW) %>%
                  group_by(ccID) %>%
                  mutate(numL=row_number()) %>%
                  ungroup() %>%
                  filter(numL <= dataUP$nGrp[m])

      dataMP <- dataIIn2 %>%
              filter(nGrp == dataUP$nGrp[m])
      if (is.na(dataMP$ccID[1]) == TRUE){
          dataII11 <- dataII11 %>% mutate(rn = -99)
          for (s in 1:nrow(dataII11)){
            if (s == 1){
              l <- 1
            } else if ((s != 1)&(dataII11$ccID[s-1] != dataII11$ccID[s])){
              l <- l+1
            }
            dataII11$rn[s] <- l
          }
          dataMP <- dataMP %>%
                    mutate(rn = row_number()) %>%
                    select(-ccID) %>%
                    merge(.,dataII11,by='rn') %>%
                    select(-rn)
      }else{
          dataMP <- dataMP %>%
                      group_by(ccID) %>%
                    mutate(rs=row_number()) %>%
                    ungroup()
          dataII11 <- dataII11 %>%
                    group_by(ccID) %>%
                    mutate(rs=row_number()) %>%
                    ungroup()
          dataMP <- merge(dataMP,dataII11,by=c('ccID','rs')) %>% select(-rs)
      }
      if (m == 1){
        dataMPF <- dataMP
      }else{
        dataMPF <- rbind(dataMPF,dataMP)
      }
    }

    dataMPF <- dataMPF %>%
      mutate(outoforder = 1) %>%
      select(-randS,-randW,-dataI,-nGrp.x,-nGrp.y,-numL,-rowN) %>%
      arrange(batchN.x,ccID)

    return(dataMPF)
} #end toadd_m2

######################################################################

## FUNCTION: toadd_m3
## DESCRIPTION: find matches to those below classified as needing matches

toadd_m3 <- function(data1,data2,matchNN,data3){
  batchN=ccID=nGrp=batchN.x=.=serumID=nBatch=matchT=dataI=NULL

  #include only those unmatched
   data22 <- data2 %>% select(batchN, ccID,nGrp) %>% mutate(count=-99)
  for (i in 1:nrow(data2)){
    if (data2$nGrp[i] > 1){
      for (k in 1:data2$nGrp[i]){
        data22A <- data.frame(batchN=data2$batchN[i],ccID=data2$ccID[i],
                              nGrp=data2$nGrp[i],count=k)
        data22 <- rbind(data22,data22A)
      }
    }else{
      data22$count[i] = 1
    }
  }
  data22 <- data22 %>% arrange(batchN,ccID,count) %>% filter(count != -99)
  data22M <- data22 %>% filter(is.na(ccID) == TRUE) %>% select(-ccID)
  data22U <- data22 %>% filter(is.na(ccID) == FALSE)
  dataUM <- data1 %>%
            group_by(batchN.x,ccID) %>%
            mutate(count=row_number()) %>%
            ungroup()

  dataUM1 <- merge(dataUM,data22M,by.x=c('batchN.x','count'),
                  by.y=c('batchN','count'),all.y=TRUE)
  dataUM2 <- merge(dataUM,data22U,by.x=c('batchN.x','ccID','count'),
                   by.y=c('batchN','ccID','count'),all.y=TRUE)

  dataUM <- dataUM1 %>%
              rbind(.,dataUM2) %>%
            filter((is.na(serumID) == TRUE)|(is.na(ccID)==TRUE)) %>%
            mutate(match = 0,matchc = 0,matchn = matchNN - nGrp)

  if (nrow(dataUM) <= 1){
    dataO3 <- data1 %>% select(-nBatch)
  }else{
  c <- 1
  for (l in 1:(nrow(dataUM)-1)){
    ll <- l+1
    for (j in ll:nrow(dataUM)){
        if ((dataUM$ccID[l] == dataUM$ccID[j])&
        (dataUM$matchn[l]==dataUM$nGrp[j])){
              dataUM$match[l] = c
              dataUM$match[j] = c
              c = c + 1
              dataUM$matchc[l] = 1
              dataUM$matchc[j] = 2
            }
    }
  }

  dataUM <- dataUM %>%
            select(match,batchN.x,count,ccID) %>%
            rename(batchN=batchN.x) %>%
            filter(match != 0) %>%
            group_by(match) %>%
            mutate(matchT = row_number()) %>%
            ungroup()

  dataM2 <- dataUM %>%
            filter(matchT == 2) %>%
            select(-matchT,-count) %>%
            merge(.,data3,by=c('batchN','ccID')) %>%
            rename(batchN.y=batchN) %>%
            mutate(outoforder = 1)
  dataW <- dataUM %>%
            filter(matchT == 1) %>%
            select(-ccID,-matchT) %>%
            rename(batchN.x=batchN) %>%
            merge(.,dataM2,by='match') %>%
            select(-match,-count,-dataI)

  dataO3 <- data1 %>%
            select(-nBatch) %>%
            rbind(.,dataW)
  }

  return(dataO3)

} #end toadd_m3

######################################################################

## FUNCTION: swQC
## DESCRIPTION: search for samples within the target dataset to switch with
## then switch with the samples

swQC <- function(dataC4,dataO5){
  batchN=loc=ccID=nCh=QCsamp=nGrp=serumID=randS=.=rn=nAv=NULL
## find locations for new samples in the new dataset
dataC5 <- dataC4 %>%
            group_by(batchN) %>%
            mutate(nCh = sum(is.na(loc)==TRUE)) %>%
            ungroup() %>%
            group_by(batchN,ccID) %>%
            mutate(nGrp = case_when(QCsamp==0 ~ n(),
                                    TRUE ~ NA)) %>%
            ungroup() %>%
            filter((nCh > 0)&((QCsamp == 1)|(nGrp == 1))) %>%
          group_by(batchN) %>%
          mutate(nAv = sum(nGrp,na.rm=T)) %>%
          ungroup()

##switch QC samples, batch by Batch
dataB <- dataC5 %>% select(batchN) %>% unique()
dataO <- data.frame(serumID=character(),studyID=character(),event=numeric(),
                    ccID=character(),caseControl=character(),QCsamp=numeric(),
                    batchN=numeric(),loc=numeric(),outoforder=numeric())

for (s in 1:nrow(dataB)){
  dataBsl <- dataB$batchN[s]
  dataC6 <- dataC5 %>% filter(batchN == dataBsl)

  dataQCP <- dataC6 %>%
    filter(QCsamp == 1, is.na(loc) == TRUE)

  dataS <- dataC6 %>%
    filter(is.na(nGrp) == FALSE)

  dataOri <- dataO5 %>%
              filter(serumID %in% c(dataC6$serumID)) %>%
              mutate(rn = row_number()) %>%
              select(-serumID)

    dataSSB <- dataS %>%
              mutate(randS = runif(1)) %>%
              arrange(randS) %>%
              mutate(rn = row_number())
    dataSB <- dataSSB %>%
             select(-batchN,-loc) %>%
                merge(.,dataOri,by='rn') %>%
              select(-rn,-nCh,-nGrp,-nAv,-randS)
    dataSSB2 <- dataSSB %>%
                select(rn,loc,batchN)
    dataQC <- dataQCP %>%
              select(-loc,-batchN) %>%
              mutate(rn = row_number()) %>%
              merge(.,dataSSB2,by='rn') %>%
              select(-rn,-nCh,-nGrp,-nAv)
    dataO <- rbind(dataSB,dataQC) %>%
                rbind(.,dataO)

}
dataO <- dataO %>% mutate(outoforder = 1)
dataCM <- dataC4 %>%
          filter(!(serumID %in% dataO$serumID)) %>%
          rbind(.,dataO) %>%
          arrange(batchN,loc)

return(dataCM)
} #end swQC

######################################################################

