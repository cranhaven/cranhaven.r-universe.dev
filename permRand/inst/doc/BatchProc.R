## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries, message=FALSE-------------------------------------------------

library(dplyr)
library(magrittr)
library(permRand)

## ----testData-----------------------------------------------------------------
data("testR",package="permRand")

serumIDs <- testR[[1]]
serumLoc <- testR[[2]]
emptyQC <- testR[[3]]
motherQC <- testR[[4]]

## ----qualData-----------------------------------------------------------------
tests1 <- uniqueID(serumIDs,"serumID")
tests2 <- uniqueID(serumLoc,"serumID")
tests3 <- uniqueID(emptyQC,"serumID")
tests4 <- uniqueID(motherQC,"serumID")

## ----format-------------------------------------------------------------------
QCMaster <- randTest(dataMom=motherQC,dataChild=emptyQC,maxAliq=4, nEvent=c(28,27,28,30))

serumMaster <- formatRand(QCdata=QCMaster,serumIDR=serumIDs,serumPack=serumLoc)

## ----rand---------------------------------------------------------------------

serumMaster3 <- serumMaster %>% filter(event == 3)
serumRand <- allRand(dataR=serumMaster3,batchTot=c(40,44), numQC=2,
                     withinN=2,numMatch=2,chkRep=1)

## ----test1ar------------------------------------------------------------------
test <- testCCAcross(dataS=serumRand)

## ----test2ar------------------------------------------------------------------
test <- testQCmatch(dataS=serumRand,numQCs=4,numMatch=2)

## ----test3ar------------------------------------------------------------------
test <- uniqueID(serumRand,"serumID")

## ----test4ar------------------------------------------------------------------
test <- testPair(dataS=serumRand)

## ----test5ar------------------------------------------------------------------
test <- orderCases(dataI=serumRand,betW=4)

## ----test6ar------------------------------------------------------------------
test <- batchCount(dataS=serumRand,batchSizeT=84)

## ----test7ar------------------------------------------------------------------
test <- countQC(dataS=serumRand,QCN=4)

## ----packingar----------------------------------------------------------------
unBlind <- outputLab(dataOut=serumRand,blind=0,origP=1,maxRows=9,maxCols=9,newPack=1)
blind <- outputLab(dataOut=serumRand,blind=1,origP=0,maxRows=9,maxCols=9,newPack=1)

## ----switchR------------------------------------------------------------------
serumSwitch <- switchR(dataIn=serumRand,numqc=2,numqcM=2,batchS=44)

## ----checksr------------------------------------------------------------------
test1 <- testCCAcross(serumSwitch)
test2 <- testQCmatch(serumSwitch,2,2)
test3 <- uniqueID(serumSwitch,"serumID")
test4 <- testPair(serumSwitch)
test5 <- orderCases(serumSwitch,4)
test6 <- batchCount(serumSwitch,44)
test7 <- countQC(serumSwitch,2)

## ----packingarS---------------------------------------------------------------
serumSwitchP <- unBlind %>% 
                select(serumID,rack,row,col) %>%
                merge(.,serumSwitch,by='serumID')
unBlindSw <- outputLab(serumSwitchP,blind=0,origP=.,maxRows=.,maxCols=.,newPack=0)
blindSw <- outputLab(serumSwitchP,blind=1,origP=.,maxRows=.,maxCols=.,newPack=0)

