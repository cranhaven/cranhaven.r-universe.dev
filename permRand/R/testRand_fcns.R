
######################################################################
##
## File Name: testRand_fcns.R
## Description: functions for the randomization test data
## Date Created: 12/6/2024
## Last Updated: 12/6/2024
##
######################################################################

# FUNCTION: testDataUB
# DESCRIPTION: generates a test dataset with serumIDs, and subjectIDs
testDataUB <- function(studySize,expNS,numCC){
  studyID=time1=time2=time3=time4=co=exist=event=NULL
  Freq=serumID=.=serumID2n=serumID2o=NULL

  nlen <- studySize*(numCC+1)
  testData <- data.frame(studyID=character(nlen))

  #create the unblinded serumIDs
  testData <- testData %>% mutate(
    studyID=(paste(letters[ceiling(row_number()/studySize)+23],
                   sprintf("%03d",row_number()-floor(row_number()/studySize)*studySize),sep=""))) %>%
    mutate(studyID=toupper(studyID),
           time1 = runif(n=nlen,min=0,max=1),
           time2 = runif(n=nlen,min=0,max=1),
           time3 = runif(n=nlen,min=0,max=1),
           time4 = runif(n=nlen,min=0,max=1),
           co = expNS/((4*(numCC+1)*studySize))) %>%
    mutate(D_event1 = case_when(time1 <= co ~ 1,
                                TRUE ~ 99),
           D_event2 = case_when(time2 <= co ~ 2,
                                TRUE ~ 99),
           D_event3 = case_when(time3 <= co ~ 3,
                                TRUE ~ 99),
           D_event4 = case_when(time4 <= co ~ 4,
                                TRUE ~ 99)) %>%
    select(-time1,-time2,-time3,-time4,-co) %>%
    group_by(studyID) %>%
    pivot_longer(cols=c("D_event1","D_event2","D_event3","D_event4"),names_to="exist",values_to="event") %>%
    select(-exist) %>% ungroup() %>%
    mutate(serumID = paste("S",sprintf("%09d",
                                       floor(runif(nlen*4,min=1,max=999999999))),sep="")) %>%
    filter(event != 99)

  d <- 1
  iter7 <- 0
  while(d != 0){
    #test to ensure no repeats of serumIDs
    iter7 <- iter7 + 1
    if (iter7 > 10000){stop("Function does not converge.")}

    testData2 <- data.frame(xtabs(~serumID,data=testData)) %>%
      filter(Freq != 1)

    if (nrow(testData2) != 0) {
      testData <- merge(testData,testData2,by=c("serumID")) %>%
        rename(serumID2o=serumID) %>%
        mutate(serumID2n = paste("S",sprintf("%09d",
                                             floor(runif(nrow(.),min=1,max=999999999))),sep="")) %>%
        mutate(serumID = case_when(Freq > 1 ~ serumID2n,
                                   TRUE ~ serumID2o)) %>%
        select(-serumID2n,-serumID2o,-Freq)
    } else {
      break
    }
  } #end while loop
  return(testData)
} #end function

######################################################################

# FUNCTION: testDataLoc
# DESCRIPTION: generates a packing location
testDataLoc <- function(dataIn,rowSize,colSize){
  event=studyID=NULL

  dataIn2 <- packLoc(dataIn,rowSize,colSize)
 dataIn2 <- dataIn2 %>% select(-event,-studyID)
  return(dataIn2)

}

######################################################################

# FUNCTION: testDataQC
# DESCRIPTION: generates a list of empty QC samples
testDataQC <- function(numQC,rowSize,colSize){
  Freq=.=serumID=serumID2n=serumID2o=NULL

  #generate serumIDs
  td_QC <- data.frame(serumID=character(numQC)) %>%
  mutate(serumID= paste("S",sprintf("%09d",
          floor(runif(nrow(.),min=1,max=999999999))),sep=""))
   #test no repeat serumIDs
  iter8 <- 0
  d <- 1
  while(d != 0){
    iter8 <- iter8 + 1
    if (iter8 > 10000){stop("Function does not converge.")}
    #test to ensure no repeats of serumIDs

    testData2 <- data.frame(xtabs(~serumID,data=td_QC)) %>%
      filter(Freq != 1)

    if (nrow(testData2) != 0) {
      td_QC <- merge(td_QC,testData2,by=c("serumID")) %>%
        rename(serumID2o=serumID) %>%
        mutate(serumID2n = paste("S",sprintf("%09d",
                                             floor(runif(nrow(.),min=1,max=999999999))),sep="")) %>%
        mutate(serumID = case_when(Freq > 1 ~ serumID2n,
                                   TRUE ~ serumID2o)) %>%
        select(-serumID2n,-serumID2o,-Freq)
    } else {
      break
    }
  } #end while loop

  #generate package locations
  td_QC <- packLoc(td_QC,rowSize,colSize)

  return(td_QC)
}

