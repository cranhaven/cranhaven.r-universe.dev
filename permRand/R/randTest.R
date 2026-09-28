

######################################################################
##
## File Name: randTest.R
## Description: contains the function, randTest
## Date Created: 12/6/2024
## Last Updated: 3/21/2025
##
######################################################################

## FUNCTION: randTest
## DESCRIPTION: assigns random test for the QC/aliquots and ID numbers
## dataMom: is the data from the QC samples
## dataChild: is the empty vials
## maxAliq: number of "children" per "adult"
## nEvent: number in each event

#' QC Identifiers
#'
#' @description
#' We first assign IDs linking mother/child and events using the function `randTest`.
#'
#' @param  dataMom The mother dataset.
#' @param dataChild Child dataset.
#' @param maxAliq Number of aliquots per mother aliquot.
#' @param nEvent Number of aliquots per each of event or lab.
#' @return The output of the function is a dataset with the ID links.
#' @examples
#' randTest(dataMom=motherQC,dataChild=emptyQC,maxAliq=4, nEvent=c(28,27,28,30))
#' @export
randTest <- function(dataMom,dataChild,maxAliq, nEvent){
  rand=.=event=rowN=rack=serumID=RN=IDm=ZZrn=NULL

  #create lists of event to assign to each mother
  nTot <- sum(nEvent)
  dataE <- data.frame(event = numeric(nTot))
  for (i in 1:length(nEvent)){
    if (i == 1) {c <- 1}
    dataE$event[c:(c+nEvent[i]-1)] <- i
    c <- c+nEvent[i]
  }
  dataMom2 <- dataMom %>% mutate(rowN = row_number())
  dataA <- dataE %>% mutate(ZZrn=row_number()) %>%
            group_by(ZZrn) %>%
            mutate(rand=runif(1)) %>%
            ungroup() %>% select(-ZZrn) %>%
            arrange(rand) %>% mutate(rowN=row_number()) %>%
            merge(.,dataMom2,by="rowN") %>%
             arrange(event) %>% mutate(IDm = 0) %>%
            select(-rand,-rowN)
  for (i in 1:nrow(dataA)){
    if ((i == 1)||((i != 1)&(dataA$event[i-1] != dataA$event[i]))) {
      l <- 1
    } else {
      l <- l+1
    }
    dataA$IDm[i] <- l
  }

  #repeat the mother serum samples for merging
  data2 <- dataA[rep(row.names(dataA),times=maxAliq),] %>%
            arrange(rack, row, col) %>%
            rename(motherSerumID=serumID,motherRack=rack, motherRow = row, motherCol = col) %>%
            mutate(rowN = row_number())

  #This file assigns ID linking the mother to "children" IDs.
  dataQC <- dataChild %>% mutate(RN = row_number()) %>%
            group_by(RN) %>%
            mutate(rand=runif(1)) %>%
            ungroup() %>% select(-RN) %>%
            arrange(rand) %>% mutate(rowN = row_number()) %>%
            merge(.,data2,by='rowN') %>%
            select(-rand,-rowN) %>%
            arrange(event) %>%
            mutate(studyID='')

  for (m in 1:nrow(dataQC)){
      if ((m == 1)||((i != 1)&(dataQC$IDm[m-1] != dataQC$IDm[m]))) {
        s <- 1
      }else{
        s <- s+1
      }
      if (s < 10){
          dataQC$studyID[m] <- paste(dataQC$IDm[m],"Q","0",s,sep="")
        }else{
          dataQC$studyID[m] <- paste(dataQC$IDm[m],"Q",s,sep="")
        }
      }
  dataQC <- dataQC %>% select(-IDm)

  return(dataQC)
} #end randTest
