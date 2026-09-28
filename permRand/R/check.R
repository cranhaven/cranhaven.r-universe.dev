

######################################################################
##
## File Name: check.R
## Description: checks the randomization of the aliquots
## Date Created: 12/6/2024
## Last Updated: 4/22/2025
##
######################################################################
## FUNCTION: testCCAcross
## DESCRIPTION: no sample groups across batches or plates.  The output
## dataset, should have 0 rows.

#' Sample groups within batches
#'
#' @description
#' Tests and finds sample groups that are across batches.
#'
#' @param dataS The test dataset.
#' @return The output lists all batches with not enough QC sample sets or the QC samples do not come from the same mother.
#' @examples
#' test <- testCCAcross(dataS=serumRand)
#' @export
testCCAcross <- function(dataS){
  QCsamp=batchN=loc=minloc=minbatch=maxloc=maxbatch=prob=ccID=NULL

  #identify the first and next one after
  dataQc <- dataS %>%
            filter(QCsamp == 0) %>%
            arrange(batchN,loc) %>%
          mutate(minbatch = min(batchN),maxbatch = max(batchN)) %>%
            group_by(batchN) %>%
            mutate(minloc = min(loc),maxloc = max(loc)) %>%
            ungroup() %>%
            filter(((loc == minloc)&(batchN != minbatch))|
                     ((loc == maxloc)&(batchN != maxbatch))) %>%
              mutate(prob = case_when((lag(ccID) == ccID) ~ 1,
                                      TRUE ~ 0)) %>%
              filter(prob == 1)
  dataQc2 <- dataS %>%
            arrange(batchN,loc) %>%
            filter(ccID %in% dataQc$ccID)

    return(dataQc2)
}

######################################################################
## FUNCTION: testQCmatch
## DESCRIPTION: measures that every batch has at least the specified number
## of matching sets in a batch.  The output lists all samples that are not
## matched or batches with not enough QC sample sets.

#' Tests QC matches
#'
#' @description
#' Measures that every batch has at least the specified number of matching QC sample sets in a batch.
#'
#' @param dataS Randomized data.
#' @param numQCs Number of QCs specified per dataset.
#' @param numMatch Number of QC samples form a single mother within a batch.
#' @return The output lists all batches with not enough QC sample sets or the QC samples do not come from the same mother.
#' @examples
#' test <- testQCmatch(dataS=serumRand,numQCs=4,numMatch=2)
#' @export
testQCmatch <- function(dataS,numQCs,numMatch){
  QCsamp=batchN=ccID=countMatch=countBatch=.=NULL

  maxB <- max(dataS$batchN)

  testOqc <- dataS %>%
              filter(QCsamp == 1) %>%
              arrange(batchN,ccID) %>%
              group_by(batchN,ccID) %>%
              mutate(countMatch = n()) %>%
              ungroup() %>%
              group_by(batchN) %>%
              mutate(countBatch=n()) %>%
              ungroup() %>%
              select(batchN, ccID, countMatch, countBatch) %>%
              distinct(.) %>%
              filter((batchN != maxB)&((countBatch < numQCs)|(countMatch < numMatch)))

  return(testOqc)
}

######################################################################
## FUNCTION: testPair
## DESCRIPTION: Test if sets are not next to each other.
## Ensure that temp has no values as it reports any sets that are not
## next to each other.

#' Ensures complete sets.
#'
#' @description
#' Tests if sets are next to each other.  Any sets that are not next to each other are flagged.
#'
#' @param dataS Test dataset.
#' @return The output reports any sets that are separated in the "loc".
#' @examples
#' test <- testPair(dataS=serumRand)
#' @export
testPair <- function(dataS){
  QCsamp=ccID=batchN=loc=probs=NULL

  testP <- dataS %>%
            filter(QCsamp == 0) %>%
            arrange(ccID,batchN,loc) %>%
            group_by(ccID) %>%
            mutate(lagloc = lag(loc)) %>%
            mutate(probs = case_when((lagloc != (loc-1)) ~ 1,
                                     TRUE ~ 0)) %>%
            filter(probs == 1)

  testProb <- dataS %>%
              filter(ccID %in% testP$ccID) %>%
              arrange(batchN,loc)

 return(testProb)

}

######################################################################
## FUNCTION: orderCases
## DESCRIPTION: Tests if a large number of cases or controls are next to each other.
## The output stores if there are any cases or controls together beyond a certain
## specified value, and if so, test lists the study IDs of the cases or controls
## in the order they are listed.

#' Number of single group in sequence.
#'
#' @description
#' Tests if a large number of cases or controls are next to each other.
#'
#' @param dataI Dataset to be tested.
#' @param betW Number of cases or controls to check if they are next to each other.
#' @return The output stores if there are any cases or controls together beyond a certain specified value.
#' @examples
#' test <- orderCases(dataI=serumRand,betW=4)
#' @export
orderCases <- function(dataI,betW){
  loc=NULL

  batchN=caseControl=groupO=groupC=groupCm=lagC=NULL

  dataO <- dataI %>%
            arrange(batchN, loc) %>%
            mutate(groupO = -99, groupC = -99,lagC = lag(caseControl))

  for (i in(1:nrow(dataO))){
    if (i == 1){
      s <- 1
      p <- 1
    }else if ((dataO$lagC[i] == dataO$caseControl[i])&(i != 1)){
      p <- p+1
    }else{
      s <- s+1
      p <- 1
    }
    dataO$groupO[i] = s
    dataO$groupC[i] = p
  }
  dataO2 <- dataO %>%
          select(groupO,groupC) %>%
          group_by(groupO) %>%
          summarize(groupCm = max(groupC)) %>%
          ungroup() %>%
          filter(groupCm > betW)

  dataO3 <- dataO %>%
            filter(groupO %in% dataO2$groupO) %>%
            select(-groupO, -groupC, -lagC)

  return(dataO3)

}

######################################################################
## FUNCTION: batchCount
## DESCRIPTION: Counts how many are in each of the batches.
## The output should be empty, as it contains the ID of any batch that does not contain
## the specified number of individuals, except the last batch.

#' Tests the number in each batch.
#'
#' @description
#' Counts the number of samples that are in each of the batches.
#'
#' @param dataS Test dataset.
#' @param batchSizeT Batch size to test for.
#' @return The ID of any batch that does not contain the specified number of samples.
#' @examples
#' test <- batchCount(dataS=serumRand,batchSizeT=84)
#' @export
batchCount <- function(dataS,batchSizeT){
  batchN=loc=batchNmax=NULL
  dataBC <- dataS %>%
            arrange(batchN, loc) %>%
            group_by(batchN) %>%
            count() %>%
            ungroup() %>%
            mutate(batchNmax = max(batchN)) %>%
            filter((n != batchSizeT)&(batchN != batchNmax))

  dataBC2 <- dataS %>%
            arrange(batchN, loc) %>%
            filter(batchN %in% dataBC$batchN)

  return(dataBC2)
}

######################################################################
## FUNCTION: countQC
## DESCRIPTION: Counts how many QC samples are in each of the batches, and if it
## doesn't match the number specified.
## The output should have no samples in it.  It counts how many batches do not
## have the number of QC samples that doesn't match the number specified.

#' Number of QC in each batch.
#'
#' @description
#' Count how many QC samples are in each of the batches, and if it doesn't match the number specified.
#'
#' @param dataS Test dataset.
#' @param QCN Number of QC samples per batch.
#' @return The output includes any batches that does not contain the number of QC samples specified.
#' @examples
#' test <- countQC(dataS=serumRand,QCN=4)
#' @export
countQC <- function(dataS,QCN){

  QCsamp=batchN=.=loc=NULL

  dataCtQC <- dataS %>%
              filter(QCsamp == 1) %>%
              group_by(batchN) %>%
              count(.) %>%
              ungroup() %>%
              mutate(maxGrp = max(batchN)) %>%
              filter(n < QCN)
  dataCtQC2 <-  dataS %>%
              arrange(batchN,loc) %>%
              filter((QCsamp == 1)&(batchN %in% dataCtQC$batchN))

  return(dataCtQC2)

}

