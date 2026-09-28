

######################################################################
##
## File Name: allRandS.R
## Description: complete randomization
## Date Created: 6/24/2025
## Last Updated: 6/24/2025
##
######################################################################

# FUNCTION: allRandS
# DESCRIPTION: Complete randomization of aliquots.
# dataR = data for randomization
# batchTot = size of plate, just use one plate per batch, batch size inclusive of QC samples
# numQC = number of QC samples per batch
# withinN = number of samples away that the QC samples must be from each other
# numMatch = number of QC samples per plate if two or more plates per batch or
# per batch if one plate per batch
# chkRep = check if there is a repeat of the groups within the batches

allRandS <- function(dataR, batchTot, numQC, withinN, numMatch,chkRep){
  QCsamp=ccID=studyID=randS=randW=grp=rand2=loc=.=batchN=NULL
  ###########################
  # start processing the study samples
  # split the data so we only are only processing study subject samples
  # assign a random number to case control sets, then assign a random number to
  # each of the elements within these sets

  dataS <- dataR %>% filter(QCsamp == 0) %>%
            group_by(ccID) %>%
            mutate(randS = runif(1)) %>%
            ungroup() %>%
            group_by(studyID) %>%
            mutate(randW = runif(1)) %>%
            ungroup() %>%
            arrange(randS,randW)

  # assign batches and ensure that case-control pairs are not across batches
  temp25 <- repCC(dataS, batchTot, numQC)

  ###########################
  # start processing QC samples
  dataQC <- dataR %>% filter(QCsamp == 1)
  numBatch <- max(temp25$batchN)

  # assign batches to the QC samples
  temp2a <- batchOrd(dataQC, numMatch, numQC, numBatch,chkRep)

  #########################
  # merge/append the study and QC samples to create a single file
  # randomize order of sets within batches

  temp2a <- temp2a %>%
            select(-grp,-rand2) %>%
            group_by(studyID) %>%
            mutate(randS = runif(1),randW = 1) %>%
              ungroup()
  tempFin <- temp25 %>%
              select(-randS,-loc) %>%
              group_by(ccID) %>%
              mutate(randS = runif(1)) %>%
              ungroup() %>%
            rbind(.,temp2a) %>%
              group_by(batchN) %>%
              arrange(batchN,randS,randW) %>%
              mutate(loc = row_number()) %>%
              ungroup()

  # ensure that QC samples are not within a certain distance of each other
  tempFin <- chkWith(tempFin,withinN)

  tempFin <- tempFin %>% select(-randW, -randS)
  #return the randomized file
  return(tempFin)

} #end allRandS function
