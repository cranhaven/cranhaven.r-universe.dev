

######################################################################
##
## File Name: formatRand.R
## Description: format data for randomization
## Date Created: 12/6/2024
## Last Updated: 3/24/2025
##
######################################################################

## FUNCTION: formatRand
## DESCRIPTION: appends the QC data to the serum samples to create a final file before
## entry into the random functions and outputs this new dataset
## QCdata = QC data
## serumIDR = serum data with serumIDs
## serumPack = serum data with packing lists

#' Formats data
#'
#' @description
#' The function `formatRand` formats the dataset for the randomization function.  This function inputs serum data for both the study subjects and QC.
#'
#' @param QCdata QC data.
#' @param serumIDR Serum data with serumIDs.
#' @param serumPack Serum data with packing lists.
#' @return A dataset that is formatted and ready for the randomization file.
#' @examples
#' serumMaster <- formatRand(QCdata=QCMaster,serumIDR=serumIDs,serumPack=serumLoc)
#' @export
formatRand <- function(QCdata,serumIDR,serumPack){

  studyID=serumID=.=rack=event=NULL
  # process the serum data
  serumMerge <- serumIDR %>%
                merge(.,serumPack,by="serumID") %>%
                mutate(ccID = substr(studyID,2,length(studyID)),
                       caseControl=substr(studyID,1,1),QCsamp=0)

  #format the QC samples for merging
  QCMerge <- QCdata %>%
              select(serumID,rack,row,col,event,studyID) %>%
              mutate(ccID = str_split_i(studyID,"Q",1),
                     caseControl=str_split_i(studyID,"Q",2),QCsamp=1)
  # output has studyID, D_Event, rack, row, col, serumID, ccID, caseControl

  serumMerge <- serumMerge %>%
                rbind(.,QCMerge)

  #outputs dataset
  return(serumMerge)

} #end formatRand function
