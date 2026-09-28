

######################################################################
##
## File Name: outputLab.R
## Description: create output labels for the randomized groups
## Date Created: 12/6/2024
## Last Updated: 5/23/2025
##
######################################################################

#' Output labels.
#'
#' @description
#' Creates output labels for the randomized groups.
#'
#' @param dataOut Dataset to be formatted for packing list.
#' @param blind Indicator 0/1 select if a blinded (1) or unblinded(0) packing list is to be generated.
#' @param origP Indicator, 0/1, inclusion of the original packing location (1) or deletion of the packing location (0).
#' @param maxRows Maximum row for the output dataset.
#' @param maxCols Maximum column for the output dataset.
#' @param newPack 0/1 indicator to generate new packing locations.
#' @return A dataset to be used for packing lists.
#' @examples
#' blind <- outputLab(dataOut=serumRand,blind=1,origP=0,maxRows=9,maxCols=9,newPack=1)
#' unBlindSw <- outputLab(serumSwitchP,blind=0,origP=.,maxRows=.,maxCols=.,newPack=0)
#' @export
outputLab <- function(dataOut, blind, origP, maxRows, maxCols,newPack){
  rack=batchN=loc=studyID=ccID=caseControl=QCsamp=NULL

  if (newPack == 1){
  if (origP == 1){
        dataOut <- dataOut %>%
            rename(orig_rack=rack, orig_row=row, orig_col=col)
  }else{
    dataOut <- dataOut %>%
              select(-rack,-row,-col)
  }

  dataOut <- dataOut %>% arrange(batchN,loc)
  #assign rack, row, col locations to the randomization dataset
  outPack <- packLoc(dataOut, maxRows, maxCols)
  }else{
    outPack <- dataOut %>% arrange(batchN,loc)
  }

  #blinded dataset
  if (blind == 1){
    outPack <- outPack %>%
                  select(-studyID,-ccID,-caseControl,-QCsamp)
  }

  return(outPack)
} #end outputLab
