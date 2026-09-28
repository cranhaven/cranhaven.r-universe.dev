

######################################################################
##
## File Name: testRand.R
## Description: contains the testRand function
## Date Created: 12/6/2024
## Last Updated: 12/6/2024
##
######################################################################

## FUNCTION: testRand
## DESCRIPTION: generates test data for the randomization functions
## rowSize = max row size
## colSize = max column size
## studySize = number of cases
## expNS = number of aliquots per case/control
## numCC = number of controls per case
## QCpct = percent of QCs for number of samples
## child = number of children per "mother" aliquot

#' Test Dataset
#'
#' @description
#' Generates test data for the randomization functions.
#'
#' @param rowSize Max row size.
#' @param colSize Max column size.
#' @param studySize Number of cases.
#' @param expNS Number of aliquots per case/control.
#' @param numCC Number of controls per case.
#' @param QCpct Percent of QCs for number of samples.
#' @param child Number of children per "mother' aliquot.
#' @return A practice dataset.
#' @examples
#' testR <- testRand(rowSize=20,colSize=15,studySize=1000,expNS=7000,numCC=2,QCpct=0.05,child=4)
#' @export
testRand <- function(rowSize,colSize,studySize,expNS,numCC,QCpct,child){

# generates dataset with serumID, event, studyID
td_ub <- testDataUB(studySize,expNS,numCC)

# serum packing location
td_loc <- testDataLoc(td_ub,rowSize,colSize)

# QC mother sets
numQC <- ceiling(expNS*QCpct)
numQCM <- ceiling(numQC/child)
qcM_loc <- testDataQC(numQCM,rowSize,colSize)

# QC child sets
numQCC <- numQCM*child
qcC_loc <- testDataQC(numQCC,rowSize,colSize)

testR <- list(td_ub,td_loc,qcC_loc,qcM_loc)
return(testR)
} #end test rand function

