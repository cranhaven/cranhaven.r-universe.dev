

######################################################################
##
## File Name: general_fcns.R
## Description: general functions used in the randomizer package
## Date Created: 12/6/2024
## Last Updated: 3/11/2025
##
######################################################################

## FUNCTION: packLoc()
## DESCRIPTION: assign locations within a packing list for the serum

packLoc <- function(dataS,maxRow,maxCol){

  #Rack (1-9), Row (1-9), Col (1-9)
  dataS$rack <- 0
  dataS$row <- 0
  dataS$col <- 0
  cc <- 0
  rr <- 1
  ra <- 1
  maxDat <- nrow(dataS)
  for (l in 1:maxDat){
    if ((rr == maxRow)&(cc == maxCol)){
      cc <- 1
      rr <- 1
      ra <- ra+1
    }else if (cc == maxCol){
        cc <- 1
        rr <- rr + 1
    }else{
        cc <- cc+1
    }
    dataS$rack[[l]] <- ra
    dataS$row[[l]] <- rr
    dataS$col[[l]] <- cc
  }
  return(dataS)
} #end pacLoc


######################################################################

## FUNCTION: uniqueID()
## DESCRIPTION: tests for unique IDs

#' Unique IDs
#'
#' @description
#' Test for unique IDs.
#'
#' @param testD Test dataset.
#' @param IDN ID to test.
#' @return Any IDs that are not unique.
#' @examples
#' test <- uniqueID(serumRand,"serumID")
#' @export
uniqueID <- function(testD,IDN){
  Freq=NULL
  testD2 <- data.frame(xtabs(~get(IDN),data=get("testD"))) %>%
            filter(Freq > 1)

  return(testD2)

} #end function
