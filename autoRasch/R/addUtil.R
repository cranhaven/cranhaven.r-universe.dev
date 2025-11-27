
##### automatic initial rescore ######

# downCoding <- function(X){
#
#   # datebeforecheck <- IEPS_dataset[,5:22]
#   datebeforecheck <- X
#   # datebeforecheck[which(datebeforecheck == 0)] <- NA
#   nitem <- ncol(X)
#   dataaftercheck <- c()
#
#   for(i in 1:nitem){
#     print(i)
#     tempResp <- datebeforecheck[,i]
#     maxResp <- max(tempResp,na.rm = TRUE)
#     minResp <- min(tempResp,na.rm = TRUE)
#     nfactor <- factor(tempResp)
#     catLevel <- levels(nfactor)
#     tempCatLevel <- catLevel
#     for(j in 1:length(catLevel)){
#       tempResp[which(tempResp >= as.numeric(catLevel[j]))] <- tempResp[which(tempResp >= as.numeric(catLevel[j]))] - (as.numeric(catLevel[j]) - (j-1))
#       tempnfactor <- factor(tempResp)
#       tempCatLevel <- levels(tempnfactor)
#       nfactor <- factor(tempResp)
#       catLevel <- levels(nfactor)
#     }
#     dataaftercheck <- cbind(dataaftercheck,tempResp)
#   }
#
#   colnames(dataaftercheck) <- paste("V",c(1:nitem))
#   return(dataaftercheck)
# }


###### downcoding follow minimum ##########

downCodingMin <- function(X){

  datebeforecheck <- X
  nitem <- ncol(X)
  dataaftercheck <- c()

  for(i in 1:nitem){
    tempResp <- datebeforecheck[,i]
    minResp <- min(tempResp,na.rm = TRUE)
    tempResp <- tempResp - minResp
    dataaftercheck <- cbind(dataaftercheck,tempResp)
  }

  colnames(dataaftercheck) <- paste("V",c(1:nitem))
  return(dataaftercheck)
}


###### rescore threshold disorder ########

rescore <- function(X, itemno, collapseCat){

  dataTemp <- X
  tempResp <- dataTemp[,itemno]
  maxResp <- max(tempResp,na.rm = TRUE)
  minResp <- min(tempResp,na.rm = TRUE)

  tempResp[which(tempResp %in% collapseCat)] <- min(collapseCat)

  nfactor <- factor(tempResp)
  catLevel <- levels(nfactor)

  for(j in seq_along(catLevel)){
    tempResp[which(tempResp >= as.numeric(catLevel[j]))] <- tempResp[which(tempResp >= as.numeric(catLevel[j]))] - (as.numeric(catLevel[j]) - (minResp+(j-1)))
    nfactor <- factor(tempResp)
    catLevel <- levels(nfactor)
  }

  dataTemp[,itemno] <- tempResp
  return(dataTemp)

}

##### map subject to DIF group #####


