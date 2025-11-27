#' Create Mapping Matrix of DIF Groups
#'
#' This function automatically create a mapping matrix based on the existing DIF inducing covariates.

#' @param backInfo A matrix of person background information (e.g., gender, country, age, etc);
#' @param idxUsed The column number of \code{backInfo} that is used for creating the mapping matrix.
#' @param contMethod The method of how to handle a continuous variable (e.g., mean, median). This parameter is passing a function used to split the variable into binary. The default is \code{mean}.
#'
#' @return
#' A binary matrix that maps respondents to the groups that the respondents belongs to.
#'
#' @export
createGroup <- function(backInfo, idxUsed = NULL, contMethod = c("mean","median")){

  bckInfo <- as.matrix(backInfo)
  rownum <- nrow(bckInfo)
  if(is.null(idxUsed)){
    difCov <- seq_len(ncol(bckInfo))
  } else {
    difCov <- idxUsed
  }
  tempName <- c()
  contThMethod <- contMethod[1]
  tempMap <- c()

  for(i in seq_along(difCov)){
    tempLevel <- levels(factor(bckInfo[,difCov[i]]))
    tempFact <- length(tempLevel)
    ### For continuous variables ###
    if(tempFact > 10){
      tempTh <- do.call(contThMethod, list(bckInfo[,difCov[i]],na.rm = TRUE))
      vecMap <- rep(0,rownum)
      vecMap[which(bckInfo[,difCov[i]] >= tempTh)] <- 1
      tempMap <- cbind(tempMap, vecMap)
      tempName <- c(tempName,colnames(bckInfo)[difCov[i]])
    } else {
      ### for binary ###
      if(tempFact == 2){
        vecMap <- rep(0,rownum)
        vecMap[which(bckInfo[,difCov[i]] >= as.numeric(tempLevel[2]))] <- 1
        tempMap <- cbind(tempMap,vecMap)
        tempName <- c(tempName,colnames(bckInfo)[difCov[i]])
      } else {
        for(j in 1:tempFact){
          vecMap <- rep(0,rownum)
          vecMap[which(bckInfo[,difCov[i]] == as.numeric(tempLevel[j]))] <- 1
          tname <- paste(colnames(bckInfo)[difCov[i]],j,sep = "_")
          tempMap <- cbind(tempMap,vecMap)
          tempName <- c(tempName,tname)
        }
      }
    }
  }

  colnames(tempMap) <- tempName
  NAmap <- bckInfo*0
  NAmap <- apply(NAmap,1,sum) + 1
  tempMap <- tempMap * NAmap
  return(tempMap)
}
