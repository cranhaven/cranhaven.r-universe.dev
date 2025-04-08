##' Convert Z score to correponding p-values
##' @note Due to the IEEE number limits of representing doubles,
##' any \eqn{Z} score over 37.51929999999999765 will be converted to a \eqn{p}-value of 1e-309.
##' @title Convert Z-score to correponding p-value
##' @export
##' @param z.score Z-score(s) (either scalar or vector).
##' @param ... not used.
##' @return corresponding \eqn{p}-value(s).
##' @import stats
##' @author Beibei Jiang \email{beibei_jiang@@psych.mpg.de} and Benno Pütz \email{puetz@@psych.mpg.de}
ZtoP <- function(z.score, ...){
  if(max(z.score)<=37.51929999999999765){
    return(2*pnorm(-abs(as.numeric(z.score))))
  }else{
    warning("There is some Z score value over the system length. After converting, the p-value is recorded as 1e-309.")
    pvals <- 2*pnorm(-abs(as.numeric(z.score)))
    pvals[pvals==0] <- 1e-309
    return(pvals)
  }
}


##' index set for idx-th chunk of size chunk for n elements
##'
##' For proper use of this function it will return the set of variant indices
##' corresponding to the \code{idx}-th chunk of size \code{chunk} in \code{n} variants, taking
##' care of the case that the last chunk might have less than \code{n} elements.
##' If used with an \code{idx}-value outside the possible chunks (i.e., negative or
##' larger than \code{ceiling(n/chunk)}) an empty vector (\code{numeric(0)}) is returned.
##' @param idx chunk index (which chunk, first is 1)
##' @param n total number of variants
##' @param chunk desired chunksize
##' @return index range into variants for chunk \code{idx} (see details)
##' @author Benno Pütz \email{Benno Pütz \email{puetz@@psych.mpg.de}
ithChunk <- function(idx, n, chunk = 1000){
  start <- (idx-1) * chunk + 1   #
  return(if(idx < 1 || start>n){
    numeric(0)   # this should not be reached
  } else {
    start:min(idx * chunk, n)   # take cae of "incomplete" chunks
  })
}








##' @description Write out the result of epistasis analysis. Z score matrix is not a symmetric matrix.
##' @title Write out epistasis result (normal matrix)
##' @export
##' @param Zmatrix is the Z score matrix (non-symmetric matrix).
##' @param indexArr is the index of Zmarix whose z score is over the given \code{zpthres}.
##' @param outfile is the SNP pairs file for the second stage.
##' @param ... not used.
##' @return null
##' @import utils
##' @author Beibei Jiang \email{beibei_jiang@@psych.mpg.de}
WriteSnpPairs <- function(Zmatrix,
                          indexArr,
                          outfile = "NONE",
                          ...){
  if(!is.null(indexArr))
  {
    if(is.matrix(indexArr))
    {
      SNP1 <- rownames(Zmatrix)[indexArr[,1]]
      SNP2 <- colnames(Zmatrix)[indexArr[,2]]
      Zscore <- Zmatrix[indexArr]
    }else
    {
      SNP1 <- rownames(Zmatrix)[indexArr[1]]
      SNP2 <- colnames(Zmatrix)[indexArr[2]]
      Zscore <- Zmatrix[indexArr[1], indexArr[2]]
    }
    pairdata <- data.frame(cbind(SNP1, SNP2, Zscore), stringsAsFactors = FALSE)
    rm(SNP1)
    rm(SNP2)
    rm(Zscore)
    if (nrow(pairdata)>0){
      pairdata$ZP <- ZtoP(pairdata$Zscore)
      write.table(pairdata, outfile, col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    }
  }
}






##' @description Write out the result of epistasis analysis. Z score matrix is a symmetric matrix.
##' @title Write out epistasis result (symmetric matrix)
##' @export
##' @param Zmatrix is the Z score matrix (symmetric matrix).
##' @param indexArr is the index of Zmarix whose z score is over the given \code{zpthres}.
##' @param outfile is the SNP pairs file for the second stage.
##' @param ... not used.
##' @return null
##' @import utils
##' @author Beibei Jiang \email{beibei_jiang@@psych.mpg.de}
WriteSnpPairs_sym <- function(Zmatrix,
                              indexArr,
                              outfile = "NONE",
                              ...){
  if(!is.null(indexArr))
  {
    indexArr.nodup <- indexArr[indexArr[,1] < indexArr[,2],]
    if(is.matrix(indexArr.nodup))
    {
      SNP1 <- rownames(Zmatrix)[indexArr.nodup[,1]]
      SNP2 <- colnames(Zmatrix)[indexArr.nodup[,2]]
      Zscore <- Zmatrix[indexArr.nodup]
    }else
    {
      SNP1 <- rownames(Zmatrix)[indexArr.nodup[1]]
      SNP2 <- colnames(Zmatrix)[indexArr.nodup[2]]
      Zscore <- Zmatrix[indexArr.nodup[1], indexArr.nodup[2]]
    }
    
    pairdata <- data.frame(cbind(SNP1, SNP2, Zscore),
                           stringsAsFactors = FALSE)
    rm(SNP1)
    rm(SNP2)
    rm(Zscore)
    if (nrow(pairdata)>0){
      pairdata$ZP <- ZtoP(pairdata$Zscore)
      write.table(pairdata, outfile, col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    }
  }
}

