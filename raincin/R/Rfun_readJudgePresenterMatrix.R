# Rfun_readJudgePresenterMatrix.R
# J Gou
# 2020-02-17/18
# 
#
#' @name readJudgePresenterMatrix
#' @title Transform a Judge-Presenter Matrix Converter
#' @description Convert a judge-presenter matrix to a set of square matrices
#' @param jpMat a Judge-Presenter matrix, or a User-Movie matrix
#' @return A list, including numGame 
#' \enumerate{
#'  \item \code{numGame}: Number of times teams i and j faced eath other
#'  \item \code{numWin}: Number of wins teams i plays against j 
#'  \item \code{numTie}: Number of ties teams i plays against j 
#'  \item \code{numLoss}: Number of losses teams i plays against j 
#'  \item \code{numPt}: Number of points teams i accumulates against j 
#' } 
#'
#' @author Jiangtao Gou
#' @author Shuyi Wu
#' @export
#' @import stats
#' @examples
#' jpMat <- matrix(data=c(5,4,3,0, 5,5,3,1, 0,0,0,5, 0,0,2,0, 4,0,0,3, 1,0,0,4),
#' nrow=6,byrow=TRUE)
#' result <- readJudgePresenterMatrix(jpMat)
#' print(result)
#
readJudgePresenterMatrix <- function(jpMat) {
  nJdg <- (dim(jpMat))[1]
  nPst <- (dim(jpMat))[2]
  #
  numGame <- matrix(data=rep(x=0, times=nPst*nPst), nrow=nPst)
  numWin <- matrix(data=rep(x=0, times=nPst*nPst), nrow=nPst)
  numTie <- matrix(data=rep(x=0, times=nPst*nPst), nrow=nPst)
  numLoss <- matrix(data=rep(x=0, times=nPst*nPst), nrow=nPst)
  numPt <- matrix(data=rep(x=0, times=nPst*nPst), nrow=nPst)
  #
  for (k in 1:nJdg) {
    # Matrix::nnzero(x=jpMat[k,], na.counted=FALSE) # Old way
    nzidx <- which(jpMat[k,] != 0) # none-zero indices
    lgthnzidx <- length(nzidx) 
    if (lgthnzidx > 1) {
      for (i in 1:(lgthnzidx-1)) {
        for (j in (i+1):lgthnzidx) {
          numGame[nzidx[i], nzidx[j]] <- numGame[nzidx[i], nzidx[j]] + 1
          #
          if (jpMat[k,nzidx[i]] > jpMat[k,nzidx[j]]) {
            numWin[nzidx[i], nzidx[j]] <- numWin[nzidx[i], nzidx[j]] + 1
          } else if (jpMat[k,nzidx[i]] < jpMat[k,nzidx[j]]) {
            numWin[nzidx[j], nzidx[i]] <- numWin[nzidx[j], nzidx[i]] + 1
          } else {
            numTie[nzidx[j], nzidx[i]] <- numTie[nzidx[j], nzidx[i]] + 1
            numTie[nzidx[i], nzidx[j]] <- numTie[nzidx[i], nzidx[j]] + 1
          } # End of if jpMat
          #
          numPt[nzidx[i], nzidx[j]] <- numPt[nzidx[i], nzidx[j]] + jpMat[k,nzidx[i]]
          numPt[nzidx[j], nzidx[i]] <- numPt[nzidx[j], nzidx[i]] + jpMat[k,nzidx[j]]
        } # End of for j
      } # End of for i
    } # End of if lgthnzidx
  } # End of for k
  #
  numGame[lower.tri(numGame)] <- t(numGame)[lower.tri(numGame)]
  numLoss <- t(numWin)
  #<https://stackoverflow.com/questions/26166569/copy-upper-triangle-to-lower-triangle-for-several-matrices-in-a-list>
  result <- list(numGame=numGame, numWin=numWin, numTie=numTie, numLoss=numLoss, numPt=numPt)
  return(result)
}
