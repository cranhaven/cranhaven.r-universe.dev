#' Create Leave-persons-out Folds
#'
#' This function is used to create a leave-persons-out cross-validation fold vector to be used by \code{\link{crossvalidate}}.
#'
#' @inheritParams nPers
#'
#' @param k integer scalar. Number of folds to create.
#'
#' @return A fold vector, which is a vector of length \code{nrow(getData(gpModel))} of integers from 1 to k. If \code{foldVector[i]=j}, then data point i is assigned to fold j.
#' @seealso \code{\link{crossvalidate}} for how to use the created fold vector to perform cross-validation.
#' @details The folds are created such that the data of each person is fully in one fold.
#' @examples
#' \donttest{
#' data("demoLGCM")
#' lgcm <- gppm(
#'   "muI+muS*t", "varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma",
#'   demoLGCM, "ID", "y"
#' )
#' theFolds <- createLeavePersonsOutFolds(lgcm)
#' }
#'
#' @export
createLeavePersonsOutFolds <- function(gpModel, k = 10) {
  checkGPPM(gpModel)

  # get all person ids
  theData <- getData(gpModel)
  idCol <- getID(theData)
  theIDs <- theData[, idCol]
  uniqueIDs <- sort(unique(theIDs))
  nPers <- length(uniqueIDs)
  # decide which person to put in which fold
  if (k <= nPers) {
    min_reps <- nPers %/% k
    if (min_reps > 0) {
      spares <- nPers %% k
      seqVector <- rep(1:k, min_reps)
      if (spares > 0) {
        seqVector <- c(seqVector, sample(1:k, spares))
      }
      foldVector <- sample(seqVector)
    } else {
      foldVector <- sample(1:k, size = nPers)
    }
  } else {
    stop("Fewer Persons than folds requested. Consider using lower k.")
  }

  # build fold vector for each observation of each person
  foldVectorLong <- vector(mode = "integer", nrow(theData))
  for (i in seq_len(nrow(theData))) {
    personIdx <- theData[i, idCol] == uniqueIDs
    stopifnot(sum(personIdx) == 1)
    foldVectorLong[i] <- foldVector[personIdx]
  }
  return(foldVectorLong)
}

validate_cross <- function(gpModel, foldVector) {
  # gpModel
  checkGPPM(gpModel)

  # foldVector
  n <- nrow(getData(gpModel))
  if (length(foldVector) != n) {
    stop("foldVector invalid length")
  }
  if (!identical(sort(unique(foldVector)), 1:max(foldVector))) {
    stop("foldVector invalid format")
  }

  if (max(foldVector) < 2) {
    stop("foldVector invalid format")
  }
}

#' Cross-validation.
#'
#' Performs cross-validation of a Gaussian process panel model.
#'
#' @inheritParams nPers
#'
#' @param foldVector integer vector. Describes the foldstructure to use. For example, created by \code{\link{createLeavePersonsOutFolds}}.
#'
#' @return Cross-validation estimates of the mean squared error (MSE) and the negative log-predictive probability (nLPP)
#' @details The fold vector, must be a vector of length nrow(getData(gpModel)) of integers from 1 to k. If \code{foldVector[i]=j}, then data point i is assigned to fold j.
#' @examples
#' \donttest{
#' data("demoLGCM")
#' lgcm <- gppm(
#'   "muI+muS*t", "varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma",
#'   demoLGCM, "ID", "y"
#' )
#' theFolds <- createLeavePersonsOutFolds(lgcm, k = 2) # for speed, in practive rather use default k=10
#' crosRes <- crossvalidate(lgcm, theFolds)
#' crosRes$MSE # mean squared error
#' crosRes$nLPP # negative log-predictive probability
#' }
#' @export
crossvalidate <- function(gpModel, foldVector) {
  validate_cross(gpModel, foldVector)


  nFolds <- max(foldVector)
  theData <- getData(gpModel)
  resnLPP <- 0
  resSE <- 0
  for (cFold in 1:nFolds) {
    trainRows <- foldVector != cFold
    testRows <- foldVector == cFold
    # train model
    tmpModel <- subsetData(gpModel, trainRows)
    tmpModel <- fit(tmpModel)
    # get predictions
    thePreds <- predict(tmpModel, theData[testRows, ])
    theAcc <- accuracy(thePreds)
    resnLPP <- resnLPP + theAcc$nLPP
    resSE <- resSE + theAcc$SSE
  }
  mse <- resSE / nrow(theData)
  return(list(MSE = mse, nLPP = resnLPP))
}
