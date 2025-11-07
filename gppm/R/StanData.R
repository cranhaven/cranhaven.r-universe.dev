new_StanData <- function(X, Y, nPer, nTime, maxTime, nPreds, IDs) {
  stopifnot(is.list(X)) # X list of length nPer, contains maxTime x nPreds matrices
  stopifnot(is.list(Y)) # Y list of length nPer, contains maxTime vectors
  stopifnot(is.numeric(nPer) && length(nPer) == 1) # number of persons scalar
  stopifnot(is.numeric(nTime)) # number of time points vector of legnth nPer
  stopifnot(is.numeric(maxTime) && length(maxTime) == 1) # max number of time points scalar
  stopifnot(is.numeric(nPreds) && length(nPreds) == 1) # number of predictors scaler

  structure(
    list(
      X = X,
      Y = Y,
      nPer = nPer,
      nTime = nTime,
      maxTime = maxTime,
      nPreds = nPreds
    ),
    class = "StanData",
    IDs = IDs
  )
}

validate_StanData <- function(myStanData) {
  stopifnot(length(myStanData$X) == myStanData$nPer)
  stopifnot(length(myStanData$Y) == myStanData$nPer)
  stopifnot(identical(dim(myStanData$X[[1]]), as.integer(c(myStanData$maxTime, myStanData$nPreds))))
  stopifnot(identical(length(myStanData$Y[[1]]), as.integer(c(myStanData$maxTime))))
  myStanData
}

StanData <- function(X, Y, nPer, nTime, maxTime, nPreds, IDs) {
  validate_StanData(new_StanData(X, Y, nPer, nTime, maxTime, nPreds, IDs))
}

as_StanData <- function(myData, ...) {
  UseMethod("as_StanData")
}

#' @export
as_StanData.LongData <- function(myData, ...) {
  ## constants
  fakeData <- 10^100

  ## helper vars
  IDField <- attr(myData, "ID")
  YField <- attr(myData, "DV")
  IDs <- myData[[IDField]]
  uniqueIDs <- unique(IDs)
  xCols <- setdiff(names(myData), c(IDField, YField))

  ## helper output
  nPer <- length(uniqueIDs)
  nTime <- table(IDs)[as.character(uniqueIDs), drop = FALSE]
  maxTime <- max(nTime)
  nPreds <- length(xCols)


  # init
  xMatrices <- matrix(data = fakeData, nrow = maxTime, ncol = nPreds)
  colnames(xMatrices) <- xCols
  X <- rep(list(xMatrices), nPer)
  Y <- rep(list(rep(fakeData, maxTime)), nPer)
  timeCounter <- rep(1, nPer)
  names(timeCounter) <- paste0("ID", uniqueIDs)

  # calc
  for (i in seq_len(nrow(myData))) {
    # helper vars
    timeCounterIndex <- names(timeCounter) == paste0("ID", myData[i, IDField])
    personI <- which(myData[i, IDField] == uniqueIDs) # which is not stupidity but necessary
    timeI <- timeCounter[timeCounterIndex]

    # core
    X[[personI]][timeI, ] <- as.numeric(myData[i, xCols])
    Y[[personI]][timeI] <- as.numeric(myData[i, YField])

    # update helper
    timeCounter[timeCounterIndex] <- timeCounter[timeCounterIndex] + 1
  }
  StanData(X, Y, nPer, nTime, maxTime, nPreds, uniqueIDs)
}

#' @export
as_StanData.default <- function(myData, ...) {
  stop(
    "Don't know how to coerce object of class ",
    paste(class(myData), collapse = "/"), " into Stan Data",
    call. = FALSE
  )
}
