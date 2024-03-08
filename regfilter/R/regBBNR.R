###############################################################
###############################################################
###############################################################
#' @export
regBBNR <- function(x, ...) UseMethod("regBBNR")

#' Blame Based Noise Reduction for Regression
#'
#' Application of the regBBNR noise filtering method in a regression dataset.
#'
#' In classification problems, \emph{Blame Based Noise Reduction} (BBNR) removes a sample if it participates in the misclassification of another sample and
#' if its removal does not produce the misclassification on another correctly classified sample.
#' The implementation of this noise filter to be used in regression problems follows the proposal of Martín \emph{et al.} (2021),
#' which is based on the use of a noise threshold (\code{t}) to determine the similarity between the output variable of the samples.
#'
#' @param x a data frame of input attributes.
#' @param y a double vector with the output regressand of each sample.
#' @param t a double in [0,1] with the \emph{threshold} used by regression noise filter (default: 0.2).
#' @param k an integer with the number of nearest neighbors to be used (default: 5).
#' @param formula a formula with the output regressand and, at least, one input attribute.
#' @param data a data frame in which to interpret the variables in the formula.
#' @param ... other options to pass to the function.
#'
#' @return The result of applying the regression filter is a reduced dataset containing the clean samples (without errors or noise), since it removes noisy samples (those with errors).
#' This function returns an object of class \code{rfdata}, which contains information related to the noise filtering process in the form of a list with the following elements:
#' \item{xclean}{a data frame with the input attributes of clean samples (without errors).}
#' \item{yclean}{a double vector with the output regressand of clean samples (without errors).}
#' \item{numclean}{an integer with the amount of clean samples.}
#' \item{idclean}{an integer vector with the indices of clean samples.}
#' \item{xnoise}{a data frame with the input attributes of noisy samples (with errors).}
#' \item{ynoise}{a double vector with the output regressand of noisy samples (with errors).}
#' \item{numnoise}{an integer with the amount of noisy samples.}
#' \item{idnoise}{an integer vector with the indices of noisy samples.}
#' \item{filter}{the full name of the noise filter used.}
#' \item{param}{a list of the argument values.}
#' \item{call}{the function call.}
#'
#' Note that objects of the class \code{rfdata} support \link{print.rfdata}, \link{summary.rfdata} and \link{plot.rfdata} methods.
#'
#' @references
#' S. Delany and P. Cunningham,
#' \strong{An analysis of case-base editing in a spam filtering system},
#' \emph{in European Conference on Case-Based Reasoning}, 128-141, 2004.
#' \doi{https://doi.org/10.1007/978-3-540-28631-8_11}.
#'
#' J. Martín, J. A. Sáez and E. Corchado,
#' \strong{On the regressand noise problem: Model robustness and synergy with regression-adapted noise filters.}
#' \emph{IEEE Access}, 9:145800-145816, 2021.
#' \doi{https://doi.org/10.1109/ACCESS.2021.3123151}.
#' @examples
#' # load the dataset
#' data(rock)
#'
#' # usage of the default method
#' set.seed(9)
#' out.def <- regBBNR(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- regBBNR(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{regCNN}}, \code{\link{regRNN}}, \code{\link{regENN}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name regBBNR
NULL

###############################################################
###############################################################
###############################################################
#' @rdname regBBNR
#' @export
#' @importFrom "FNN" "get.knn"
#' @importFrom "FNN" "knn.reg"
regBBNR.default <- function(x, y, t=0.2, k=5, ...){

  ######### check for errors #########
  if(!is.data.frame(x)){
    stop("argument \"x\" must be a data frame")
  }
  if(!is.numeric(y)){
    stop("argument \"y\" must be a factor vector")
  }
  if(any(t < 0) || any(t > 1)){
    stop("argument \"threshold\" must be in [0,1]")
  }
  if(nrow(x) != length(y)){
    stop("number of rows of \"x\" must be equal to length of \"y\"")
  }
  if(k < 1){
    stop("number of \"Nearest-Neighbor\" must be greater than 1")
  }

  dataset <- cbind(x, y)
  output <- ncol(dataset)
  original.data <- dataset
  dataset <- normalizeData2(dataset)

  pred_nei <- get.knn(data = dataset[,-output], k = k, algorithm = "brute")$nn.index
  knnInf <- sapply(1:nrow(dataset),function(i){
    pred_reg <- knn.reg(train = dataset[-i,-output], test = dataset[i,-output], y = dataset[-i,output], k = k, algorithm = "brute")$pred
    noisysample <- forecast(prediccion = pred_reg, real = dataset[i,output], t)
    nearestNeigh <- setdiff(1:nrow(dataset),i)[pred_nei[i,]-1]
    c(noisysample, nearestNeigh)
  })
  noisysample <- as.logical(knnInf[1,])
  knnNeigh <- knnInf[-1,]

  regressorWellOther <- lapply(which(noisysample),function(i){
    id_WellOther <- which(!forecast(prediccion = dataset[knnNeigh[,i],output], real = dataset[i,output], t))
    knnNeigh[id_WellOther,i]
  })
  coverageSets <- list()
  length(coverageSets) <- nrow(dataset)
  for(i in 1:length(regressorWellOther)){
    items <- regressorWellOther[[i]]
    for(j in items){
      coverageSets[[j]] <- c(coverageSets[[j]],i)
    }
  }
  regressorOther <- lapply(which(!noisysample),function(i){
    id_Other <- which(forecast(prediccion = dataset[knnNeigh[,i],output], real = dataset[i,output], t))
    knnNeigh[id_Other,i]
  })

  toExamine <- unlist(regressorOther)
  toExamine <- sort(unique(toExamine))

  toRemove <- integer(0)
  for(i in toExamine){
    if(!is.null(coverageSets[[i]])){
      training <- dataset[setdiff(1:nrow(dataset),c(toRemove,i)),-output]
      affects_predic <- knn.reg(train = training, test = dataset[coverageSets[[i]],-output], y = dataset[setdiff(1:nrow(dataset),c(toRemove,i)),output], k = k, algorithm = "brute")$pred
      affectsRemoval <- forecast(prediccion = affects_predic, real = dataset[coverageSets[[i]],output], t)

      if(all(!affectsRemoval)){
        toRemove <- c(toRemove,i)
      }
    }else{
      toRemove <- c(toRemove,i)}
  }

  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- setdiff(1:nrow(original.data), toRemove)
  numclean <- length(idclean)
  xclean <- original.data[idclean, -ncol(original.data)]
  yclean <- original.data[idclean, ncol(original.data)]

  idnoise <- toRemove
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise, -ncol(original.data)]
  ynoise <- original.data[idnoise, ncol(original.data)]

  param <- list(k=k, t=t)
  call <- match.call()
  call[[1]] <- as.name("regBBNR")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Blame Based Noise Reduction",
              param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname regBBNR
#' @importFrom "stats" "model.frame"
regBBNR.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- regBBNR.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("regBBNR")

  return(res)
}

###############################################################
###############################################################
###############################################################
