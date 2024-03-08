###############################################################
###############################################################
###############################################################
#' @export
regCNN <- function(x, ...) UseMethod("regCNN")

#' Condensed Nearest Neighbors for Regression
#'
#' Application of the regCNN noise filtering method in a regression dataset.
#'
#' \emph{Condensed Nearest Neighbors} (CNN) seeks to obtain a data subset that improves the quality of the original dataset.
#' In classification problems, CNN performs a first classification and stores all the samples that are misclassified.
#' Then, those stored samples are taken as a training set. The process stops when all the unstored samples are correctly classified.
#' The implementation of this noise filter to be used in regression problems follows the proposal of Martín \emph{et al.} (2021),
#' which is based on the use of a noise threshold (\code{t}) to determine the similarity between the output variable of the samples.
#'
#' @param x a data frame of input attributes.
#' @param y a double vector with the output regressand of each sample.
#' @param t a double in [0,1] with the \emph{threshold} used by regression noise filter (default: 0.2).
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
#' L. Devroye, L. Gyorfi and G. Lugosi,
#' \strong{Condensed and edited nearest neighbor rules.}
#' \emph{In: A Probabilistic Theory of Pattern Recognition}, 31:303-313, 1996.
#' \doi{https://doi.org/10.1007/978-1-4612-0711-5_19}.
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
#' out.def <- regCNN(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- regCNN(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{regRNN}}, \code{\link{regENN}}, \code{\link{regBBNR}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name regCNN
NULL

###############################################################
###############################################################
###############################################################
#' @rdname regCNN
#' @export
#' @importFrom "FNN" "knn.reg"
regCNN.default <- function(x, y, t=0.2, ...){

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

  dataset <- cbind(x, y)
  output <- ncol(dataset)
  original.data <- dataset
  dataset <- normalizeData2(dataset)

  firstDif <- which(forecast(prediccion = dataset[,output], real = dataset[1,output], t))[1]
  if (firstDif == 1) {store <- 1
  }else{store <- c(1, firstDif)}
  grabBag <- setdiff(1:firstDif, store)

  for(i in (firstDif+1):nrow(dataset)){
    nn_pred <- knn.reg(train = dataset[store,-output], test = dataset[i,-output], y = dataset[store,output], k = 1, algorithm = c("brute"))$pred
    areDifferent <- forecast(prediccion = nn_pred, real = dataset[i,output], t)
    if(areDifferent){
      grabBag <- c(grabBag,i)
    }else{
      store <- c(store,i)
    }
  }

  KeepOn <- TRUE
  while(KeepOn){
    KeepOn <- FALSE
    for(i in grabBag){
      p_nn <- knn.reg(train = dataset[store,-output], test = dataset[i,-output],y = dataset[store,output],k = 1, algorithm=c("brute"))$pred
      areSimilar <- forecast(prediccion = p_nn, real = dataset[i,output], t)
      if(!areSimilar){
        store <- c(store,i)
        grabBag <- setdiff(grabBag,i)
        KeepOn <- TRUE
      }
    }
  }

  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- sort(store)
  numclean <- length(idclean)
  xclean <- original.data[idclean,-ncol(original.data)]
  yclean <- original.data[idclean,ncol(original.data)]
  idnoise <- sort(grabBag)
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise,-ncol(original.data)]
  ynoise <- original.data[idnoise,ncol(original.data)]

  param <- list(t=t)
  call <- match.call()
  call[[1]] <- as.name("regCNN")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Condensed Nearest Neighbors",
              param = param,
              call = call)
  class(ret) <- "rfdata"

  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname regCNN
#' @importFrom "stats" "model.frame"
regCNN.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- regCNN.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("regCNN")

  return(res)
}

###############################################################
###############################################################
###############################################################
