###############################################################
###############################################################
###############################################################
#' @export
regCVCF <- function(x, ...) UseMethod("regCVCF")

#' Cross-Validated Committees Filter for Regression
#'
#' Application of the regCVCF noise filtering method in a regression dataset.
#'
#' In classification problems, \emph{Cross-Validated Committees Filter} (CVCF) divides the dataset into \code{nfolds} cross-validation folds and builds
#' a decision tree with {C4.5} on each one. Using each classifier, a prediction of the whole dataset is obtained.
#' Finally, a sample is considered as noisy using a voting scheme (indicated by the argument \code{vote}): if equal to \code{TRUE},
#' a consensus voting is used (in which a sample is removed if it is misclassified by all the models); if equal to \code{FALSE},
#' a majority voting is used (in which a sample is removed if it is misclassified by more than a half of the models).
#' The implementation of this noise filter to be used in regression problems follows the proposal of Martín \emph{et al.} (2021),
#' which is based on the use of a noise threshold (\code{t}) to determine the similarity between the output variable of the samples.
#'
#' @param x a data frame of input attributes.
#' @param y a double vector with the output regressand of each sample.
#' @param t a double in [0,1] with the \emph{threshold} used by regression noise filter (default: 0.2).
#' @param nfolds number of folds in which the dataset is split (default: 10).
#' @param vote a logical indicating if the consensus voting (\code{TRUE}) or majority voting (\code{FALSE}) is used (default: \code{FALSE}).
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
#' S. Verbaeten and A. Van,
#' \strong{Ensemble methods for noise elimination in classification problems},
#' \emph{in: International Workshop on Multiple Classifier Systems}, 317-325, 2003.
#' \doi{https://doi.org/10.1007/3-540-44938-8_32}.
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
#' out.def <- regCVCF(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- regCVCF(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{regIPF}}, \code{\link{regIRF}}, \code{\link{regEF}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name regCVCF
NULL

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname regCVCF
#' @importFrom "stats" "as.formula"
#' @importFrom "rpart" "rpart"
#' @importFrom "stats" "predict"
#' @importFrom "modelr" "crossv_kfold"
regCVCF.default <- function(x, y, t=0.2, nfolds=10, vote=FALSE, ...){

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
  if(nfolds < 3){
    stop("number of \"Nearest-Neighbor\" must be greater than 3")
  }

  dataset <- cbind(x, y)
  output <- ncol(dataset)
  original.data <- dataset
  dataset <- normalizeData2(dataset)
  formu <- as.formula(paste(names(dataset)[output],"~.",sep = ""))

  if(vote){
    threshold <- nfolds
  }else{
    threshold <- floor(nfolds/2)+1
  }

  folds <- crossv_kfold(dataset, nfolds)
  votes <- vector("integer", nrow(dataset))
  for(i in 1:nfolds){
    model <- rpart(formu, dataset[folds$train[[i]]$idx,], method = "anova")
    pr_rpart <- predict(model, dataset[,-output], type="vector")

    similar <- forecast(prediccion = pr_rpart, real = dataset[,output],t)
    votes <- votes + similar
  }

  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- which(votes < threshold)
  numclean <- length(idclean)
  xclean <- original.data[idclean, -ncol(original.data)]
  yclean <- original.data[idclean, ncol(original.data)]

  idnoise <- which(votes >= threshold)
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise, -ncol(original.data)]
  ynoise <- original.data[idnoise, ncol(original.data)]

  param <- list(t=t, nfolds=nfolds, vote=vote)
  call <- match.call()
  call[[1]] <- as.name("regCVCF")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Cross-Validated Committees Filter for Regression",
              param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname regCVCF
#' @importFrom "stats" "model.frame"
regCVCF.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- regCVCF.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("regCVCF")

  return(res)
}

###############################################################
###############################################################
###############################################################
