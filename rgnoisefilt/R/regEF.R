###############################################################
###############################################################
###############################################################
#' @export
regEF <- function(x, ...) UseMethod("regEF")

#' Ensemble Filter for Regression
#'
#' Application of the regEF noise filtering method in a regression dataset.
#'
#' In classification, \emph{Ensemble Filter} (EF) divides the dataset into \code{nfolds} cross-validation folds.
#' Then, a prediction is obtained for each one of the classifiers --C4.5, NN and LDA.
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
#' @param vote a logical indicating if the consensus voting (\code{TRUE}) or majority voting (\code{FALSE}) is used (default: \code{TRUE}).
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
#' C. Brodley and M. Friedl,
#' \strong{Identifying mislabeled training data},
#' \emph{Journal of Artificial Intelligence Research}, 11:131-167, 1999.
#' \doi{https://doi.org/10.1613/jair.606}.
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
#' out.def <- regEF(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- regEF(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{regDF}}, \code{\link{regCVCF}}, \code{\link{regIPF}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name regEF
NULL

###############################################################
###############################################################
###############################################################
#' @rdname regEF
#' @export
#' @importFrom "stats" "as.formula"
#' @importFrom "stats" "predict"
#' @importFrom "modelr" "crossv_kfold"
#' @importFrom "rpart" "rpart"
#' @importFrom "FNN" "knn.reg"
#' @importFrom "stats" "lm"
regEF.default <- function(x, y, t=0.2, nfolds=10, vote=TRUE, ...){

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
    threshold <- 3
  }else{
    threshold <- 2
  }

  folds <- crossv_kfold(dataset, nfolds)
  votes <- vector("integer",nrow(dataset))
  for(i in 1:nfolds){
    model <- rpart(formu, dataset[folds$train[[i]]$idx,], method = "anova")
    pr_rpart <- predict(model ,dataset[folds$test[[i]]$idx,-output], type = "vector")
    similar_rpart <- !forecast(prediccion = pr_rpart, real = dataset[folds$test[[i]]$idx,output], t)
    votes[folds$test[[i]]$idx] <- votes[folds$test[[i]]$idx] + similar_rpart

    pr_knn <- knn.reg(train = dataset[folds$train[[i]]$idx,-output], test = dataset[folds$test[[i]]$idx,-output], y = dataset[folds$train[[i]]$idx,output], k = 1, algorithm = "brute")$pred
    similar_knn <- !forecast(prediccion = pr_knn, real = dataset[folds$test[[i]]$idx,output],t)
    votes[folds$test[[i]]$idx] <- votes[folds$test[[i]]$idx]+similar_knn

    model_I <- lm(formula = formu, data = dataset[folds$train[[i]]$idx,])
    if(length(which(is.na(model_I$coefficients[2:length(model_I$coefficients)])))!= 0){
      model_I <- lm(formula = formu, data = dataset[folds$train[[i]]$idx,-which(is.na(model_I$coefficients[2:length(model_I$coefficients)]))])
    }
    pr_lm <- predict(model_I, dataset[folds$test[[i]]$idx,-output])
    similar_lm <- !forecast(prediccion = pr_lm, real = dataset[folds$test[[i]]$idx,output], t)
    votes[folds$test[[i]]$idx] <- votes[folds$test[[i]]$idx] + similar_lm
  }

  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- which(votes >= threshold)
  numclean <- length(idclean)
  xclean <- original.data[idclean, -ncol(original.data)]
  yclean <- original.data[idclean, ncol(original.data)]

  idnoise <- setdiff(1:nrow(original.data), idclean)
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise, -ncol(original.data)]
  ynoise <- original.data[idnoise, ncol(original.data)]

  param <- list(t=t, nfolds=nfolds, vote=vote)
  call <- match.call()
  call[[1]] <- as.name("regEF")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Ensemble Filter",
              param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname regEF
#' @importFrom "stats" "model.frame"
regEF.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- regEF.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("regEF")

  return(res)
}

###############################################################
###############################################################
###############################################################
