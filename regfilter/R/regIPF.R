###############################################################
###############################################################
###############################################################
#' @export
regIPF <- function(x, ...) UseMethod("regIPF")

#' Iterative Partitioning Filter for Regression
#'
#' Application of the regIPF noise filtering method in a regression dataset.
#'
#' In classification, \emph{Iterative Partitioning Filter} (IPF) builds a classifier with C4.5 on each fold (\code{nfolds}) to evaluate the whole dataset.
#' The noisy samples are removed depending on the chosen voting scheme (indicated by the argument \code{vote}): if equal to \code{TRUE},
#' a consensus voting is used (in which a sample is removed if it is misclassified by all the models); if equal to \code{FALSE},
#' a majority voting is used (in which a sample is removed if it is misclassified by more than a half of the models).
#' In addition, IPF integrates an iterative process that stops depending on the arguments \code{p}, \code{s} and \code{i}.
#' The implementation of this noise filter to be used in regression problems follows the proposal of Martín \emph{et al.} (2021),
#' which is based on the use of a noise threshold (\code{t}) to determine the similarity between the output variable of the samples.
#'
#'
#' @param x a data frame of input attributes.
#' @param y a double vector with the output regressand of each sample.
#' @param t a double in [0,1] with the \emph{threshold} used by regression noise filter (default: 0.2).
#' @param nfolds number of folds in which the dataset is split (default: 10).
#' @param p a double in [0,1] with the minimum proportion of original samples that must be labeled as noisy (default: 0.4).
#' @param s an integer with the number of iterations without improvement for the stopping criterion (default: 3).
#' @param i a double in [0,1] with the proportion of good samples which must be retained per iteration (default: 0.5).
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
#' T. M. Khoshgoftaar and P. Rebours,
#' \strong{Improving software quality prediction by noise filtering techniques},
#' \emph{Journal of Computer Science and Technology}, 22:387-396, 2007.
#' \doi{https://doi.org/10.1007/s11390-007-9054-2}
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
#' out.def <- regIPF(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- regIPF(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{regIRF}}, \code{\link{regCVCF}}, \code{\link{regFMF}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name regIPF
NULL

###############################################################
###############################################################
###############################################################
#' @rdname regIPF
#' @export
#' @importFrom "stats" "as.formula"
#' @importFrom "stats" "predict"
#' @importFrom "rpart" "rpart"
regIPF.default <- function(x, y, t=0.4, nfolds=10, vote=FALSE, p=0.01, s=3, i=0.5, ...){

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
  if(s < 1){
    stop('"s" parameter must be greater than 1')
  }
  if(any(p < 0) || any(p > 1)){
    stop('"p" parameter must be in [0,1]')
  }
  if(any(i < 0) || any(i > 1)){
    stop('"i" parameter must be in [0,1]')
  }
  if(nfolds < 3){
    stop("number of \"Nearest-Neighbor\" must be greater than 3")
  }

  dataset <- cbind(x, y)
  output <- ncol(dataset)
  original.data <- dataset
  dataset <- normalizeData2(dataset)

  origSize <- nrow(dataset)
  formu <- as.formula(paste(names(dataset)[output],"~.",sep = ""))
  row.names(dataset) <- 1:nrow(dataset)

  if(vote){
    majThreshold <- nfolds
  }else{majThreshold <- floor(nfolds/2)+1}

  stopThreshold <- floor(nrow(dataset)*p)
  KeepOn <- TRUE
  counter <- 0
  countIter <- 0
  Dg <- data.frame()

  while(KeepOn){
    countIter <- countIter+1
    VotesGeneral <- vector("integer",nrow(dataset))
    VotesLocal <- vector("integer",nrow(dataset))

    folds <- as.list(crossv_kfold(dataset, nfolds))

    for(i in 1:nfolds){
      model_rpart <- rpart(formu, dataset[folds$test[[i]]$idx,], method = "anova")
      pr_rpart <- predict(model_rpart, dataset[,-output], type = "vector")

      VotesGeneral <- VotesGeneral+forecast(prediccion = pr_rpart, real = dataset[,output], t)
      VotesLocal[folds$test[[i]]$idx] <- VotesLocal[folds$test[[i]]$idx]+forecast(prediccion = pr_rpart[folds$test[[i]]$idx], real = dataset[folds$test[[i]]$idx,output],t)
    }

    NoisyIndexes <- which((VotesGeneral >= majThreshold) & (VotesLocal==1))
    GoodIndexes <- which(VotesGeneral==0)
    AmountGoodInst <- min(length(NoisyIndexes),round(i*length(GoodIndexes)))
    GoodIndexesToKeep <- sample(GoodIndexes, AmountGoodInst, replace = FALSE)

    Dg <- rbind(Dg,dataset[GoodIndexesToKeep,])
    if(length(NoisyIndexes)>0){
      dataset <- dataset[-c(NoisyIndexes,GoodIndexesToKeep),]
    }

    if(length(NoisyIndexes) <= stopThreshold & counter+1==s) KeepOn <- FALSE
    if(length(NoisyIndexes) <= stopThreshold & counter+1<s) counter <- counter+1
    if(length(NoisyIndexes) > stopThreshold) counter <- 0
  }
  finalData <- rbind(dataset, Dg)

  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- sort(as.integer(row.names(finalData)))
  numclean <- length(idclean)
  xclean <- original.data[idclean, -ncol(original.data)]
  yclean <- original.data[idclean, ncol(original.data)]

  idnoise <- setdiff(1:origSize, as.integer(row.names(finalData)))
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise, -ncol(original.data)]
  ynoise <- original.data[idnoise, ncol(original.data)]

  param <- list(t = t, nfolds = nfolds, vote = vote)
  call <- match.call()
  call[[1]] <- as.name("regIPF")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Iterative Partitioning Filter",
              param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname regIPF
#' @importFrom "stats" "model.frame"
regIPF.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- regIPF.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("regIPF")

  return(res)
}

###############################################################
###############################################################
###############################################################
