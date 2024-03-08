###############################################################
###############################################################
###############################################################
#' @export
regHRRF <- function(x, ...) UseMethod("regHRRF")

#'  Hybrid Repair-Remove Filter for Regression
#'
#' Application of the regHRRF noise filtering method in a regression dataset.
#'
#' \code{regHRRF} is an adaptation of \emph{Hybrid Repair-Remove Filter} (HRRF) found in the field of classification, which builds a classifier set using
#' SVM, MLPNN, CART and k-NN (\code{k}= 1, 3 and 5) on the dataset. HRRF removes noisy samples depending on chosen \emph{voting} scheme
#' (indicated by the argument \code{vote}): if equal to \code{TRUE}, a consensus voting is used (in which a sample is removed if it is misclassified by all the models);
#' if equal to \code{FALSE}, a majority voting is used (in which a sample is removed if it is misclassified by more than a half of the models).
#' The process is repeated while the prediction accuracy (over the original dataset) of the ensemble increases.
#' The implementation of this noise filter to be used in regression problems follows the proposal of Martín \emph{et al.} (2021),
#' which is based on the use of a noise threshold (\code{t}) to determine the similarity between the output variable of the samples.
#'
#' @param x a data frame of input attributes.
#' @param y a double vector with the output regressand of each sample.
#' @param t a double in [0,1] with the \emph{threshold} used by regression noise filter (default: 0.2).
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
#' A. Miranda, L. Garcia, A. Carvalho and A. Lorena,
#' \strong{Use of classification algorithms in noise detection and elimination}
#' \emph{in Hybrid Artificial Intelligence Systems}, 417-424, 2009.
#' \doi{https://doi.org/10.1007/978-3-642-02319-4_50}.
#'
#' J. Martín, J. A. Sáez and E. Corchado,
#' \strong{On the regressand noise problem: Model robustness and synergy with regression-adapted noise filters.}
#' \emph{IEEE Access}, 9:145800-145816, 2021.
#' \doi{https://doi.org/10.1109/ACCESS.2021.3123151}.
#' @examples
#' # load the dataset
#' data(rock) # data regresion
#'
#' # usage of the default method
#' set.seed(9)
#' out.def <- regHRRF(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- regHRRF(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{regIPF}}, \code{\link{regEF}}, \code{\link{regFMF}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name regHRRF
NULL

###############################################################
###############################################################
###############################################################
#' @rdname regHRRF
#' @export
#' @importFrom "stats" "as.formula"
#' @importFrom "e1071" "svm"
#' @importFrom "nnet" "nnet"
#' @importFrom "FNN" "knn.reg"
#' @importFrom "rpart" "rpart"
#' @importFrom "utils" "capture.output"
#' @importFrom "stats" "predict"
regHRRF.default <- function(x, y, t=0.2, vote=FALSE, ...){

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
  formu <- as.formula(paste(names(dataset)[output],"~.",sep = ""))
  row.names(dataset) <- 1:nrow(dataset)

  threshold <- ifelse(vote,4,2)
  KeepOn <- TRUE
  correctRegressor <- 0
  counter <- 1
  while(KeepOn){
    preds <- predictions(trainn = dataset, test = dataset, formu = formu, t = t)
    votesNoise <- rowSums(apply(preds, 2, function(v){forecast(prediccion = v, real = dataset[,output], t)}))
    noiseInds <- which(votesNoise >= threshold)

    dataTemp <- dataset[setdiff(1:nrow(dataset),noiseInds),]

    predsOriginal <- predictions(trainn = dataTemp, test = normalizeData2(original.data), formu = formu, t = t)
    correctRegressorNew <- sum(apply(predsOriginal, 2, function(v){!forecast(prediccion = v, real = original.data[,output], NS = t)}))
    if(correctRegressorNew <= correctRegressor){
      KeepOn <- FALSE
    }else{
      correctRegressor <- correctRegressorNew
      counter <- counter+1
      dataset <- dataTemp
    }
  }

  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- as.integer(row.names(dataTemp))
  numclean <- length(idclean)
  xclean <- original.data[idclean, -ncol(original.data)]
  yclean <- original.data[idclean, ncol(original.data)]

  idnoise <- setdiff(1:nrow(original.data), as.integer(row.names(dataTemp)))
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise, -ncol(original.data)]
  ynoise <- original.data[idnoise, ncol(original.data)]

  param <- list(t = t, vote = vote)
  call <- match.call()
  call[[1]] <- as.name("regHRRF")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Hybrid Remove Regressor Filter",
              param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname regHRRF
#' @importFrom "stats" "model.frame"
regHRRF.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- regHRRF.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("regHRRF")

  return(res)
}
###############################################################
###############################################################
###############################################################
predictions <- function(trainn, test, formu, t){
  out <- matrix(nrow = nrow(test), ncol = 4)
  output <- ncol(trainn)
  ob_train <- trainn[,-output]
  ob_test <- test[,-output]

  model_svm <- svm(x = ob_train, y = trainn[,output], type = "eps-regression", kernel = "radial")
  pred_svm <- predict(model_svm, ob_test)
  out[,1] <- pred_svm

  rubbish <- capture.output(model_net <- nnet(x = ob_train, y = trainn[,output], size=10, linout=TRUE,  MaxNWts=ncol(trainn)^3))
  pred_net <- predict(model_net, ob_test, type="raw")
  out[,2] <- pred_net

  model_rpart <- rpart(formu, trainn, method  = "anova")
  pred_rpart <- predict(model_rpart, ob_test, type="vector")
  out[,3] <- pred_rpart

  knnPreds <- matrix(nrow = nrow(test),ncol = 3)

  kk <- c(1,3,5)
  for(ik in 1:3){
    if(all(dim(trainn)==dim(test)) && all(trainn==test)){
      for(it in 1:nrow(trainn)){
        knnPreds[it,ik] <- knn.reg(train = ob_train[-it,], test = ob_test[it,],y = trainn[-it,output], k = kk[ik], algorithm=c("brute"))$pred
      }
    }else{
      knnPreds[,ik] <- knn.reg(train = ob_train, test = ob_test, y = trainn[,output], k = kk[ik], algorithm=c("brute"))$pred
    }
  }

  knnSimilar <- matrix(nrow = nrow(test),ncol = 3)
  for(p in 1:nrow(test)){
    knnSimilar[p,] <- forecast(prediccion = knnPreds[p,],real = test[p,output],NS = t)
    if(any(knnSimilar[p,])){out[p,4] <- mean(knnPreds[p,which(knnSimilar[p,]==T)])
    }else{out[p,4] <- mean(knnPreds[p,])}
  }
  return(out)
}
