###############################################################
###############################################################
###############################################################
#' @export
regDF <- function(x, ...) UseMethod("regDF")

#' Dynamic Filter for Regression
#'
#' Application of the regDF noise filtering method in a regression dataset.
#'
#' In classification, \emph{Dynamic Filter} (DF) divides the dataset into \code{nfolds} cross-validation folds and obtains the prediction of
#' 9 classifiers: SVM; k-NN with \code{k} = 3, 5 and 9; CART; C4.5; MLPN;
#' \emph{Random Forest} and \emph{Naive Bayes}. Then, it selects one ensemble of size \code{m} with best predictions.
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
#' @param m an integer in [1,9] with the number of algorithms in the ensemble (default: 3).
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
#' L. Garcia, A. Lorena and A. Carvalho,
#' \strong{A study on class noise detection and elimination},
#' \emph{Brazilian Symposium on Neural Networks}, 13-18, 2012.
#' \doi{https://doi.org/10.1109/SBRN.2012.49}.
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
#' out.def <- regDF(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- regDF(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{regEF}}, \code{\link{regGE}}, \code{\link{regHRRF}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name regDF
NULL

###############################################################
###############################################################
###############################################################
#' @rdname regDF
#' @export
#' @importFrom "stats" "as.formula"
#' @importFrom "stats" "predict"
#' @importFrom "modelr" "crossv_kfold"
#' @importFrom "e1071" "svm"
#' @importFrom "FNN" "knn.reg"
#' @importFrom "rpart" "rpart"
#' @importFrom "gbm" "gbm"
#' @importFrom "gbm" "predict.gbm"
#' @importFrom "randomForest" "randomForest"
#' @importFrom "stats" "lm"
#' @importFrom "nnet" "nnet"
#' @importFrom "utils" "capture.output"
regDF.default <- function(x, y, t=0.2, nfolds=10, m=3, vote=FALSE, ...){

  ######### check for errors #########
  if(m<1 || m>9){
    stop("parameter of m must be in [1,9]")
  }
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
  if(!(isTRUE(vote) | isFALSE(vote))){
    stop("number of \"vote\" must be TRUE or FALSE")
  }

  dataset <- cbind(x, y)
  output <- ncol(dataset)
  original.data <- dataset
  dataset <- normalizeData2(dataset)

  if(vote){
    threshold <- m
  }else{
    threshold <- floor(m/2)+1
  }

  predictions <- matrix(nrow=nrow(dataset), ncol=9)
  predictions[,1:9] <- sapply(1:9,function(key){pred_met(key, dataset, dataset, output, t)})
  ensembleInds <- order(colSums(predictions),decreasing=TRUE)[1:m]
  v <- c("SVM","3NN","5NN","9NN","RPART","GBM","RF","LR","MPNN")

  folds <- crossv_kfold(dataset, nfolds)
  votes <- integer(nrow(dataset))
  for(i in 1:nfolds){
    votes[folds$test[[i]]$idx] <- m-rowSums(sapply(ensembleInds,function(ind){pred_met(key=ind,dataset[folds$train[[i]]$idx,],dataset[folds$test[[i]]$idx,],output,t)}))
  }

  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- which(votes < threshold)
  numclean <- length(idclean)
  xclean <- original.data[idclean, -ncol(original.data)]
  yclean <- original.data[idclean, ncol(original.data)]

  idnoise <- setdiff(1:nrow(original.data), idclean)
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise, -ncol(original.data)]
  ynoise <- original.data[idnoise, ncol(original.data)]

  param <- list(t=t, nfolds=nfolds, m=m, vote=vote)
  call <- match.call()
  call[[1]] <- as.name("regDF")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Dynamic Filter",
              param = param,
              call = call)
  class(ret) <- "rfdata"

  return(ret)
}

###############################################################
###############################################################
###############################################################
pred_met <- function(key, trainn, test, output, t){
  formu <- as.formula(paste(names(trainn)[output],"~.",sep = ""))

  if(key==1){
    model <- svm(x = trainn[,-output], y = trainn[,output], type = "eps-regression", kernel = "radial")
    pred <- predict(model, test[,-output])
    out <- !forecast(prediccion = pred, real = test[,output], NS = t)
  }
  if(key%in%c(2,3,4) && !identical(trainn,test)){
    pred <- knn.reg(train = trainn[,-output],test = test[,-output],y = trainn[,output],k = getK(key), algorithm=c("brute"))$pred
    out <- !forecast(prediccion = pred, real = test[,output], NS = t)
  }
  if(key%in%c(2,3,4) && identical(trainn,test)){
    pred <- sapply(1:nrow(trainn),function(i){knn.reg(train = trainn[-i,-output],test = test[i,-output],y = trainn[-i,output],k = getK(key), algorithm=c("brute"))$pred})
    out <- !forecast(prediccion = pred, real = test[,output], NS = t)
  }
  if(key==5){
    model <- rpart(formu, trainn)
    pred <- predict(model, test[,-output], type="vector")
    out <- !forecast(prediccion = pred, real = test[,output], NS = t)
  }
  if(key==6){
    modelo <- gbm(formula = formu, distribution = "gaussian", data = trainn,n.trees = 600)
    pred <- predict.gbm(modelo, test[,-output], n.trees = 600)
    out <- !forecast(prediccion = pred, real = test[,output], NS = t)
  }
  if(key==7){
    modelo <- randomForest(trainn[,-output], y=trainn[,output], xtest=NULL, ytest=NULL, ntree=500)
    pred <- predict(modelo, test[,-output])
    out <- !forecast(prediccion = pred, real = test[,output], NS = t)
  }
  if(key==8){
    model_I <- lm(formula = formu, data = trainn)
    if(length(which(is.na(model_I$coefficients[2:length(model_I$coefficients)])))!= 0){
      model_I <- lm(formula = formu, data = trainn[,-which(is.na(model_I$coefficients[2:length(model_I$coefficients)]))])
    }
    pred <- predict(model_I, test[,-output])
    out <- !forecast(prediccion = pred, real = test[,output], NS = t)
  }
  if(key==9){
    rubbish <- capture.output(modelo <- nnet(formu, trainn, size=10, linout=TRUE, MaxNWts=ncol(trainn)^3))
    preds <- predict(modelo, test[,-output], type="raw")
    out <- !forecast(prediccion = preds, real = test[,output], NS = t)
  }
  return(out)
}

getK <- function(k){
  ifelse(k==2,3,ifelse(k==3,5,9))
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname regDF
#' @importFrom "stats" "model.frame"
regDF.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- regDF.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("regDF")

  return(res)
}

###############################################################
###############################################################
###############################################################
