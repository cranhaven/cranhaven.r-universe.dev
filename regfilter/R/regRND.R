###############################################################
###############################################################
###############################################################
#' @export
regRND <- function(x, ...) UseMethod("regRND")

#' Regressand Noise Detection for Regression
#'
#' Application of the regRND noise filtering method in a regression dataset.
#'
#' \emph{Regressand Noise Detection} (RND) is an adaptation of \emph{Class Noise Detection and Classification} (CNDC) found in the field of classification.
#' In a first step, CNDC builds an ensemble with SVM, \emph{Random Forest}, \emph{Naive Bayes}, k-NN and \emph{Neural Network}.
#' Then, a sample is marked as noisy using a voting scheme (indicated by the argument \code{vote}): if equal to \code{TRUE},
#' a consensus voting is used (in which a sample is marked as noisy if it is misclassified by all the models); if equal to \code{FALSE},
#' a majority voting is used (in which a sample is marked as noisy if it is misclassified by more than a half of the models).
#' Then, the decision to remove a sample is made by a distance filtering.
#' The implementation of this noise filter to be used in regression problems follows the proposal of Martín \emph{et al.} (2021),
#' which is based on the use of a noise threshold (\code{t}) to determine the similarity between the output variable of the samples.
#'
#' @param x a data frame of input attributes.
#' @param y a double vector with the output regressand of each sample.
#' @param t a double in [0,1] with the \emph{threshold} used by regression noise filter (default: 0.2).
#' @param nfolds an integer with the number of folds in which the dataset is split (default: 10).
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
#' Z. Nematzadeh, R. Ibrahim and A. Selamat,
#' \strong{Improving class noise detection and classification performance: A new two-filter CNDC model},
#' \emph{Applied Soft Computer}, 94:106428, 2020.
#' \doi{https://doi.org/10.1016/j.asoc.2020.106428}.
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
#' out.def <- regRND(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- regRND(formula = perm ~ ., data = rock[,])
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#' 
#' @seealso \code{\link{regENN}}, \code{\link{regAENN}}, \code{\link{regGE}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name regRND
NULL

###############################################################
###############################################################
###############################################################
#' @rdname regRND
#' @export
#' @importFrom "stats" "as.formula"
#' @importFrom "stats" "predict"
#' @importFrom "modelr" "crossv_kfold"
#' @importFrom "e1071" "svm"
#' @importFrom "FNN" "get.knn"
#' @importFrom "FNN" "knn.reg"
#' @importFrom "randomForest" "randomForest"
#' @importFrom "stats" "lm"
#' @importFrom "nnet" "nnet"
regRND.default <- function(x, y, t=0.2, nfolds=5, vote=FALSE, ...){

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

  folds <- as.list(crossv_kfold(dataset, nfolds))
  votes <- as.data.frame(matrix(NA, nrow = nrow(dataset), ncol = nfolds))
  models <- c("SVM", "NN", "LR", "RF", "NNET")

  for(i in 1:nfolds){
    for(kk in 1:length(models)){
      ml <- models[kk]
      votes[folds$test[[i]]$idx,kk] <- testing(alg = ml, train.fold = dataset[folds$train[[i]]$idx,],test.fold = dataset[folds$test[[i]]$idx,],output = output,t = t)
    }
  }

  vote <- ifelse(vote, 5, 3)
  idx.noisy <- which(rowSums(votes)>=vote)
  
  if(length(idx.noisy) >= 1){
    dataset[-idx.noisy,"tag"] <- "clean"
    dataset[idx.noisy,"tag"] <- "noise"
    
    real.clean <- dist.noisy(dataset, output)
    idx.noisy <- setdiff(idx.noisy, real.clean)
  }else{idx.noisy <- NULL}

  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- setdiff(1:nrow(original.data), idx.noisy)
  numclean <- length(idclean)
  xclean <- original.data[idclean, -ncol(original.data)]
  yclean <- original.data[idclean, ncol(original.data)]

  idnoise <- idx.noisy
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise, -ncol(original.data)]
  ynoise <- original.data[idnoise, ncol(original.data)]

  param <- list(t=t, nfolds=nfolds)
  call <- match.call()
  call[[1]] <- as.name("regRND")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Regressand Noise Detection",
              param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

# --------------------------------------------------------------------------------------- #
# ------------------------------- Complementary functions ------------------------------- #
# --------------------------------------------------------------------------------------- #
testing <- function(alg, train.fold, test.fold, output, t, returns){
  formu <- as.formula(paste(names(train.fold)[output],"~.",sep = ""))

  if(alg=="SVM"){
    svm.model <- svm(x = train.fold[,-output], y = train.fold[,output], type = "eps-regression", kernel = "radial")
    pred <- predict(svm.model, test.fold[,-output])
    out <- forecast(prediccion = pred,real = test.fold[,output],NS = t)
  }
  if(alg=="NN"){
    pred <- knn.reg(train = train.fold[,-output],test = test.fold[,-output],y = train.fold[,output],k = 10, algorithm=c("brute"))$pred
    out <- forecast(prediccion = pred,real = test.fold[,output],NS = t)
  }
  if(alg=="RF"){
    rf.model <- randomForest(train.fold[,-output], y=train.fold[,output],  xtest=NULL, ytest=NULL, ntree=500)
    pred <- predict(rf.model, test.fold[,-output])
    out <- forecast(prediccion = pred,real = test.fold[,output],NS = t)
  }
  if(alg=="LR"){

    rl.model <- lm(formula = formu, data = train.fold)
    if(length(which(is.na(rl.model$coefficients[2:length(rl.model$coefficients)])))!= 0){
      rl.model <- lm(formula = formu, data = train.fold[,-which(is.na(rl.model$coefficients[2:length(rl.model$coefficients)]))])
    }
    pred <- predict(rl.model, test.fold[,-output])
    out <- forecast(prediccion = pred, real = test.fold[,output], NS = t)
  }
  if(alg=="NNET"){
    nnet.model <- nnet(formu, train.fold, size=10, linout=TRUE, MaxNWts=ncol(train.fold)^3, maxit=30,trace=F)
    pred <- predict(nnet.model, test.fold[,-output], type="raw")
    out <- forecast(prediccion = pred, real = test.fold[,output],NS = t)
  }

  return(out)
}

###############################################################
###############################################################
###############################################################
dist.noisy <- function(df, output){
  
  tg = which(colnames(df)%in%"tag")
  nei <- get.knn(data = df[,-c(output, tg)], k = 10, algorithm = "brute")$nn.index
  nei.dist <- get.knn(data = df[,-c(output, tg)], k = 10, algorithm = "brute")$nn.dist
  
  ###
  indx <- NULL
  for(i in 1:nrow(nei)){
    DC1 <- mean(nei.dist[i,which(df[nei[i,],"tag"]%in%"clean")])
    DC2 <- mean(nei.dist[i,which(df[nei[i,],"tag"]%in%"noise")])
    
    if(is.na(DC1)){DC1 <- 0}
    if(is.na(DC2)){DC2 <- 0}
    
    if(DC1 < DC2 && df[i,"tag"] == "noise"){
      indx <- append(indx, i)
    }
  }
  return(indx)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname regRND
#' @importFrom "stats" "model.frame"
regRND.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- regRND.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("regRND")

  return(res)
}

###############################################################
###############################################################
###############################################################
