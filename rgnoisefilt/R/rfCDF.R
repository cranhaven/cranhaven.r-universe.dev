###############################################################
###############################################################
###############################################################
#' @export
rfCDF <- function(x, ...) UseMethod("rfCDF")

#' Covering Distance Filtering for Regression
#'
#' Application of the rfCDF noise filtering method in a regression dataset.
#'
#' \code{CDF} divides the dataset into two subsets, \emph{Din} and \emph{Dout}, which represent samples within and outside the covering interval, respectively. 
#' Samples in \emph{Din} are considered to have low noise and are retained in the final clean set of samples. 
#' Then, the noise of each sample is estimated using the \emph{Covering Distance} function. 
#' Samples in \emph{Dout} can be removed one by one based on their absolute noise, with samples exhibiting larger noise removed first. 
#' Each time a new sample is removed, an objective function can be estimated. 
#' Finally, the removing operation is stopped at the maximum value of the objective function.
#'
#' @param x a data frame of input attributes.
#' @param y a double vector with the output regressand of each sample.
#' @param subsets an integer with the number of subsets to be used (default: 5).
#' @param VCdim an integer specifying the VC-dimension (default: 0.1*\code{nrow(x)}).
#' @param prob a double with the probability used in the filtering process (default: 0.05).
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
#' G. Jiang, W. Wang, Y. Qian, J. Liang,
#' \strong{A Unified Sample Selection Framework for Output Noise Filtering: An Error-Bound Perspective.}
#' \emph{Journal of Machine Learning Research}, 22:1–65, 2021.
#' 
#' @examples
#' # load the dataset
#' data(rock)
#'
#' # usage of the default method
#' set.seed(9)
#' out.def <- rfCDF(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- rfCDF(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{regEF}}, \code{\link{regIPF}}, \code{\link{regGE}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name rfCDF
NULL

###############################################################
###############################################################
###############################################################
#' @rdname rfCDF
#' @export
#' @importFrom "FNN" "get.knn"
#' @import "modelr"
rfCDF.default <- function(x, y, subsets=5, VCdim=0.1*nrow(x), prob=0.05, ...){

  ######### check for errors #########
  if(!is.data.frame(x)){
    stop("argument \"x\" must be a data frame")
  }
  if(!is.numeric(y)){
    stop("argument \"y\" must be a factor vector")
  }
  if(nrow(x) != length(y)){
    stop("number of rows of \"x\" must be equal to length of \"y\"")
  }
  if(subsets < 1){
    stop("number of \"subsets\" must be greater than 1")
  }
  if(VCdim <= 0){
    stop("number of \"VCdim\" must be greater than 0")
  }
  if(any(prob < 0) || any(prob > 1)){
    stop("argument \"prob\" must be in [0,1]")
  }
  
  data <- cbind(x, y)
  output <- ncol(data)
  original.data <- data
  
  nfolds <- subsets
  h <- VCdim
  m <- prob
  
  # dataset partition
  part <- normalKFCV(data, nfolds)
  
  # get predictions
  pred <- matrix(data = -1, nrow = nrow(data), ncol = nfolds)
  posout <- ncol(data)
  for(f in 1:nfolds){
    train <- data[part$test[[f]],]
    pred[,f] <- FNN::knn.reg(train = train[,-posout], test = data[,-posout], y = train[,posout], k = 3, algorithm=c("brute"))$pred
  }
  
  # compute the covering interval for each sample
  minint <- apply(X = pred, MARGIN = 1, min)
  maxint <- apply(X = pred, MARGIN = 1, max)
  
  # compute the CD value for each sample and compute Dout set
  Ri <- rep(NA, nrow(data))
  inDout <- rep(FALSE, nrow(data))
  for(s in 1:nrow(data)){
    y <- data[s,posout]
    c <- (minint[s] + maxint[s])/2
    r <- (maxint[s] - minint[s])/2
    if(y >= minint[s] && y <= maxint[s]){
      Ri[s] <- (abs(y - c) + r)/2
    }
    else{
      inDout[s] <- TRUE
      Ri[s] <- abs(y - c)
    }
  }
  
  # sort the samples in Dout by the CD in ascending order
  inDout <- which(inDout)
  inDout_Ri <- Ri[inDout]
  
  idsort <- sort(inDout_Ri, decreasing = FALSE, index.return = TRUE)$ix
  inDout <- inDout[idsort]
  inDin <- setdiff(1:nrow(data), inDout)
  
  Dout <- data[inDout,] #Dout sorted
  Din <- data[-inDout,]
  
  # estimate the coefficient C
  y <- data[,posout]
  y <- matrix(data = rep(y, each=nfolds), nrow = nrow(data), ncol = nfolds, byrow = TRUE)
  error <- pred - y
  
  Cnum <- max(apply(X = error, MARGIN = 2, FUN = function(x) sum(x^2)/length(x)))
  Cden <- sum(Ri^2)/nrow(data)
  C <- Cnum/Cden
  
  # compute E(D)
  n <- nrow(data)
  sqrval <- max((h*(log(n/h)+1)-log(m))/n, 0)
  E <- max((1-sqrt(sqrval))^(-1), 0)
  
  # final iteration
  nC <- nrow(data) - nrow(Dout)
  n <- nrow(data)
  
  F_ps <- rep(NA, nrow(Dout))
  for(s in 1:nrow(Dout)){
    nF <- nC + s
    ps <- nF / n
    DF <- rbind(Din, Dout[1:s,])
    inDF <- c(inDin, inDout[1:s])
    
    # compute T
    Tnum <- sum(Ri[inDF]^2)/(n*ps)
    Tden <- sum(Ri^2)/n
    Tps <- Tnum/Tden
    
    # compute E_DF
    sqrval <- max((h*(log((n*ps)/h)+1)-log(m))/(n*ps), 0)
    E_DF <- max((1-sqrt(sqrval))^(-1), 0)
    
    # estimate the objective function
    F_ps[s] <- (C+1)*E - (C+Tps)*E_DF
  }
  
  sbest <- which.max(F_ps)
  Dfinal <- rbind(Din, Dout[1:sbest,])
  
  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- sort(as.numeric(rownames(Dfinal)))
  numclean <- length(idclean)
  xclean <- original.data[idclean,-ncol(original.data)]
  yclean <- original.data[idclean, ncol(original.data)]

  idnoise <- setdiff(1:nrow(original.data), idclean)
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise, -ncol(original.data)]
  ynoise <- original.data[idnoise, ncol(original.data)]

  param <- list(subsets=subsets, VCdim=VCdim, prob=prob)
  call <- match.call()
  call[[1]] <- as.name("rfCDF")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Covering Distance Filtering",
              param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname rfCDF
#' @importFrom "stats" "model.frame"
rfCDF.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- rfCDF.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("rfCDF")

  return(res)
}

###############################################################
###############################################################
###############################################################
