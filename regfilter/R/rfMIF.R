###############################################################
###############################################################
###############################################################
#' @export
rfMIF <- function(x, ...) UseMethod("rfMIF")

#' Mutual Information-based Filter for Regression
#'
#' Application of the rfMIF noise filtering method in a regression dataset.
#'
#' The \code{rfMIF} filter harnesses mutual information to enhance the prototypes within the training set. 
#' First, it identifies the \emph{k}-nearest neighbors for each data point. 
#' Subsequently, mutual information values are calculated and standardized between 0 and 1. 
#' \code{rfMIF} then compares the mutual information of each data point to its \emph{k}-nearest neighbors. 
#' If the discrepancy surpasses a threshold (\code{alpha}), the sample is considered noisy.
#' 
#' @param x a data frame of input attributes.
#' @param y a double vector with the output regressand of each sample.
#' @param alpha a double in [0,1] with the \emph{threshold} used by rfMIF (default: 0.05).
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
#' A. Guillen, L. Herrera, G. Rubio, H. Pomares, A. Lendasse, I. Rojas, 
#' \strong{New method for instance or prototype selection using mutual information in time series prediction.},
#' \emph{Neurocomputing}, 73:2030-2038, 2010.
#' \doi{https://doi.org/10.1016/j.neucom.2009.11.031}.
#'
#' M. Stojanović, M. Božić, M. Stanković, Z. Stajić,
#' \strong{A methodology for training set instance selection using mutual information in time series prediction.}
#' \emph{Neurocomputing}, 141:236-245, 2014.
#' \doi{https://doi.org/10.1016/j.neucom.2014.03.006}.
#'
#' @examples
#' # load the dataset
#' data(rock)
#'
#' # usage of the default method
#' set.seed(9)
#' out.def <- rfMIF(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- rfMIF(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{regENN}}, \code{\link{regAENN}}, \code{\link{regCNN}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name rfMIF
NULL

###############################################################
###############################################################
###############################################################
#' @rdname rfMIF
#' @export
#' @importFrom "FNN" "get.knn"
#' @importFrom "arules" "discretizeDF"
#' @importFrom "infotheo" "mutinformation"
rfMIF.default <- function(x, y, k=5, alpha=0.05, ...){

  ######### check for errors #########
  if(!is.data.frame(x)){
    stop("argument \"x\" must be a data frame")
  }
  if(!is.numeric(y)){
    stop("argument \"y\" must be a factor vector")
  }
  if(any(alpha < 0) || any(alpha > 1)){
    stop("argument \"alpha\" must be in [0,1]")
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
  
  nn_indices <- get.knn(data = dataset[,-output], k = k, algorithm = "brute")$nn.index
  
  data <- arules::discretizeDF(dataset, methods = NULL, default = NULL)
  mi.data <- as.data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)-1))
  for (i in 1:nrow(data)) {
    mi_dat <- mutinformation(data[-i,], method="emp")
    mi.data[i,] <- mi_dat[nrow(mi_dat),-ncol(mi_dat)]
  }
  
  mi.data <- rowSums(mi.data)
  mi.data <- (mi.data - min(mi.data)) / (max(mi.data) - min(mi.data))
  
  toInclude <- c()
  for (i in 1:nrow(data)) {
    Cdiff <- 0
    for(j in 1:k){
      mi.nei <- mi.data[nn_indices[i, j]]
      diff <- mi.data[i] - mi.nei
      if (diff > alpha){
        Cdiff <- Cdiff + 1
      }
    }
    if(Cdiff < k){
      toInclude <- c(toInclude, i)
    }
  }
  
  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- sort(toInclude)
  numclean <- length(idclean)
  xclean <- original.data[idclean,-ncol(original.data)]
  yclean <- original.data[idclean, ncol(original.data)]
  
  idnoise <- setdiff(1:nrow(original.data), idclean) 
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise, -ncol(original.data)]
  ynoise <- original.data[idnoise, ncol(original.data)]
  
  param <- list(k=k, alpha=alpha)
  call <- match.call()
  call[[1]] <- as.name("rfMIF")
  
  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Mutual Information-based",
              param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname rfMIF
#' @importFrom "stats" "model.frame"
rfMIF.formula <- function(formula, data, ...){
  
  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }
  
  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL
  
  x <- mf[,-1]
  y <- mf[,1]
  
  res <- rfMIF.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("rfMIF")
  
  return(res)
}

###############################################################
###############################################################
###############################################################
