###############################################################
###############################################################
###############################################################
#' @export
rfDROP3 <- function(x, ...) UseMethod("rfDROP3")

#' Decremental Reduction Optimization Procedure 3 for Regression
#'
#' Application of the rfDROP3 noise filtering method in a regression dataset.
#' 
#' \code{rfDROP3} works on the basis of \code{\link{rfDROP2}}, which removes an instance \code{p} only if its exclusion does not increase the prediction error of its associates.
#' This is measured by comparing the accumulation of errors with and without \code{p} in the dataset. 
#' \code{rfDROP3} integrates a initial noise filtering with \code{\link{regENN}}, and then sorts instances based on distance to the nearest enemy.
#'
#' @param x a data frame of input attributes.
#' @param y a double vector with the output regressand of each sample.
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
#' A. Arnaiz-González, J. Díez-Pastor, J. Rodríguez, C. García-Osorio,
#' \strong{Instance selection for regression: Adapting DROP.},
#' \emph{Neurocomputing}, 201:66-81, 2016.
#' \doi{https://doi.org/10.1016/j.neucom.2016.04.003}.
#'
#' D. Randall, T. Martinez,
#' \strong{Instance pruning techniques.}
#' \emph{Machine Learning: Proceedings of the Fourteenth International Conference}, 404–411, 1997.
#' 
#' @examples
#' # load the dataset
#' data(rock)
#'
#' # usage of the default method
#' set.seed(9)
#' out.def <- rfDROP3(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- rfDROP3(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{rfDROP2}}, \code{\link{regENN}}, \code{\link{regRNN}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name rfDROP3
NULL

###############################################################
###############################################################
###############################################################
#' @rdname rfDROP3
#' @export
#' @importFrom "FNN" "knn.reg"
rfDROP3.default <- function(x, y, k=5, ...){

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
  if(k < 1){
    stop("number of \"Nearest-Neighbor\" must be greater than 1")
  }
  
  original.data <- cbind(x, y)
  
  filtered <- regENN(x = x, y = y)
  S <- cbind(filtered$xclean, y = filtered$yclean)
  S <- sort_DROP2RT(data = S, D = 0.5)
  
  # Find the k-nearest neighbors
  associates_indices <- get.knn(data = S, k = k + 1, algorithm = "brute")$nn.index
  associates_indices <- cbind(associates_indices, as.numeric(rownames(S)))
  
  # Loop over each instance in S
  toRemove <- c()
  for(x in 1:nrow(S)){
    # Errors with and without x
    eWith <- 0
    eWithout <- 0
    
    # Calculate errors for each associate
    for(j in associates_indices[x,]){
      associates <- associates_indices[j,]
      
      a <- S[j,-ncol(S)]
      y_a <- S[j,ncol(S)]
      
      train.eWith <- S[c(setdiff(associates, x), x), -ncol(S)]
      y.eWith <- S[c(setdiff(associates, x), x), ncol(S)]
      pred.eWith <- abs(knn.reg(train = train.eWith, test = a, y = y.eWith, k = k)$pred)
      eWith <- abs(y_a - pred.eWith) + eWith
      
      train.eWithout <- S[setdiff(associates, x), -ncol(S)]
      y.eWithout <- S[setdiff(associates, x), ncol(S)]
      pred.eWithout <- knn.reg(train = train.eWithout, test = a, y = y.eWithout, k = k)$pred
      eWithout <- abs(y_a - pred.eWithout) + eWithout
    }
    if(eWithout <= eWith){
      toRemove <- c(toRemove, x)
    }
  }
  
  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- setdiff(1:nrow(original.data), sort(toRemove))
  numclean <- length(idclean)
  xclean <- original.data[idclean,-ncol(original.data)]
  yclean <- original.data[idclean,ncol(original.data)]
  
  idnoise <- sort(toRemove)
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise,-ncol(original.data)]
  ynoise <- original.data[idnoise,ncol(original.data)]
  
  param <- list(k = k)
  call <- match.call()
  call[[1]] <- as.name("rfDROP3")
  
  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Decremental Reduction Optimization Procedure 3",
              param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname rfDROP3
#' @importFrom "stats" "model.frame"
rfDROP3.formula <- function(formula, data, ...){
  
  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }
  
  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL
  
  x <- mf[,-1]
  y <- mf[,1]
  
  res <- rfDROP3.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("rfDROP3")
  
  return(res)
}

###############################################################
###############################################################
###############################################################
