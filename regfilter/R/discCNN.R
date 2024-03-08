###############################################################
###############################################################
###############################################################
#' @export
discCNN <- function(x, ...) UseMethod("discCNN")

#' Condensed Nearest Neighbors for Regression by Discretization
#'
#' Application of the discCNN noise filtering method in a regression dataset.
#'
#' \code{discCNN} discretizes the numerical output variable to make it compatible with \emph{Condensed Nearest Neighbors} (CNN), typically used in classification tasks. 
#' CNN performs a first classification and stores all the samples that are misclassified.
#' Then, those stored samples are taken as a training set. The process stops when all the unstored samples are correctly classified.
#' 
#' @param x a data frame of input attributes.
#' @param y a double vector with the output regressand of each sample.
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
#' A. Arnaiz-González, J. Díez-Pastor, J. Rodríguez, C. García-Osorio,
#' \strong{Instance selection for regression by discretization.}
#' \emph{Expert Systems with Applications}, 54:340-350, 2016.
#' \doi{https://doi.org/10.1016/j.eswa.2015.12.046}.
#' 
#' @examples
#' # load the dataset
#' data(rock)
#'
#' # usage of the default method
#' set.seed(9)
#' out.def <- discCNN(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- discCNN(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{discENN}}, \code{\link{discTL}}, \code{\link{discNCL}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name discCNN
NULL

###############################################################
###############################################################
###############################################################
#' @rdname discCNN
#' @export
#' @importFrom "UBL" "CNNClassif"
#' @importFrom "entropy" "entropy"
#' @importFrom "arules" "discretize"
discCNN.default <- function(x, y, ...){

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

  dataset <- cbind(x, y)
  output <- ncol(dataset)
  original.data <- dataset
  
  # Get group number
  entpy <- rep(0, 10)
  for(e in 1:6){
    y1  <-  table(arules::discretize(x = as.matrix(dataset[,ncol(dataset)]), method = "interval", breaks = e))
    entpy[e] <- entropy::entropy(y1, method=c("minimax")) 
    if(entpy[e] >= max(entpy)){B  <-  e}
  }
  
  disc <- arules::discretize(x = as.matrix(dataset[,ncol(dataset)]), method = "interval", breaks = B)
  newdata <- data.frame(x, target = factor(disc))
  formu <- as.formula(paste0(colnames(newdata)[ncol(newdata)], "~."))
  
  result_filter <- UBL::CNNClassif(form = formu, dat = newdata)
  result_filter <- result_filter[[1]]
  
  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- sort(as.numeric(rownames(result_filter)))
  numclean <- length(idclean)
  xclean <- original.data[idclean,-ncol(original.data)]
  yclean <- original.data[idclean,ncol(original.data)]

  idnoise <- setdiff(1:nrow(original.data), idclean) 
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise,-ncol(original.data)]
  ynoise <- original.data[idnoise,ncol(original.data)]

  # param <- list(t = t)
  call <- match.call()
  call[[1]] <- as.name("discCNN")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Condensed Nearest Neighbors by Discretization",
              # param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname discCNN
#' @importFrom "stats" "model.frame"
discCNN.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- discCNN.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("discCNN")

  return(res)
}

###############################################################
###############################################################
###############################################################
