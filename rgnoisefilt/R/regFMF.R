###############################################################
###############################################################
###############################################################
#' @export
regFMF <- function(x, ...) UseMethod("regFMF")

#' Fusion of Multiple Filters for Regression
#'
#' Application of the regFMF noise filtering method in a regression dataset.
#'
#' \emph{Fusion of Multiple Filters for Regression} (regFMF) is an adaptation of \emph{Ensembles of label Noise Filters} (ENF) found in the field of classification,
#' which creates an ensemble with the AENN, DF and HARF filtering techniques.
#' Then, each filter generates one vote per sample. A sample is considered as noisy using a voting scheme (indicated by the argument \code{vote}):
#' if equal to \code{TRUE}, a consensus voting is used (in which a sample is removed if it is misclassified by all the models);
#' if equal to \code{FALSE}, a majority voting is used (in which a sample is removed if it is misclassified by more than a half of the models).
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
#' L. Garcia, A. Lorena, S. Matwin and A. de Carvalho,
#' \strong{Ensembles of label noise filters: a ranking approach},
#' \emph{Data Mining Knowledge Discovery}, 30:1192–1216, 2016.
#' \doi{https://doi.org/10.1007/s10618-016-0475-9}.
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
#' out.def <- regFMF(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- regFMF(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{regDF}}, \code{\link{regHRRF}}, \code{\link{regAENN}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name regFMF
NULL

###############################################################
###############################################################
###############################################################
#' @rdname regFMF
#' @export
regFMF.default <- function(x, y, t=0.2, vote=FALSE, ...){

  if(!is.data.frame(x) | !is.vector(y)){
    stop("data argument must be a data.frame")
  }
  if(!all(unique(sapply(x, class))%in%c("integer", "numeric"))){
    stop('x: all attributes must be numerical')
  }
  if(!is.numeric(y)){
    stop('y: it must be numerical')
  }
  if(any(t < 0) || any(t > 1)){
    stop("argument \"threshold\" must be in [0,1]")
  }

  dataset <- cbind(x, y)
  output <- ncol(dataset)
  original.data <- dataset
  dataset <- normalizeData2(dataset)

  if(vote){
    majThreshold <- 3
  }else{
    majThreshold <- 2}

  votes <- as.data.frame(matrix(FALSE, nrow = nrow(dataset), ncol = 3))

  pred.aenn <- regAENN(x = x, y = y, t = t)
  votes[pred.aenn$idnoise,1] <- TRUE

  pred.hrrf <- regHRRF(x = x, y = y, t = t)
  votes[pred.hrrf$idnoise,2] <- TRUE

  pred.df <- regDF(x = x, y = y, t = t)
  votes[pred.df$idnoise,3] <- TRUE

  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- setdiff(1:nrow(original.data), which(rowSums(votes)>=majThreshold))
  numclean <- length(idclean)
  xclean <- original.data[idclean, -ncol(original.data)]
  yclean <- original.data[idclean, ncol(original.data)]

  idnoise <- which(rowSums(votes)>=majThreshold)
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise, -ncol(original.data)]
  ynoise <- original.data[idnoise, ncol(original.data)]

  param <- list(t=t, vote=vote)
  call <- match.call()
  call[[1]] <- as.name("regFMF")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Fusion Multiple Filter",
              param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname regFMF
#' @importFrom "stats" "model.frame"
regFMF.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- regFMF.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("regFMF")

  return(res)
}

###############################################################
###############################################################
###############################################################
