###############################################################
###############################################################
###############################################################
#' @export
discTL <- function(x, ...) UseMethod("discTL")

#' Tomek Links for Regression by Discretization
#'
#' Application of the discTL noise filtering method in a regression dataset.
#'
#' \code{discTL} discretizes the numerical output variable to make it compatible with \emph{Tomek Links} (TL), typically used in classification tasks. 
#' TL identifies pairs of instances that are close neighbors but belong to different classes. 
#' If an instance in such a pair is predominantly surrounded by instances from a different class, it may be flagged as noisy.
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
#' I. Tomek,
#' \strong{Two modifications of CNN.}
#' \emph{IEEE Trans. Syst. Man Cybern}, 6:769-772, 1976.
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
#' out.def <- discTL(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- discTL(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{discENN}}, \code{\link{discCNN}}, \code{\link{discNCL}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name discTL
NULL

###############################################################
###############################################################
###############################################################
#' @rdname discTL
#' @export
#' @importFrom "entropy" "entropy"
#' @importFrom "arules" "discretize"
#' 
discTL.default <- function(x, y, ...){

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
  sorted_levels <- sort(levels(newdata$target))
  newdata$target <- factor(newdata$target, levels = sorted_levels, labels = 1:length(sorted_levels))
  
  # Fix data for Tome link  
  attr_data <- newdata[, -ncol(newdata)]
  attr_data_matrix <- as.matrix(attr_data)
  class1_indices <- which(newdata[, ncol(newdata)] == levels(newdata[, ncol(newdata)])[1])
  class2_indices <- which(newdata[, ncol(newdata)] == levels(newdata[, ncol(newdata)])[2])
  
  result_filter <- compute_tomek_matrix(attr_data_matrix = attr_data_matrix, class1_indices = class1_indices, class2_indices = class2_indices)
  result_filter <- c(class1_indices[colSums(result_filter)>0],class2_indices[rowSums(result_filter)>0])
  
  # ------------------------------------ #
  # --- Building the 'filter' object --- #
  # ------------------------------------ #
  idclean <- setdiff(1:nrow(original.data), result_filter)
  numclean <- length(idclean)
  xclean <- original.data[idclean,-ncol(original.data)]
  yclean <- original.data[idclean,ncol(original.data)]

  idnoise <- setdiff(1:nrow(original.data), idclean) 
  numnoise <- length(idnoise)
  xnoise <- original.data[idnoise,-ncol(original.data)]
  ynoise <- original.data[idnoise,ncol(original.data)]

  # param <- list(t = t)
  call <- match.call()
  call[[1]] <- as.name("discTL")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Tomek Links by Discretization",
              # param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname discTL
#' @importFrom "stats" "model.frame"
discTL.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- discTL.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("discTL")

  return(res)
}

###############################################################
###############################################################
###############################################################
compute_tomek_matrix <- function(attr_data_matrix, class1_indices, class2_indices) {
  
  tomek_links_matrix <- sapply(class1_indices, function(i) {
    sapply(class2_indices, function(j) {
      
      midpoint <- (attr_data_matrix[i, ] + attr_data_matrix[j, ]) / 2
      
      distances_to_midpoint_class1 <- apply(attr_data_matrix[setdiff(class1_indices, i), ], 1, function(v) {
        sum(abs(v - midpoint))
      })
      
      if (any(distances_to_midpoint_class1 <= sum(abs(attr_data_matrix[i, ] - midpoint)))) {
        return(FALSE)
      }
      
      distances_to_midpoint_class2 <- apply(attr_data_matrix[setdiff(class2_indices, j), ], 1, function(v) {
        sum(abs(v - midpoint))
      })
      
      if (any(distances_to_midpoint_class2 <= sum(abs(attr_data_matrix[j, ] - midpoint)))) {
        return(FALSE)
      }
      return(TRUE)
    })
  })
  
  return(tomek_links_matrix)
}

