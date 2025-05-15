###############################################################
###############################################################
###############################################################
#' @export
discNCL <- function(x, ...) UseMethod("discNCL")

#' Neighborhood Cleaning Rule for Regression by Discretization
#'
#' Application of the discNCL noise filtering method in a regression dataset.
#'
#' \code{discNCL} discretizes the numerical output variable to make it compatible with \emph{Neighborhood Cleaning Rule} (NCL), typically used in classification tasks.
#' NCL identifies and prunes majority class instances that are predominantly surrounded by minority class counterparts, often perceived as noise or overlapping points. 
#' By removing these instances, decision boundaries become clearer, thereby enhancing classification performance. 
#' 
#' @param x a data frame of input attributes.
#' @param y a double vector with the output regressand of each sample.
#' @param k an integer with the number of nearest neighbors to be used (default: 3).
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
#' J. Laurikkala,
#' \strong{Improving identification of difficult small classes by balancing class distribution.}
#' \emph{Artificial Intelligence in Medicine}, 2101:63-66, 2001.
#' \doi{https://doi.org/10.1007/3-540-48229-6_9}.
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
#' out.def <- discNCL(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # show results
#' summary(out.def, showid = TRUE)
#'
#' # usage of the method for class formula
#' set.seed(9)
#' out.frm <- discNCL(formula = perm ~ ., data = rock)
#'
#' # check the match of noisy indices
#' all(out.def$idnoise == out.frm$idnoise)
#'
#' @seealso \code{\link{discCNN}}, \code{\link{discTL}}, \code{\link{discENN}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' @name discNCL
NULL

###############################################################
###############################################################
###############################################################
#' @rdname discNCL
#' @export
#' @importFrom "entropy" "entropy"
#' @importFrom "arules" "discretize"
#' @importFrom "class" "knn"
#' 
discNCL.default <- function(x, y, k=3, ...){

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
  formu <- as.formula(paste0(colnames(newdata)[ncol(newdata)], "~."))
  
  result_filter <- neighborhood_cleaning_rule(data = newdata, formula = formu, k = k)
  
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

  param <- list(k = k)
  call <- match.call()
  call[[1]] <- as.name("discNCL")

  ret <- list(xclean = xclean,
              yclean = yclean,
              numclean = numclean,
              idclean = idclean,
              xnoise = xnoise,
              ynoise = ynoise,
              numnoise = numnoise,
              idnoise = idnoise,
              filter = "Neighborhood Cleaning Rule by Discretization",
              param = param,
              call = call)
  class(ret) <- "rfdata"
  return(ret)
}

###############################################################
###############################################################
###############################################################
#' @export
#' @rdname discNCL
#' @importFrom "stats" "model.frame"
discNCL.formula <- function(formula, data, ...){

  if(!is.data.frame(data)){
    stop("argument \"data\" must be a data frame")
  }

  mf <- model.frame(formula,data)
  attr(mf,"terms") <- NULL

  x <- mf[,-1]
  y <- mf[,1]

  res <- discNCL.default(x = x, y = y, ...)
  res$call <- match.call(expand.dots = TRUE)
  res$call[[1]] <- as.name("discNCL")

  return(res)
}

###############################################################
###############################################################
###############################################################
neighborhood_cleaning_rule <- function(data, formula, k) {
  
  target_col <- which(names(data) == as.character(formula[[2]]))
  
  classes <- levels(data[, target_col])
  num_classes <- length(classes)
  
  Cl <- names(which.min(table(data[, target_col])))
  other_classes <- setdiff(classes, Cl)
  
  if (length(other_classes) > 1) {
    cleaned_data <- perform_kknn(data[which(data[, target_col] %in% other_classes), ], formula, k)
  } else {
    cleaned_data <- list(data[which(data[, target_col] %in% other_classes), ], c())
  }
  
  relevant_data <- subset(data, data[, target_col] %in% Cl)
  
  neighbors <- knn(train = data, test = data, cl = data[, target_col], k = k)
  
  min_class_value <- min(sapply(Cl, function(x) length(which(data[, target_col] == x))))
  potential_min_value <- 0.5 * min_class_value
  classes_to_remove <- names(which(sapply(other_classes, function(x) length(which(data[, target_col] == x)) > potential_min_value)))
  
  removal_indices <- c()
  for (current_class in Cl) {
    class_indices <- which(data[, target_col] == current_class)
    for (i in class_indices) {
      if (sum(neighbors == current_class) == k) {
        removal_indices <- c(removal_indices, which(neighbors == current_class))
      }
    }
  }
  
  removal_indices <- unique(removal_indices)
  
  return(removal_indices)
}

