#'Combine weighted indices of n-dimensional arrays
#'
#'@description Function to combine climate indices for multiple models through 
#'addition, subtraction, division or averaging, optionally applying weights to 
#'each index.
#'
#'@param indices List of n-dimensional arrays with equal dimensions to be 
#'  combined.
#'@param weights Vector of weights for the indices, whose length is the same as 
#'  the list of parameter \code{indices}. If not provided, a weight of 1 is 
#'  assigned to each index. If \code{operation = 'mean'} the weights are 
#'  normalized to sum 1 all together.
#'@param operation The operation for combining the indices, either \code{"mean"} 
#'  (default), \code{"add"}, \code{"subtract"} or \code{"divide"}.
#'
#'@return An array of the same dimensions as one of the elements in the 
#'parameter \code{indices}.
#'
#'@examples
#'a <- matrix(rnorm(6), 2, 3)
#'b <- matrix(rnorm(6), 2, 3)
#'
#'comb_ind <- CombineIndices(indices = list(a, b), weights = c(2, 1), 
#'                           operation = "add")
#'print(comb_ind)
#'
#'a <- rnorm(24)
#'dim(a) <- c(lon = 2, lat = 3, mod = 4)
#'b <- rnorm(24)
#'dim(b) <- c(lon = 2, lat = 3, mod = 4)
#'comb_ind <- CombineIndices(indices = list(a, b), weights = c(2, 1), 
#'                           operation = "add")
#'print(comb_ind)
#'@export
CombineIndices <- function(indices, weights = NULL, operation = "mean") {
  if (!is.null(indices)) {
    if (!is.list(indices)) {
      indices <- list(indices)
    }
  } else {
    stop("Parameter 'indices' cannot be NULL.")
  }
  for (i in 1 : length(indices)) {
    if (is.null(indices[[i]])) {
      stop("Elements of parameter 'indices' cannot be NULL.")
    }
  }
  if (length(indices) > 1) {
    for (i in 1 : (length(indices) - 1)) {
      if (length(dim(indices[[i]])) == length(dim(indices[[i + 1]]))) {
        if ( sum(dim(indices[[i]]) == dim(indices[[i + 1]])) != length(dim(indices[[i]]))) {
          stop("Parameter 'indices' must contain a list of objects with the same dimensions.")
        }
      } else {
        stop("Parameter 'indices' must contain a list of objects with the same length of dimensions.")
      }
    }
  }  
  if (!is.null(weights) && length(weights) != length(indices)) {
    stop("The vector of weights must be the same length as the number of index objects.")
  }
  if (!is.character(operation)) {
    stop("Parameter 'operation' must be a character string.")
  }
  if (!(operation %in% c("mean", "add", "subtract", "divide"))) {
    stop("Wrong operation. It must be either 'mean', 'add', 'subtract' or 'divide'.")
  }
  if (is.null(weights)) {
    if (operation == "mean") {
      weights <- rep(1 / length(indices), length(indices))
    } else {
      weights <- rep(1, length(indices))
    }
  } else if (operation == "mean") {
    weights <- weights * rep(1 / sum(weights), length(indices))
  }
  comb_ind <- NULL
  if (length(indices) > 0) {
    comb_ind <- indices[[1]] * weights[1]
  }
  if (length(indices) > 1) {
    if (operation == "mean" || operation == "add") {
      for (i in 1 : (length(indices) - 1)) {
        comb_ind <- comb_ind + indices[[i + 1]] * weights[i + 1]
      }
    } else if (operation == "subtract") {
      for (i in 1 : (length(indices) - 1)) {
        comb_ind <- comb_ind - indices[[i + 1]] * weights[i + 1] 
      }
    } else {
      if (length(indices) > 2) {
        stop("A maximum of 2 indices is supported for operation 'divide'.")
      }
      comb_ind <- comb_ind / (indices[[2]] * weights[2])
    }
  }
  comb_ind
}