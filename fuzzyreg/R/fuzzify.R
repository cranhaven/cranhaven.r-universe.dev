#' Convert Real Value Numbers to Triangular Fuzzy Numbers
#' 
#' Uses naive alternative methods to approximate triangular fuzzy
#' numbers from real value number input data.
#' @param x numeric vector.
#' @param y vector that can be coerced to factor (optional).
#' @param method character vector specifying the conversion method. See Details.
#' @param err numeric vector. Error term for the error method.
#' @param dimnames \code{list} of length 2 giving names of the \code{x} and \code{y} 
#'   variables.
#' @param ... additional parameters passed to other functions.
#' @details Converts crisp numbers in \code{x} to a triangular fuzzy number (TFN). Optionally, 
#'  values in \code{y} can be used as grouping elements and are coerced to a factor.
#'  
#'  Method \code{mean} calculates the central value of a TFN as the mean of \code{x} given
#'  \code{y}, and the left and right spreads as standard deviations. 
#'  
#'  Method \code{median} gives the central values as a median and left and right spreads 
#'  are calculated as distance of the first and third quartile from the median.
#'  
#'  Method \code{zero} inserts zeros to both spreads.
#'  
#'  Method \code{error} uses a user-defined numeric value or vector for the spreads. 
#'  The length of the numeric vector in argument \code{err} must be in 
#'  (1, \code{length(x)}, 2 * \code{length(x)}).
#' @export
#' @return A data.frame with columns representing the central value, left and right spread of
#'  \code{x} and the values in \code{y} coerrced to a factor. Attempt is made to inherit 
#'  names from the input data. Methods 
#'  \code{mean} and \code{zero} will return symmetric TFNs, whereas methods \code{median} and 
#'  \code{error} can return non-symmetric TFNs depending on input data and the data or the
#'  values in the \code{err} argument.
#' @examples 
#' fuzzify(1:5)
#' fuzzify(1:6, c(1,1,1,2,2,2), method = "err", err = runif(6) * 1e-3)


fuzzify = function(x, y = NULL, method = "mean", err = 0, dimnames = NULL, ...){
  # check input
  metody = c("mean", "median", "zero", "error")
  m = pmatch(method, metody)
  if(is.na(m)) stop(sprintf("%s was not uniquely matched to available methods %s.", method, 
                            paste(metody, collapse = ", ")))
  method = metody[m]
  
  x = as.matrix(x)
  
  if(is.null(y)){
    y = rep(1, nrow(x))
  }
  
  
  if(nrow(x) != length(y)) stop("x and y must have the same length")
  
  # conversions
  if(method == "mean"){
    res = stats::aggregate(x, by = list(y), FUN = mean, ...)
    res = cbind(res, stats::aggregate(x, by = list(y), FUN = stats::sd, ...)[, -1])
    res = res[, c(2:(ncol(x) + 1), rep((ncol(x) + 2):ncol(res), 2), 1)]
  }
  
  if(method == "median"){
    res = stats::aggregate(x, by = list(y), FUN = stats::median, ...)
    res = cbind(res, res[, 2:(ncol(x) + 1)] - stats::aggregate(x, by = list(y), FUN = stats::quantile, probs = 0.25, ...)[, -1])
    res = cbind(res, stats::aggregate(x, by = list(y), FUN = stats::quantile, probs = 0.75, ...)[, -1] - res[, 2:(ncol(x) + 1)])
    res = res[, c(2:ncol(res),1)]
  }
  
  if(method == "zero"){
    res = cbind(x, matrix(rep(0, 2 * prod(dim(x))), nrow = nrow(x)), y)
  }
  
  if(method == "error"){
    if(length(err) == 1){
      err = rep(err, 2 * prod(dim(x)))
    }
    if(length(err) == nrow(x)){
      err = rep(err, 2 * ncol(x))
    }
    if(all(length(err) != prod(dim(x)), length(err) != 2 * prod(dim(x)))){
      stop("spreads included in the err argument length not a multiple of length of predictors")
    }
    res = data.frame(matrix(c(x, rep(err, ifelse(length(err) == prod(dim(x)), 2, 1))), 
                            ncol = 3 * ncol(x), 
                            byrow = FALSE))
    res$y = y
  }
  
  # find names
  if(is.null(dimnames)) dimnames = list(LETTERS[1:ncol(x)], "y")
  if(any(length(dimnames[[1]]) != ncol(x), length(dimnames[[2]]) != 1)){
    stop("Number of variable names in argument dimnames does not correspond to dimensions in x and y")
  }
  
  name.x = switch(1 + is.null(colnames(x)), colnames(x), dimnames[[1]])
  name.y = switch(1 + is.null(dimnames[[2]]), dimnames[[2]][1], "y")
  colnames(res) = c(paste0(name.x, "c"),
                    paste0(name.x, "l"),
                    paste0(name.x, "r"),
                    name.y)
  res
}
