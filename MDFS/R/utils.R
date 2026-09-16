#' Get the recommended range for multiple discretisations
#'
#' @param k the assumed minimum number of objects in a bucket (the default is the recommended value)
#' @param n the total number of objects considered
#' @param dimensions the number of dimensions of analysis
#' @param divisions the number of divisions of discretisations
#' @return The recommended range value (a floating point number).
#' @examples
#' GetRange(n = 250, dimensions = 2)
#' @export
GetRange <- function(k = 3, n, dimensions, divisions = 1) {
  ksi <- (k / n) ^ (1 / dimensions)
  suggested.range <- (1 - ksi * (1 + divisions)) / (1 - ksi * (1 - divisions))
  range <- max(0, min(suggested.range, 1))
  reasonable.range <- 0.25
  if (range == 0) {
    warning("Too small sample for the test")
  } else if (range < reasonable.range) {
    warning("Too small sample for multiple discretizations")
  }
  range
}

#' Generate contrast variables from data
#'
#' @param data data organized in matrix with separate variables in columns
#' @param n.contrast number of constrast variables (defaults to max of 1/10 of variables number and 30)
#' @return A list with the following key names:
#'  \itemize{
#'   \item \code{indices} -- vector of indices of input variables used to construct contrast variables
#'   \item \code{x} -- data with constrast variables appended to it
#'   \item \code{mask} -- vector of booleans making it easy to select just contrast variables
#'  }
#' @examples
#' GenContrastVariables(madelon$data)
#' @export
GenContrastVariables <- function(
    data,
    n.contrast = max(ncol(data), 30)) {
  if (is.null(ncol(data))) {
    stop("Data has to have columns")
  }
  indices <- sample.int(ncol(data), n.contrast, replace = n.contrast > ncol(data))
  contrast_data <- apply(data[, indices], 2, sample)
  list(indices = indices, contrast_data = contrast_data)
}

#' Add contrast variables to data
#'
#' This function is deprecated. Please use GenContrastVariables instead.
#'
#' @param data data organized in matrix with separate variables in columns
#' @param n.contrast number of constrast variables (defaults to max of 1/10 of variables number and 30)
#' @return A list with the following key names:
#'  \itemize{
#'   \item \code{indices} -- vector of indices of input variables used to construct contrast variables
#'   \item \code{x} -- data with constrast variables appended to it
#'   \item \code{mask} -- vector of booleans making it easy to select just contrast variables
#'  }
#' @export
AddContrastVariables <- function(
    data,
    n.contrast = max(ncol(data) / 10, 30)) {
  .Deprecated("GenContrastVariables")
  if (is.null(ncol(data))) {
    stop("Data has to have columns")
  }
  indices <- sample.int(ncol(data), n.contrast, replace = n.contrast > ncol(data))
  x.contrast <- apply(data[, indices], 2, sample)
  mask <- c(rep.int(F, ncol(data)), rep.int(T, ncol(x.contrast)))
  return(list(
      indices = indices,
      x = cbind(data, x.contrast),
      mask = mask))
}

#' Discretize variable on demand
#'
#' @param data input data where columns are variables and rows are observations (all numeric)
#' @param variable.idx variable index (as it appears in \code{data})
#' @param divisions number of divisions
#' @param discretization.nr discretization number (positive integer)
#' @param seed seed for PRNG
#' @param range discretization range
#' @return Discretized variable.
#' @examples
#' Discretize(madelon$data, 3, 1, 1, 0, 0.5)
#' @export
#' @useDynLib MDFS r_discretize
Discretize <- function(
    data,
    variable.idx,
    divisions,
    discretization.nr,
    seed,
    range) {
  data <- data.matrix(data)
  storage.mode(data) <- "double"

  if (as.integer(divisions) != divisions || divisions < 1 || divisions > 15) {
    stop("Divisions has to be an integer between 1 and 15 (inclusive).")
  }

  if (as.integer(variable.idx) != variable.idx || variable.idx < 1) {
    stop("variable.idx has to be a positive integer.")
  }

  if (variable.idx > dim(data)[2]) {
    stop("variable.idx has to be in data bounds.")
  }

  if (as.integer(discretization.nr) != discretization.nr || discretization.nr < 1) {
    stop("discretization.nr has to be a positive integer.")
  }

  if (as.double(range) != range || range < 0 || range > 1) {
    stop("Range has to be a number between 0.0 and 1.0")
  }

  if (as.integer(seed) != seed || seed < 0 || seed > 2^31 - 1) {
    warning("Only integer seeds from 0 to 2^31-1 are portable. Using non-portable seed may make result harder to reproduce.")
  }

  variable <- data[, variable.idx]

  result <- .Call(
      r_discretize,
      variable,
      as.integer(variable.idx - 1), # convert to C 0-based
      as.integer(divisions),
      as.integer(discretization.nr - 1), # convert to C 0-based
      as.integer(seed),
      as.double(range))

  attr(result, "run.params") <- list(
    variable.idx      = variable.idx,
    divisions         = divisions,
    discretization.nr = discretization.nr,
    seed              = seed,
    range             = range)

  return(result)
}

#' Call omp_set_num_threads
#'
#' @param num_threads input data where columns are variables and rows are observations (all numeric)
#' @return No return value, called for side effects.
#' @export
#' @useDynLib MDFS r_omp_set_num_threads
mdfs_omp_set_num_threads <- function(
    num_threads) {
  .Call(
      r_omp_set_num_threads,
      num_threads)
}
