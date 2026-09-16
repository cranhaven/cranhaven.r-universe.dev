#' Max information gains
#'
#' @param data input data where columns are variables and rows are observations (all numeric)
#' @param decision decision variable as a binary sequence of length equal to number of observations
#' @param contrast_data the contrast counterpart of data, has to have the same number of observations - not supported with CUDA
#' @param dimensions number of dimensions (a positive integer; 5 max)
#' @param divisions number of divisions (from 1 to 15; additionally limited by dimensions if using CUDA)
#' @param discretizations number of discretizations
#' @param seed seed for PRNG used during discretizations (\code{NULL} for random)
#' @param range discretization range (from 0.0 to 1.0; \code{NULL} selects probable optimal number)
#' @param pc.xi parameter xi used to compute pseudocounts (the default is recommended not to be changed)
#' @param return.tuples whether to return tuples (and relevant discretization number) where max IG was observed (one tuple and relevant discretization number per variable) - not supported with CUDA nor in 1D
#' @param interesting.vars variables for which to check the IGs (none = all) - not supported with CUDA
#' @param require.all.vars boolean whether to require tuple to consist of only interesting.vars
#' @param use.CUDA whether to use CUDA acceleration (must be compiled with CUDA)
#' @return A \code{\link{data.frame}} with the following columns:
#'  \itemize{
#'    \item \code{IG} -- max information gain (of each variable)
#'    \item \code{Tuple.1, Tuple.2, ...} -- corresponding tuple (up to \code{dimensions} columns, available only when \code{return.tuples == T})
#'    \item \code{Discretization.nr} -- corresponding discretization number (available only when \code{return.tuples == T})
#'  }
#'
#'  Additionally attribute named \code{run.params} with run parameters is set on the result.
#' @examples
#' \donttest{
#' ComputeMaxInfoGains(madelon$data, madelon$decision, dimensions = 2, divisions = 1,
#'                     range = 0, seed = 0)
#' }
#' @importFrom stats runif
#' @export
#' @useDynLib MDFS r_compute_max_ig
ComputeMaxInfoGains <- function(
    data,
    decision,
    contrast_data = NULL,
    dimensions = 1,
    divisions = 1,
    discretizations = 1,
    seed = NULL,
    range = NULL,
    pc.xi = 0.25,
    return.tuples = FALSE,
    interesting.vars = vector(mode = "integer"),
    require.all.vars = FALSE,
    use.CUDA = FALSE) {
  data <- data.matrix(data)
  storage.mode(data) <- "double"
  if (!is.null(contrast_data)) {
    contrast_data <- data.matrix(contrast_data)
    storage.mode(contrast_data) <- "double"
    if (nrow(contrast_data) != nrow(data)) {
      stop("Count of contrast data observations differs from count of data observations.")
    }
  }

  decision <- prepare_decision(decision)

  if (length(decision) != nrow(data)) {
    stop("Length of decision is not equal to the number of rows in data.")
  }

  dimensions <- prepare_integer_in_bounds(dimensions, "Dimensions", as.integer(1), as.integer(5))

  divisions <- prepare_integer_in_bounds(divisions, "Divisions", as.integer(1), as.integer(15))

  discretizations <- prepare_integer_in_bounds(discretizations, "Discretizations", as.integer(1))

  pc.xi <- prepare_double_in_bounds(pc.xi, "pc.xi", .Machine$double.xmin)

  if (is.null(range)) {
    range <- GetRange(n = nrow(data), dimensions = dimensions, divisions = divisions)
  }

  range <- prepare_double_in_bounds(range, "Range", 0.0, 1.0)

  if (range == 0 && discretizations > 1) {
    stop("Zero range does not make sense with more than one discretization. All will always be equal.")
  }

  if (is.null(seed)) {
    seed <- round(runif(1, 0, 2^31 - 1)) # unsigned passed as signed, the highest bit remains unused for best compatibility
  }

  seed <- prepare_integer_in_bounds(seed, "Seed", as.integer(0))

  if (dimensions == 1 && return.tuples) {
    stop("return.tuples does not make sense in 1D")
  }

  if (use.CUDA) {
    if (dimensions == 1) {
      stop("CUDA acceleration does not support 1 dimension")
    }

    if ((divisions + 1)^dimensions > 256) {
      stop("CUDA acceleration does not support more than 256 cubes = (divisions+1)^dimensions")
    }

    if (return.tuples) {
      stop("CUDA acceleration does not support return.tuples parameter (for now)")
    }

    if (length(interesting.vars) > 0) {
      stop("CUDA acceleration does not support interesting.vars parameter (for now)")
    }

    if (!is.null(contrast_data)) {
      stop("CUDA acceleration does not support contrast_data parameter (for now)")
    }
  }

  out <- .Call(
      r_compute_max_ig,
      data,
      contrast_data,
      decision,
      dimensions,
      divisions,
      discretizations,
      seed,
      range,
      pc.xi,
      as.integer(interesting.vars[order(interesting.vars)] - 1),  # send C-compatible 0-based indices
      as.logical(require.all.vars),
      as.logical(return.tuples),
      as.logical(use.CUDA))

  if (return.tuples) {
    result <- out[1:3]
    names(result) <- c("IG", "Tuple", "Discretization.nr")
    result$Tuple <- t(result$Tuple + 1) # restore R-compatible 1-based indices, transpose to remain compatible with ComputeInterestingTuples
    result$Discretization.nr <- result$Discretization.nr + 1 # restore R-compatible 1-based indices
  } else {
    result <- out[1]
    names(result) <- c("IG")
  }

  result <- as.data.frame(result)

  attr(result, "run.params") <- list(
    dimensions      = dimensions,
    divisions       = divisions,
    discretizations = discretizations,
    seed            = seed,
    range           = range,
    pc.xi           = pc.xi)

  if (!is.null(contrast_data)) {
    if (return.tuples) {
      attr(result, "contrast_igs") <- out[[4]]
    } else {
      attr(result, "contrast_igs") <- out[[2]]
    }
  }

  return(result)
}

#' Max information gains (discrete)
#'
#' @param data input data where columns are variables and rows are observations (all discrete with the same number of categories)
#' @param decision decision variable as a binary sequence of length equal to number of observations
#' @param contrast_data the contrast counterpart of data, has to have the same number of observations
#' @param dimensions number of dimensions (a positive integer; 5 max)
#' @param pc.xi parameter xi used to compute pseudocounts (the default is recommended not to be changed)
#' @param return.tuples whether to return tuples where max IG was observed (one tuple per variable) - not supported with CUDA nor in 1D
#' @param interesting.vars variables for which to check the IGs (none = all) - not supported with CUDA
#' @param require.all.vars boolean whether to require tuple to consist of only interesting.vars
#' @return A \code{\link{data.frame}} with the following columns:
#'  \itemize{
#'    \item \code{IG} -- max information gain (of each variable)
#'    \item \code{Tuple.1, Tuple.2, ...} -- corresponding tuple (up to \code{dimensions} columns, available only when \code{return.tuples == T})
#'    \item \code{Discretization.nr} -- always 1 (for compatibility with the non-discrete function; available only when \code{return.tuples == T})
#'  }
#'
#'  Additionally attribute named \code{run.params} with run parameters is set on the result.
#' @examples
#' \donttest{
#' ComputeMaxInfoGainsDiscrete(madelon$data > 500, madelon$decision, dimensions = 2)
#' }
#' @importFrom stats runif
#' @export
#' @useDynLib MDFS r_compute_max_ig_discrete
ComputeMaxInfoGainsDiscrete <- function(
    data,
    decision,
    contrast_data = NULL,
    dimensions = 1,
    pc.xi = 0.25,
    return.tuples = FALSE,
    interesting.vars = vector(mode = "integer"),
    require.all.vars = FALSE) {
  data <- data.matrix(data)
  storage.mode(data) <- "integer"
  if (!is.null(contrast_data)) {
    contrast_data <- data.matrix(contrast_data)
    storage.mode(contrast_data) <- "integer"
    if (nrow(contrast_data) != nrow(data)) {
      stop("Count of contrast data observations differs from count of data observations.")
    }
  }

  decision <- prepare_decision(decision)

  if (length(decision) != nrow(data)) {
    stop("Length of decision is not equal to the number of rows in data.")
  }

  dimensions <- prepare_integer_in_bounds(dimensions, "Dimensions", as.integer(1), as.integer(5))

  divisions <- length(unique(c(data))) - 1
  if (!is.null(contrast_data)) {
    contrast_divisions <- length(unique(c(contrast_data))) - 1
    if (contrast_divisions != divisions) {
      stop("Contrast data has a different number of classes.")
    }
  }

  divisions <- prepare_integer_in_bounds(divisions, "Divisions", as.integer(1), as.integer(15))

  pc.xi <- prepare_double_in_bounds(pc.xi, "pc.xi", .Machine$double.xmin)

  if (dimensions == 1 && return.tuples) {
    stop("return.tuples does not make sense in 1D")
  }

  out <- .Call(
      r_compute_max_ig_discrete,
      data,
      contrast_data,
      decision,
      dimensions,
      divisions,
      pc.xi,
      as.integer(interesting.vars[order(interesting.vars)] - 1),  # send C-compatible 0-based indices
      as.logical(require.all.vars),
      as.logical(return.tuples),
      FALSE)  # CUDA variant is not implemented here

  if (return.tuples) {
    result <- out[1:3]
    names(result) <- c("IG", "Tuple", "Discretization.nr")
    result$Tuple <- t(result$Tuple + 1) # restore R-compatible 1-based indices, transpose to remain compatible with ComputeInterestingTuples
    result$Discretization.nr <- result$Discretization.nr + 1 # restore R-compatible 1-based indices
  } else {
    result <- out[1]
    names(result) <- c("IG")
  }

  result <- as.data.frame(result)

  attr(result, "run.params") <- list(
    dimensions      = dimensions,
    pc.xi           = pc.xi)

  if (!is.null(contrast_data)) {
    if (return.tuples) {
      attr(result, "contrast_igs") <- out[[4]]
    } else {
      attr(result, "contrast_igs") <- out[[2]]
    }
  }

  return(result)
}
