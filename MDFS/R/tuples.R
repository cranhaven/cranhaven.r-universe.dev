#' Interesting tuples
#'
#' @details
#' If running in 2D and no filtering is applied, this function is able to run in an
#' optimised fashion. It is recommended to avoid filtering in 2D if only it is
#' feasible.
#'
#' @details
#' This function calculates what \code{stat_mode} dictates.
#' When \code{decision} is omitted, the \code{stat_mode} is calculated on the descriptive variables.
#' When \code{decision} is given, the \code{stat_mode} is calculated on the decision variable, conditional on the other variables.
#' Translate "IG" to that value in the rest of this function's description.
#'
#' @param data input data where columns are variables and rows are observations (all numeric)
#' @param decision decision variable as a binary sequence of length equal to number of observations
#' @param dimensions number of dimensions (a positive integer; 5 max)
#' @param divisions number of divisions (from 1 to 15)
#' @param discretizations number of discretizations
#' @param seed seed for PRNG used during discretizations (\code{NULL} for random)
#' @param range discretization range (from 0.0 to 1.0; \code{NULL} selects probable optimal number)
#' @param pc.xi parameter xi used to compute pseudocounts (the default is recommended not to be changed)
#' @param ig.thr IG threshold above which the tuple is interesting (0 and negative mean no filtering)
#' @param I.lower IG values computed for lower dimension (1D for 2D, etc.)
#' @param interesting.vars variables for which to check the IGs (none = all)
#' @param require.all.vars boolean whether to require tuple to consist of only interesting.vars
#' @param return.matrix boolean whether to return a matrix instead of a list (ignored if not using the optimised method variant)
#' @param stat_mode character, one of: "MI" (mutual information, the default; becomes information gain when \code{decision} is given), "H" (entropy; becomes conditional entropy when \code{decision} is given), "VI" (variation of information; becomes target information difference when \code{decision} is given); decides on the value computed
#' @param average boolean whether to average over discretisations instead of maximising (the default)
#' @return A \code{\link{data.frame}} or \code{\link{NULL}} (following a warning) if no tuples are found.
#'
#'  The following columns are present in the \code{\link{data.frame}}:
#'  \itemize{
#'    \item \code{Var} -- interesting variable index
#'    \item \code{Tuple.1, Tuple.2, ...} -- corresponding tuple (up to \code{dimensions} columns)
#'    \item \code{IG} -- information gain achieved by \code{var} in \code{Tuple.*}
#'  }
#'
#'  Additionally attribute named \code{run.params} with run parameters is set on the result.
#' @examples
#' \donttest{
#' ig.1d <- ComputeMaxInfoGains(madelon$data, madelon$decision, dimensions = 1, divisions = 1,
#'                              range = 0, seed = 0)
#' ComputeInterestingTuples(madelon$data, madelon$decision, dimensions = 2, divisions = 1,
#'                          range = 0, seed = 0, ig.thr = 100, I.lower = ig.1d$IG)
#' }
#' @export
#' @useDynLib MDFS r_compute_all_matching_tuples
ComputeInterestingTuples <- function(
    data,
    decision = NULL,
    dimensions = 2,
    divisions = 1,
    discretizations = 1,
    seed = NULL,
    range = NULL,
    pc.xi = 0.25,
    ig.thr = 0,
    I.lower = NULL,
    interesting.vars = vector(mode = "integer"),
    require.all.vars = FALSE,
    return.matrix = FALSE,
    stat_mode = "MI",
    average = FALSE) {
  if (!(stat_mode %in% c("MI", "H", "VI"))) {
    stop("stat_mode has to be one of MI, H or VI.")
  }

  stat_mode_map <- list(
    H = 1,
    MI = 2,
    VI = 3
  )
  stat_mode <- stat_mode_map[stat_mode]

  if (!is.null(decision) && dimensions > 2 && stat_mode == 3) {
    stop("Unable to compute target information difference in higher than 2 dimensions (it is not well defined)")
  }

  if (is.null(decision) && dimensions > 2 && stat_mode != 1) {
    stop("Unable to compute decisionless non-entropy statistics in higher than 2 dimensions (they are not defined)")
  }

  data <- data.matrix(data)
  storage.mode(data) <- "double"

  if (!is.null(I.lower)) {
    if (length(I.lower) != ncol(data)) {
      stop("Length of I.lower is not equal to the number of columns in data.")
    }

    if (dimensions != 2) {
      # TODO:
      stop("More dimensions than 2 not supported with I.lower set.")
    }

    I.lower <- as.double(I.lower)
  }

  if (!is.null(decision)) {
    decision <- prepare_decision(decision)

    if (length(decision) != nrow(data)) {
      stop("Length of decision is not equal to the number of rows in data.")
    }
  }

  dimensions <- prepare_integer_in_bounds(dimensions, "Dimensions", as.integer(2), as.integer(5))

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

  result <- .Call(
      r_compute_all_matching_tuples,
      data,
      decision,
      dimensions,
      divisions,
      discretizations,
      seed,
      range,
      pc.xi,
      as.integer(interesting.vars[order(interesting.vars)] - 1),  # send C-compatible 0-based indices
      as.logical(require.all.vars),
      as.double(ig.thr),
      I.lower,
      as.logical(return.matrix),
      as.integer(stat_mode),
      as.logical(average))

  if (dimensions == 2 && length(interesting.vars) == 0 && ig.thr <= 0 && return.matrix) {
    # do nothing, we have a matrix for you
  } else {
    if (length(result[[1]]) == 0) {
      warning("No tuples were returned.")
      return(NULL)
    }

    names(result) <- c("Var", "Tuple", "IG")

    result$Var <- result$Var + 1 # restore R-compatible 1-based indices
    result$Tuple <- result$Tuple + 1 # restore R-compatible 1-based indices

    result <- as.data.frame(result)
  }

  attr(result, "run.params") <- list(
    dimensions      = dimensions,
    divisions       = divisions,
    discretizations = discretizations,
    seed            = seed,
    range           = range,
    pc.xi           = pc.xi)

  return(result)
}

#' Interesting tuples (discrete)
#'
#' @details
#' If running in 2D and no filtering is applied, this function is able to run in an
#' optimised fashion. It is recommended to avoid filtering in 2D if only it is
#' feasible.
#'
#' @details
#' This function calculates what \code{stat_mode} dictates.
#' When \code{decision} is omitted, the \code{stat_mode} is calculated on the descriptive variables.
#' When \code{decision} is given, the \code{stat_mode} is calculated on the decision variable, conditional on the other variables.
#' Translate "IG" to that value in the rest of this function's description.
#'
#' @param data input data where columns are variables and rows are observations (all discrete with the same number of categories)
#' @param decision decision variable as a binary sequence of length equal to number of observations
#' @param dimensions number of dimensions (a positive integer; 5 max)
#' @param pc.xi parameter xi used to compute pseudocounts (the default is recommended not to be changed)
#' @param ig.thr IG threshold above which the tuple is interesting (0 and negative mean no filtering)
#' @param I.lower IG values computed for lower dimension (1D for 2D, etc.)
#' @param interesting.vars variables for which to check the IGs (none = all)
#' @param require.all.vars boolean whether to require tuple to consist of only interesting.vars
#' @param return.matrix boolean whether to return a matrix instead of a list (ignored if not using the optimised method variant)
#' @param stat_mode character, one of: "MI" (mutual information, the default; becomes information gain when \code{decision} is given), "H" (entropy; becomes conditional entropy when \code{decision} is given), "VI" (variation of information; becomes target information difference when \code{decision} is given); decides on the value computed
#' @return A \code{\link{data.frame}} or \code{\link{NULL}} (following a warning) if no tuples are found.
#'
#'  The following columns are present in the \code{\link{data.frame}}:
#'  \itemize{
#'    \item \code{Var} -- interesting variable index
#'    \item \code{Tuple.1, Tuple.2, ...} -- corresponding tuple (up to \code{dimensions} columns)
#'    \item \code{IG} -- information gain achieved by \code{var} in \code{Tuple.*}
#'  }
#'
#'  Additionally attribute named \code{run.params} with run parameters is set on the result.
#' @examples
#' \donttest{
#' ig.1d <- ComputeMaxInfoGainsDiscrete(madelon$data > 500, madelon$decision, dimensions = 1)
#' ComputeInterestingTuplesDiscrete(madelon$data > 500, madelon$decision, dimensions = 2,
#'                                  ig.thr = 100, I.lower = ig.1d$IG)
#' }
#' @export
#' @useDynLib MDFS r_compute_all_matching_tuples_discrete
ComputeInterestingTuplesDiscrete <- function(
    data,
    decision = NULL,
    dimensions = 2,
    pc.xi = 0.25,
    ig.thr = 0,
    I.lower = NULL,
    interesting.vars = vector(mode = "integer"),
    require.all.vars = FALSE,
    return.matrix = FALSE,
    stat_mode = "MI") {
  if (!(stat_mode %in% c("MI", "H", "VI"))) {
    stop("stat_mode has to be one of MI, H or VI.")
  }

  stat_mode_map <- list(
    H = 1,
    MI = 2,
    VI = 3
  )
  stat_mode <- stat_mode_map[stat_mode]

  if (!is.null(decision) && dimensions > 2 && stat_mode == 3) {
    stop("Unable to compute target information difference in higher than 2 dimensions (it is not well defined)")
  }

  if (is.null(decision) && dimensions > 2 && stat_mode != 1) {
    stop("Unable to compute decisionless non-entropy statistics in higher than 2 dimensions (they are not defined)")
  }

  data <- data.matrix(data)
  storage.mode(data) <- "integer"

  if (!is.null(I.lower)) {
    if (length(I.lower) != ncol(data)) {
      stop("Length of I.lower is not equal to the number of columns in data.")
    }

    if (dimensions != 2) {
      # TODO:
      stop("More dimensions than 2 not supported with I.lower set.")
    }

    I.lower <- as.double(I.lower)
  }

  if (!is.null(decision)) {
    decision <- prepare_decision(decision)

    if (length(decision) != nrow(data)) {
      stop("Length of decision is not equal to the number of rows in data.")
    }
  }

  dimensions <- prepare_integer_in_bounds(dimensions, "Dimensions", as.integer(2), as.integer(5))

  divisions <- length(unique(c(data))) - 1

  divisions <- prepare_integer_in_bounds(divisions, "Divisions", as.integer(1), as.integer(15))

  pc.xi <- prepare_double_in_bounds(pc.xi, "pc.xi", .Machine$double.xmin)

  result <- .Call(
      r_compute_all_matching_tuples_discrete,
      data,
      decision,
      dimensions,
      divisions,
      pc.xi,
      as.integer(interesting.vars[order(interesting.vars)] - 1),  # send C-compatible 0-based indices
      as.logical(require.all.vars),
      as.double(ig.thr),
      I.lower,
      as.logical(return.matrix),
      as.integer(stat_mode))

  if (dimensions == 2 && length(interesting.vars) == 0 && ig.thr <= 0 && return.matrix) {
    # do nothing, we have a matrix for you
  } else {
    if (length(result[[1]]) == 0) {
      warning("No tuples were returned.")
      return(NULL)
    }

    names(result) <- c("Var", "Tuple", "IG")

    result$Var <- result$Var + 1 # restore R-compatible 1-based indices
    result$Tuple <- result$Tuple + 1 # restore R-compatible 1-based indices

    result <- as.data.frame(result)
  }

  attr(result, "run.params") <- list(
    dimensions      = dimensions,
    divisions       = divisions,
    range           = range,
    pc.xi           = pc.xi)

  return(result)
}
