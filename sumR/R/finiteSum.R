#' Calculates the sum of a discrete series with a single maximum for a pre-set
#' number of evaluations
#'
#' A discrete finite series is calculated exactly with no need for
#' approximations. This can also be used for infinite series approximation with
#' a pre-determined number of iterations, but this has no guarantee of quality
#' of approximation.  Result is returned in the log scale.
#' @param logFunction The function that returns the series value
#' \ifelse{html}{\out{a<sub>n</sub>}}{\eqn{a_n}} in
#' the log scale. Can either be an `R` function or a string indicating one
#' of the precompiled functions. See [precompiled()] for a list of
#' available functions. If defined in `R`, the function's definition must
#' have two arguments. The first argument must be the integer argument
#' equivalent to \eqn{n} in \ifelse{html}{\out{a<sub>n</sub>}}{\eqn{a_n}} and
#' the second must be a vector of numeric parameters.
#' @param parameters A numeric vector with parameters used in logFunction.
#' Vectorized summation over various parameter values sets is not implemented.
#' Use [apply()] or their variants to achieve this.
#' @param n A single integer positive number indicating the number of iterations
#' to perform in the function.
#' @param n0 The sum will be performed for the series starting at this value.
#' @return A [summed-objects()] object. Note that the sum is returned in the
#' \code{log} scale.
#' @seealso [precompiled()] provides a list with precompiled functions
#' that can be used for the summation.
#' @importFrom matrixStats logSumExp
#' @examples
#' # Sum values from 5 to 100
#' finiteSum(function(x, p) log(x), numeric(), 100 - 5, 5)
#' @export
finiteSum <- function(logFunction, parameters = numeric(), n, n0 = 0){
  stopifnot(is.function(logFunction) || is.character(logFunction),
            length(logFunction) == 1,
            is.numeric(parameters),
            is.numeric(n),
            length(n) == 1,
            n > 0,
            is.numeric(n0),
            n0 >= 0,
            length(n0) == 1)

  n <- as.integer(n); n0 <- as.integer(n0)

  if (is.character(logFunction)){
    out <- .Call("sum_n_times_precomp", logFunction, parameters, n, n0,
                 PACKAGE = "sumR")
  } else
    out <- list(
      sum = matrixStats::logSumExp(sort(logFunction(n0:(n0 + n), parameters))),
      n = n)

  out$method <- "Finite sum"
  out$maxReached <- TRUE
  class(out) <- "summed"
  out
}
