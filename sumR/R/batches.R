#' Approximates the sum of a positive discrete infinite series with a single
#' maximum using the batches algorithm
#'
#' A simple method to perform the summation. It adds the values in batches and
#' stops when the accumulated batch is smaller than the desired threshold. There
#' is an implementation purely in `R` and one in `C`. The one in R is
#' usually slightly faster due to vectorized computing. Result is returned
#' in the log scale.
#' @param logFunction The function that returns the series value
#' \ifelse{html}{\out{a<sub>n</sub>}}{\eqn{a_n}} in
#' the log scale. Can either be an `R` function or a string indicating one
#' of the pre-coded functions. See [precompiled()] for a list of
#' available functions. If defined in `R`, the function's definition must
#' have two arguments. The first argument must be the integer argument
#' equivalent to \eqn{n} in \ifelse{html}{\out{a<sub>n</sub>}}{\eqn{a_n}} and
#' the second must be a vector of numeric parameters.
#' @param parameters A numeric vector with parameters used in logFunction.
#' Vectorized summation over various parameter values sets is not implemented.
#' Use [apply()] or their variants to achieve this.
#' @param batch_size The batch size at which point convergence checking is
#' performed. The algorithm perform at least twice this number of function
#' evaluations. See 'details'.
#' @param epsilon The desired error margin for the approximation. See 'details'.
#' @param maxIter The maximum number of iterations for the approximation. In
#' most cases, this number will not be reached unless it is very small.
#' @param n0 The sum will be approximated for the series starting at this value.
#' @return A [summed-objects()] object. Note that the sum is returned in the
#' \code{log} scale.
#' @seealso [precompiled()] provides a list with precompiled functions
#' that can be used for the summation. [infiniteSum()] is a more
#' efficient algorithm.
#' @details The series \ifelse{html}{\out{a<sub>n</sub>}}{\eqn{a_n}} must pass
#' the ratio convergence test, meaning that the ratio
#' \ifelse{html}{\out{a<sub>n+1</sub>/a<sub>n</sub>}}{\eqn{a_{n+1}/a_n}} must
#' converge to a number \eqn{L < 1} when \eqn{n} goes to infinity.
#'
#' The batches algorithm consists of evaluating the function a fixed number of
#' times for two checkpoints. If the difference between the sum at these
#' checkpoints is smaller than `epsilon`, the code stops and the later
#' checkpoint sum is returned. Else, continue summing until the next checkpoint.
#' All checkpoints are `batch_size` long.
#'
#' This function's efficiency is reliant on the choice of `batch_size`.
#' If it is set too large, the algorithm overshoots the necessary
#' number of function evaluations too much. If it is set too small, the
#' algorithm will need to process too many partial summations which slows it
#' down. However, if they are well calibrated for the series, they can
#' potentially be very efficient.
#'
#' Since the batch sizes are known before the calculations are made,
#' function evaluations can be vectorized. This is why there are two functions
#' available. `infiniteSum_batches` does the calculations at the `R`
#' level, while `infiniteSum_batches_C` interfaces the low level `C`
#' code. However, the `C` code does not use vectorization since it isn't
#' available on long double precision type, and therefore the `R` level
#' function should be faster in most cases.
#' 
#' Another difference is that the low level code uses double precision for the
#' calculations. This means that it is less prone to rounding errors. But this
#' also means that the two functions can sometimes require a different number
#' of iterations and function evaluations to reach the stop criteria. This
#' is shown in the examples.
#'
#' Another requirement in the current installment of this function is that the
#' series must have only a single maximum. This is the case for most discrete
#' probability distributions and marginalization problems. This limitation
#' will be addressed in the future.
#' @examples
#' ## Define some function that is known to pass the ratio test.
#' param = 0.1
#' funfun <- function(k, p) return(k * log1p(-p[1]))
#' result <- infiniteSum_batches(funfun, parameters = param)
#'
#' ## This series is easy to verify analytically
#' TrueSum = -log(param)
#' TrueSum - result$sum
#' # Notice that it required 400 function evaluations for the approximation.
#' result$n
#' 
#' # If we use the C function, it reaches a lower error, but requires more
#' # iterations
#' result_C <- infiniteSum_batches_C(funfun, parameters = param)
#' TrueSum - result_C$sum
#' result_C$n
#'
#' ## A common problem is finding the normalizing constant for the
#' ## Conway-Maxwell-Poisson distribution. It has already been included
#' ## in the precompiled list of functions.
#' comp_params = c(lambda = 5, nu = 3)
#' result <- infiniteSum_batches("COMP", comp_params)
#' # With a specifically chosen argument value, the summation can be done with
#' # fewer iterations. But it is usually hard to know the ideal choice for
#' # applications beforehand
#' result$n
#' infiniteSum_batches("COMP", comp_params, batch_size = 11)$n
#' # A small batch_size ensures a small number of iterations, but slows the
#' # method due to multiple checking.
#' infiniteSum_batches("COMP", comp_params, batch_size = 2)$n
#' @importFrom matrixStats logSumExp
#' @export
infiniteSum_batches <- function(logFunction, parameters = numeric(),
                                batch_size = 40, epsilon = 1e-15, maxIter = 1e5,
                                n0 = 0){

  stopifnot(is.function(logFunction) || is.character(logFunction),
            length(logFunction) == 1,
            is.numeric(parameters),
            is.numeric(batch_size),
            batch_size > 1,
            length(batch_size) == 1,
            is.numeric(epsilon),
            epsilon > 0,
            length(epsilon) == 1,
            is.numeric(maxIter),
            maxIter > 0,
            length(maxIter) == 1,
            is.numeric(n0),
            n0 >= 0,
            length(n0) == 1)

  if (is.character(logFunction)){
    if (logFunction == "COMP"){
      stopifnot(length(parameters) == 2)
      logFunction <- COMP
    } else
    if (logFunction == "double_poisson"){
      stopifnot(length(parameters) == 2)
      logFunction <- dbl_poisson
    } else
    if (logFunction == "bessel_I"){
      stopifnot(length(parameters) == 2)
      logFunction <- bessel_I
    } else
    if (logFunction == "bessel_I_logX"){
      stopifnot(length(parameters) == 2)
      logFunction <- bessel_I_logX
    } else
      stop("log function not found.")
    if (any(is.infinite(parameters))) stop("Parameters must be finite.")
  }

  # Setup
  lEps <- log(epsilon)
  nextCheckPoint <- n0 + batch_size
  funValues <- logFunction(n0:nextCheckPoint, parameters)
  lastCheckPoint <- nextCheckPoint + 1
  nextCheckPoint <- nextCheckPoint + batch_size
  increment <- logFunction(lastCheckPoint:nextCheckPoint, parameters)
  summedIncrement <- matrixStats::logSumExp(increment)
  n <- batch_size * 2

  # Convergence checking
  while (n < maxIter && (summedIncrement > lEps ||
                         increment[length(increment)] -
                         increment[length(increment) - 1] >
                         -log1p(exp(increment[length(increment)] -
                                    summedIncrement)) ||
         funValues[length(funValues)] > funValues[length(funValues) - 1] ||
         is.infinite(funValues[length(funValues)]))){
    funValues <- c(funValues, increment)
    lastCheckPoint <- nextCheckPoint + 1
    nextCheckPoint <- nextCheckPoint + batch_size
    increment <- logFunction(lastCheckPoint:nextCheckPoint, parameters)
    summedIncrement <- matrixStats::logSumExp(increment)
    n <- n + batch_size
  }
  funValues <- c(funValues, increment)

  out <- list(sum = matrixStats::logSumExp(sort(funValues)),
              n = n, method = "Batches in R", maxReached = n >= maxIter)
  class(out) <- "summed"
  out
}

#' @rdname infiniteSum_batches
#' @export
infiniteSum_batches_C <- function(logFunction, parameters = numeric(),
                                  batch_size = 40, epsilon = 1e-15,
                                  maxIter = 1e5, n0 = 0){

  stopifnot(is.function(logFunction) || is.character(logFunction),
            length(logFunction) == 1,
            is.numeric(parameters),
            is.numeric(batch_size),
            batch_size > 1,
            length(batch_size) == 1,
            is.numeric(epsilon),
            epsilon > 0,
            length(epsilon) == 1,
            is.numeric(maxIter),
            maxIter > 0,
            length(maxIter) == 1,
            is.numeric(n0),
            n0 >= 0,
            length(n0) == 1)

  maxIter <- as.integer(maxIter); n0 <- as.integer(n0)
  batch_size <- as.integer(batch_size)

  if (is.character(logFunction)){
    out <- .Call("infinite_batches_precomp",
                 logFunction, parameters, batch_size, epsilon, maxIter, n0,
                 PACKAGE = "sumR")
    } else if (is.function(logFunction)) {
    f <- function(k, Theta) logFunction(k, Theta)

    out <- .Call("inf_batches",
                 body(f), parameters, batch_size, epsilon,
                 maxIter, n0, new.env(),
                 PACKAGE = "sumR")
  } else {
    warning('Argument lFun must either be the name of a precompiled function
            or a function. See help("precompiled") to see which functions are
            available.')
    out <- list(sum = -Inf, n = 0, method = "canceled")
    class(out) <- "summed"
    out
  }

  out$method <- "Batches in C"
  out$maxReached <- out$n >= maxIter
  class(out) <- "summed"
  out
}
