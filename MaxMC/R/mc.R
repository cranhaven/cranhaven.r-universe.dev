#' Monte Carlo with Tie-Breaker
#'
#' Find the Monte Carlo (MC) p-value by generating N replications
#' of a statistic.
#'
#' The \code{dgp} function defined by the user is used to
#' generate new observations in order to compute the simulated
#' statistics.
#'
#' Then \code{\link{pvalue}} is applied to the statistic and
#' its simulated values. \code{\link{pvalue}} computes the
#' p-value by ranking the statistic compared to its simulated
#' values. Ties in the ranking are broken according to a uniform
#' distribution.
#'
#' We allow for four types of p-value: \code{leq}, \code{geq},
#' \code{absolute} and \code{two-tailed}. For one-tailed test,
#' \code{leq} returns the proportion of simulated values smaller
#' than the statistic while \code{geq} returns the proportion of
#' simulated values greater than the statistic. For two-tailed
#' test with a symmetric statistic, one can use the
#' absolute value of the statistic and its simulated values to
#' retrieve a two-tailed test (i.e. type = \code{absolute}).
#' If the statistic is not symmetric, one can specify the p-value
#' type as \code{two-tailed} which is equivalent to twice the minimum
#' of \code{leq} and \code{geq}.
#'
#' Ties in the ranking are broken according to a uniform
#' distribution.
#'
#' @param dgp A function. The function inputs the first argument
#' \code{y} and outputs a simulated \code{y}. It should represent the
#' data generating process under the null. Default value is the function
#' \code{sample(y, replace = TRUE)}, i.e. the
#' bootstrap resampling of \code{y}.
#'
#' @inheritParams mmc
#' @inheritParams pvalue
#'
#' @return The returned value is an object of class \code{mc}
#' containing the following components:
#'  \item{S0}{Observed value of \code{statistic}.}
#'  \item{pval}{Monte Carlo p-value of \code{statistic}.}
#'  \item{y}{Data specified in call.}
#'  \item{statistic}{\code{statistic} function specified in call.}
#'  \item{dgp}{\code{dgp} function specified in call.}
#'  \item{N}{Number of replications specified in call.}
#'  \item{type}{\code{type} of p-value specified in call.}
#'  \item{call}{Original call to \code{mmc}.}
#'  \item{seed}{Value of \code{.Random.seed} at the start of
#'  \code{mc} call.}
#'
#' @references Dufour, J.-M. (2006), Monte Carlo Tests with nuisance parameters:
#' A general approach to finite sample inference and nonstandard asymptotics in econometrics.
#' \emph{Journal of Econometrics}, \bold{133(2)}, 443-447.
#'
#' @references Dufour, J.-M. and Khalaf L. (2003), Monte Carlo Test Methods in Econometrics.
#' in Badi H. Baltagi, ed., \emph{A Companion to Theoretical Econometrics}, Blackwell Publishing Ltd, 494-519.
#'
#' @seealso \code{\link{mmc}}, \code{\link{pvalue}}
#' @example /inst/examples/mc_example.R
#' @export
#'
mc <- function(y, statistic, ..., dgp = function(y) sample(y, replace = TRUE),
                       N = 99, type = c("geq", "leq", "absolute", "two-tailed")) {
  # Match type and extract exact call
  type <- match.arg(type)
  call <- match.call()

  # Extract function from string of characters in statistic
  if (is.character(statistic)) {
    statistic <- get(statistic, mode = "function", envir = parent.frame())
  }
  # Test if statistic is a function
  if (!is.function(statistic)) {
    stop("'statistic' must be a function or a string naming
         a valid function")
  }
  # Generate seed if none is specified in GlobalEnv
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)){
    stats::runif(1)
  }
  # Extract seed integer vector
  seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  # Compute the statistic
  S0 <- statistic(y, ...)

  # Test if S0 is a vector of length 1
  if (length(S0)!=1 && !is.atomic(S0)){
      stop("'statistic' must return an atomic vector of
           length = 1")
  }

  # Simulate the statistic N times
  S <- simulation_mc(y, statistic, dgp, N, ...)
  # Compute the p-value
  pval <- pvalue(S0, S, type)

  # Return "mc" object
  return_mc(S0 = S0, y = y, statistic = statistic, dgp = dgp,
            N = N, type = type, call = call, seed = seed, pval= pval)
}

