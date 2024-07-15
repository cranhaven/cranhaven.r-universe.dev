#' Refined optimization test.
#'
#' If the potential solution is indeed a local optimum of the objective function, and if it is used to initialize a second optimization, then original and "refined" solutions ought to be close.
#'
#' @param xsol Potential solution vector of length \code{nx}.
#' @param fun Objective function to be maximized (or minimized), with first argument the length-\code{nx} parameter vector over which optimization is to take place.  Should return a scalar result.
#' @param maximize Logical, whether a maximum or a minimum of the objective function is sought.
#' @param maxit Maximum number of iterations for \code{\link[stats]{optim}} refit (see Details).
#' @param reltol Relative tolerance for convergence of \code{\link[stats]{optim}} refit (see Details).
#' @param xopt Optional refit solution calculated externally from an optimization algorithm of choice (see Details).
#' @return An object of class \code{optrefit} inheriting from \code{optcheck}, with elements:
#' \describe{
#'   \item{\code{xsol}}{The potential solution.}
#'   \item{\code{ysol}}{The value of \code{fun(xsol)}.}
#'   \item{\code{maximize}}{Logical indicating whether the potential solution should maximize or minimize the objective function.}
#'   \item{\code{xopt}}{The solution found by the general-purpose optimizer.}
#'   \item{\code{yopt}}{The function value at the optimal solution, i.e., \code{fun(xopt)}.}
#' }
#' @details By default, a so-called **refi**ned op(**t**)imization (or refit) test is performed by running the default Nelder-Mead simplex method provided by \code{\link[stats]{optim}}, initialized by the potential solution \code{xsol}.  Only a simplified interface to \code{\link[stats]{optim}}'s control parameters are provided here.
#'
#' Alternatively, the refit test can be performed with any optimization algorithm of choice.  This is done externally, with the refined solution passed to \code{optim_refit} via the argument \code{xopt}.
#' @seealso \code{summary}, \code{print}, and \code{diff} for \code{optrefit} objects are available; see \code{\link{summary.optrefit}}, \code{\link{print.optrefit}}, and \code{\link{diff.optrefit}}.
#' @export
optim_refit <- function(xsol, fun, maximize = TRUE,
                        maxit = 5e3, reltol = 1e-8, xopt) {
  if(missing(xopt)) {
    ans <- optim(par = xsol, fn = fun, method = "Nelder-Mead",
                 control = list(fnscale = ifelse(maximize, -1, 1),
                                parscale = abs(xsol),
                                maxit = maxit, reltol = reltol))
    if(ans$convergence == 1) {
      warning("Iteration limit maxit has been reached.")
    }
    if(ans$convergence == 10) {
      warning("Degeneracy of the Nelder-Mead simplex.")
    }
  } else {
    ans <- list(par = xopt, value = fun(xopt))
  }
  ans <- list(xsol = xsol, ysol = fun(xsol),
              maximize = maximize,
              xopt = ans$par, yopt = ans$value)
  class(ans) <- c("optrefit", "optcheck")
  ans
}
