#' Format the optimization method controls
#'
#' This function provides a way to merge the user specified controls for the
#' optimization methods with their respective default controls.
#'
#' @inheritParams mmc
#'
#' @return A list. Arguments to be used to control the behavior
#' of the algorithm chosen in \code{method}.
#'
#' @example /inst/examples/get_control_example.R
#'
#' @keywords internal
#'
get_control <- function(method, control = list()) {
  if (method == "GenSA") {
      default_control <- list(maxit = 1000, threshold.stop = -1, temperature = 5230,
                              visiting.param = 2.62, acceptance.param = -5, max.time = NULL,
                              nb.stop.improvement = 25, smooth = TRUE, max.call = 1e+07,
                              simple.function = FALSE, trace.fn = NULL, verbose = FALSE,
                              trace.mat = TRUE)
  } else if (method == "pso") {
      default_control <- list(trace = 0, fnscale = 1, maxit = 1000L, maxf = Inf,
                              abstol = -1, reltol = 0, REPORT = 1, s = NA, k = 3,
                              p = NA, w = 1/(2 * log(2)), c.p = 0.5 + log(2),
                              c.g = 0.5 + log(2),  d = NA, v.max = NA, rand.order = TRUE,
                              max.restart = Inf, maxit.stagnate = 25, vectorize = TRUE,
                              hybrid = FALSE, hybrid.control = NULL, trace.stats = TRUE,
                              type = "SPSO2007")
  } else if (method == "GA") {
      default_control <- list(popSize = 50, pcrossover = 0.8, pmutation = 0.1,
                              updatePop = FALSE, postFitness = NULL, maxiter = 100, run = 25,
                              maxFitness = 1, names = NULL, suggestions = NULL, optim = FALSE,
                              optimArgs = list(method = "L-BFGS-B",poptim = 0.05,
                                               pressel = 0.5, control = list(fnscale = -1,
                                                                             maxit = 100)),
                              keepBest = FALSE, parallel = FALSE, monitor = FALSE)
  } else if (method == "gridSearch") {
      default_control <- list(npar = 1L, n = 10L,
                              printDetail = TRUE, method = NULL, mc.control = list(), cl = NULL,
                              keepNames = FALSE, asList = FALSE)
  }
    nmsC <- names(default_control)
    default_control[(namc <- names(control))] <- control
    if (length(noNms <- namc[!namc %in% nmsC])) {
        warning("unknown names in control: ", paste(noNms, collapse = ", "))
    }
  return(default_control)
}
