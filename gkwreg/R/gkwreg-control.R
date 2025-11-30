#' Control Parameters for Generalized Kumaraswamy Regression
#'
#' @description
#' Auxiliary function for controlling \code{gkwreg()} fitting process.
#' This function consolidates all technical/advanced fitting options in one place,
#' keeping the main \code{gkwreg()} interface clean and user-friendly. Follows
#' the same design pattern as \code{\link[stats]{glm.control}},
#' \code{\link[betareg]{betareg.control}}, and similar control functions in R.
#'
#' @param method Character string specifying the optimization algorithm.
#'   Options: \code{"nlminb"} (default), \code{"BFGS"}, \code{"Nelder-Mead"},
#'   \code{"CG"}, \code{"SANN"}, \code{"L-BFGS-B"}. If \code{"nlminb"}, uses
#'   \code{\link[stats]{nlminb}}; otherwise uses \code{\link[stats]{optim}}
#'   with the specified method.
#'
#' @param start Optional named list of starting values for regression coefficients.
#'   Names should match parameter names (alpha, beta, gamma, delta, lambda).
#'   If \code{NULL} (default), starting values are determined automatically.
#'
#' @param fixed Optional named list of parameters to hold fixed at specific
#'   values during estimation. Currently experimental. Default \code{NULL}.
#'
#' @param hessian Logical. If \code{TRUE} (default), compute the Hessian matrix
#'   via \code{\link[TMB]{sdreport}} to obtain standard errors and variance-covariance
#'   matrix. Set to \code{FALSE} for faster fitting when standard errors are not needed.
#'
#' @param maxit Integer. Maximum number of iterations for the optimizer.
#'   Default 500 for derivative-based methods, 10000 for SANN.
#'   Increase for difficult optimization problems.
#'
#' @param reltol Numeric. Relative convergence tolerance for the optimizer.
#'   Default \code{sqrt(.Machine$double.eps)} approx. 1.5e-8. Smaller values require
#'   tighter convergence but may increase computation time. Used by Nelder-Mead,
#'   BFGS, and CG methods.
#'
#' @param abstol Numeric. Absolute convergence tolerance. Default 0.
#'   Used by some optimization methods as an additional stopping criterion.
#'
#' @param trace Integer. Controls verbosity of the optimizer.
#'   \itemize{
#'     \item 0: Silent (default)
#'     \item 1: Print iteration progress
#'     \item 2+: Print detailed diagnostic information (up to 6 for L-BFGS-B)
#'   }
#'   Ignored if \code{silent = TRUE}.
#'
#' @param silent Logical. If \code{TRUE} (default), suppress all progress messages
#'   from TMB compilation and optimization. Set to \code{FALSE} for debugging or
#'   to monitor long-running fits.
#'
#' @param eval.max Integer. Maximum number of function evaluations (nlminb only).
#'   Default 500. Increase for difficult optimization problems.
#'
#' @param iter.max Integer. Maximum number of iterations (nlminb only).
#'   Default 300. Usually less than \code{eval.max}.
#'
#' @param step.min Numeric. Minimum step length (nlminb only). Default 1e-8.
#'   Controls how small steps can become before stopping.
#'
#' @param step.max Numeric. Maximum step length (nlminb only). Default 1.
#'   Useful for preventing overshooting in difficult optimization problems.
#'
#' @param x.tol Numeric. Tolerance for parameter convergence (nlminb only).
#'   Default 1.5e-8. Optimizer stops if parameter changes are smaller than this.
#'
#' @param rel.tol Numeric. Relative tolerance for function value (nlminb only).
#'   Default \code{sqrt(.Machine$double.eps)}. Alternative specification of
#'   relative tolerance.
#'
#' @param alpha Numeric. Reflection factor for Nelder-Mead method. Default 1.0.
#'   Only used when \code{method = "Nelder-Mead"}.
#'
#' @param beta Numeric. Contraction factor for Nelder-Mead method. Default 0.5.
#'   Only used when \code{method = "Nelder-Mead"}.
#'
#' @param gamma Numeric. Expansion factor for Nelder-Mead method. Default 2.0.
#'   Only used when \code{method = "Nelder-Mead"}.
#'
#' @param type Integer. Update formula for CG method. Options:
#'   \itemize{
#'     \item 1: Fletcher-Reeves update
#'     \item 2: Polak-Ribiere update
#'     \item 3: Beale-Sorenson update
#'   }
#'   Default 1. Only used when \code{method = "CG"}.
#'
#' @param temp Numeric. Starting temperature for SANN method. Default 10.
#'   Only used when \code{method = "SANN"}.
#'
#' @param tmax Integer. Number of function evaluations at each temperature
#'   for SANN method. Default 10. Only used when \code{method = "SANN"}.
#'
#' @param lmm Integer. Number of BFGS updates retained in L-BFGS-B method.
#'   Default 5. Only used when \code{method = "L-BFGS-B"}.
#'
#' @param factr Numeric. Convergence tolerance factor for L-BFGS-B method.
#'   Convergence occurs when the reduction in the objective is within this
#'   factor of the machine tolerance. Default 1e7 (tolerance ~1e-8).
#'   Only used when \code{method = "L-BFGS-B"}.
#'
#' @param pgtol Numeric. Tolerance on the projected gradient for L-BFGS-B method.
#'   Default 0 (check suppressed). Only used when \code{method = "L-BFGS-B"}.
#'
#' @param REPORT Integer. Frequency of progress reports for BFGS, L-BFGS-B and SANN
#'   methods when \code{trace > 0}. Default 10 for BFGS/L-BFGS-B, 100 for SANN.
#'
#' @param warn.1d.NelderMead Logical. Whether to warn when Nelder-Mead is used
#'   for one-dimensional optimization. Default \code{TRUE}.
#'
#' @param fnscale Numeric. Overall scaling to be applied to the function value
#'   and gradient during optimization. Default 1. If negative, turns the problem
#'   into a maximization problem.
#'
#' @param parscale Numeric vector. Scaling values for parameters. Optimization
#'   is performed on par/parscale. Default \code{rep(1, n_params)}.
#'
#' @param ndeps Numeric vector. Step sizes for finite-difference approximation
#'   to the gradient. Default 1e-3.
#'
#' @param ... Additional arguments passed to the optimizer. Allows fine-grained
#'   control without formally adding parameters. Advanced users only.
#'
#' @return An object of class \code{"gkw_control"}, which is a list containing
#'   all control parameters with validated and default-filled values. This object
#'   is passed to \code{gkwreg()} via the \code{control} argument.
#'
#' @details
#' This function provides a centralized way to set all technical parameters
#' for model fitting. It serves several purposes:
#' \itemize{
#'   \item \strong{Clean interface}: \code{gkwreg()} has fewer arguments
#'   \item \strong{Organized documentation}: All technical options documented here
#'   \item \strong{Input validation}: Parameters validated before fitting
#'   \item \strong{Extensibility}: New options can be added without changing \code{gkwreg()}
#'   \item \strong{Backward compatibility}: Old code continues working
#' }
#'
#' \strong{Method-specific parameters:}
#'
#' Each optimization method accepts different control parameters:
#' \itemize{
#'   \item \strong{Nelder-Mead}: \code{alpha}, \code{beta}, \code{gamma}, \code{maxit},
#'         \code{reltol}, \code{abstol}, \code{trace}, \code{REPORT}, \code{warn.1d.NelderMead}
#'   \item \strong{BFGS}: \code{maxit}, \code{reltol}, \code{abstol}, \code{trace}, \code{REPORT}
#'   \item \strong{CG}: \code{type}, \code{maxit}, \code{reltol}, \code{abstol}, \code{trace}
#'   \item \strong{SANN}: \code{temp}, \code{tmax}, \code{maxit}, \code{trace}, \code{REPORT}
#'   \item \strong{L-BFGS-B}: \code{lmm}, \code{factr}, \code{pgtol}, \code{trace}, \code{REPORT}
#' }
#'
#' \strong{When to use gkw_control():}
#'
#' Most users never need to adjust these settings. Use \code{gkw_control()} when:
#' \itemize{
#'   \item Optimization fails to converge (increase \code{maxit}, adjust tolerances)
#'   \item Debugging fit problems (set \code{silent = FALSE}, \code{trace = 1})
#'   \item Comparing optimizers (try \code{method = "BFGS"} vs \code{"nlminb"})
#'   \item Fine-tuning performance (disable \code{hessian} if SEs not needed)
#'   \item Using custom starting values (\code{start = list(...)})
#' }
#'
#' \strong{Recommended practices:}
#' \itemize{
#'   \item Start with defaults, only adjust if needed
#'   \item Increase \code{maxit} before adjusting tolerances
#'   \item Use \code{trace = 1} to diagnose convergence issues
#'   \item Disable \code{hessian} for speed if only point estimates needed
#'   \item Try different \code{method}s if one fails (BFGS often more robust)
#'   \item For L-BFGS-B with bounds, adjust \code{factr} and \code{pgtol} if needed
#' }
#'
#' @examples
#' \donttest{
#' # Default control (used automatically if not specified)
#' ctrl <- gkw_control()
#' print(ctrl)
#'
#' # Increase iterations for difficult problem
#' ctrl_robust <- gkw_control(maxit = 1000, trace = 1)
#'
#' # Try alternative optimizer
#' ctrl_bfgs <- gkw_control(method = "BFGS")
#'
#' # Fast fitting without standard errors
#' ctrl_fast <- gkw_control(hessian = FALSE)
#'
#' # Verbose debugging
#' ctrl_debug <- gkw_control(silent = FALSE, trace = 2)
#'
#' # Custom starting values
#' ctrl_start <- gkw_control(
#'   start = list(
#'     alpha = c(0.5, 0.2),
#'     beta = c(1.0, -0.3)
#'   )
#' )
#'
#' # Configure Nelder-Mead with custom reflection/contraction
#' ctrl_nm <- gkw_control(
#'   method = "Nelder-Mead",
#'   alpha = 1.5,
#'   beta = 0.75
#' )
#'
#' # Configure L-BFGS-B for bounded optimization
#' ctrl_lbfgsb <- gkw_control(
#'   method = "L-BFGS-B",
#'   factr = 1e6,
#'   lmm = 10
#' )
#'
#' # Configure SANN for rough surfaces
#' ctrl_sann <- gkw_control(
#'   method = "SANN",
#'   temp = 20,
#'   tmax = 20,
#'   maxit = 20000
#' )
#' }
#'
#' @seealso
#' \code{\link{gkwreg}} for the main fitting function,
#' \code{\link[stats]{nlminb}}, \code{\link[stats]{optim}} for optimizer details,
#' \code{\link[betareg]{betareg.control}} for similar design pattern.
#'
#' @references
#' Nocedal, J., & Wright, S. J. (2006). \emph{Numerical Optimization} (2nd ed.).
#' Springer.
#'
#' Belisle, C. J. P. (1992). Convergence theorems for a class of simulated
#' annealing algorithms on R^d. \emph{Journal of Applied Probability}, 29, 885-895.
#'
#' Byrd, R. H., Lu, P., Nocedal, J. and Zhu, C. (1995). A limited memory
#' algorithm for bound constrained optimization.
#' \emph{SIAM Journal on Scientific Computing}, 16, 1190-1208.
#'
#' @author Lopes, J. E.
#'
#' @export
gkw_control <- function(
  method = c("nlminb", "BFGS", "Nelder-Mead", "CG", "SANN", "L-BFGS-B"),
  start = NULL,
  fixed = NULL,
  hessian = TRUE,
  maxit = 500,
  reltol = sqrt(.Machine$double.eps),
  abstol = 0,
  trace = 0,
  silent = TRUE,
  # nlminb-specific parameters
  eval.max = 500,
  iter.max = 300,
  step.min = 1e-8,
  step.max = 1,
  x.tol = 1.5e-8,
  rel.tol = sqrt(.Machine$double.eps),
  # Nelder-Mead specific parameters
  alpha = 1.0,
  beta = 0.5,
  gamma = 2.0,
  warn.1d.NelderMead = TRUE,
  # CG specific parameters
  type = 1,
  # SANN specific parameters
  temp = 10,
  tmax = 10,
  # L-BFGS-B specific parameters
  lmm = 5,
  factr = 1e7,
  pgtol = 0,
  # BFGS and L-BFGS-B specific
  REPORT = NULL,
  # General optim parameters
  fnscale = 1,
  parscale = NULL,
  ndeps = NULL,
  ...
) {
  # ARGUMENT VALIDATION

  # Match method argument
  method <- match.arg(method)

  # Adjust maxit default for SANN if not explicitly set
  if (method == "SANN" && missing(maxit)) {
    maxit <- 10000
  }

  # Validate numeric parameters
  if (!is.numeric(maxit) || length(maxit) != 1 || maxit <= 0 ||
    maxit != round(maxit)) {
    stop("'maxit' must be a single positive integer", call. = FALSE)
  }

  if (!is.numeric(reltol) || length(reltol) != 1 || reltol < 0) {
    stop("'reltol' must be a single non-negative numeric value", call. = FALSE)
  }

  if (!is.numeric(abstol) || length(abstol) != 1 || abstol < 0) {
    stop("'abstol' must be a single non-negative numeric value", call. = FALSE)
  }

  if (!is.numeric(trace) || length(trace) != 1 || trace < 0 ||
    trace != round(trace)) {
    stop("'trace' must be a single non-negative integer", call. = FALSE)
  }

  # Validate logical parameters
  if (!is.logical(hessian) || length(hessian) != 1 || is.na(hessian)) {
    stop("'hessian' must be a single logical value (TRUE or FALSE)",
      call. = FALSE
    )
  }

  if (!is.logical(silent) || length(silent) != 1 || is.na(silent)) {
    stop("'silent' must be a single logical value (TRUE or FALSE)",
      call. = FALSE
    )
  }

  # Validate start if provided
  if (!is.null(start)) {
    if (!is.list(start)) {
      stop("'start' must be NULL or a named list", call. = FALSE)
    }
    if (is.null(names(start)) || any(names(start) == "")) {
      stop("'start' must be a named list with parameter names", call. = FALSE)
    }
  }

  # Validate fixed if provided
  if (!is.null(fixed)) {
    if (!is.list(fixed)) {
      stop("'fixed' must be NULL or a named list", call. = FALSE)
    }
  }

  # Validate nlminb-specific parameters
  if (!is.numeric(eval.max) || length(eval.max) != 1 || eval.max <= 0 ||
    eval.max != round(eval.max)) {
    stop("'eval.max' must be a single positive integer", call. = FALSE)
  }

  if (!is.numeric(iter.max) || length(iter.max) != 1 || iter.max <= 0 ||
    iter.max != round(iter.max)) {
    stop("'iter.max' must be a single positive integer", call. = FALSE)
  }

  if (!is.numeric(step.min) || length(step.min) != 1 || step.min <= 0) {
    stop("'step.min' must be a single positive numeric value", call. = FALSE)
  }

  if (!is.numeric(step.max) || length(step.max) != 1 || step.max <= 0) {
    stop("'step.max' must be a single positive numeric value", call. = FALSE)
  }

  if (!is.numeric(x.tol) || length(x.tol) != 1 || x.tol < 0) {
    stop("'x.tol' must be a single non-negative numeric value", call. = FALSE)
  }

  if (!is.numeric(rel.tol) || length(rel.tol) != 1 || rel.tol < 0) {
    stop("'rel.tol' must be a single non-negative numeric value", call. = FALSE)
  }

  # Validate method-specific parameters

  # Nelder-Mead parameters
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0) {
    stop("'alpha' must be a single positive numeric value", call. = FALSE)
  }

  if (!is.numeric(beta) || length(beta) != 1 || beta <= 0 || beta >= 1) {
    stop("'beta' must be a single numeric value between 0 and 1", call. = FALSE)
  }

  if (!is.numeric(gamma) || length(gamma) != 1 || gamma <= 1) {
    stop("'gamma' must be a single numeric value greater than 1", call. = FALSE)
  }

  if (!is.logical(warn.1d.NelderMead) || length(warn.1d.NelderMead) != 1) {
    stop("'warn.1d.NelderMead' must be a single logical value", call. = FALSE)
  }

  # CG parameters
  if (!is.numeric(type) || length(type) != 1 || !(type %in% c(1, 2, 3))) {
    stop("'type' must be 1, 2, or 3", call. = FALSE)
  }

  # SANN parameters
  if (!is.numeric(temp) || length(temp) != 1 || temp <= 0) {
    stop("'temp' must be a single positive numeric value", call. = FALSE)
  }

  if (!is.numeric(tmax) || length(tmax) != 1 || tmax <= 0 || tmax != round(tmax)) {
    stop("'tmax' must be a single positive integer", call. = FALSE)
  }

  # L-BFGS-B parameters
  if (!is.numeric(lmm) || length(lmm) != 1 || lmm <= 0 || lmm != round(lmm)) {
    stop("'lmm' must be a single positive integer", call. = FALSE)
  }

  if (!is.numeric(factr) || length(factr) != 1 || factr <= 0) {
    stop("'factr' must be a single positive numeric value", call. = FALSE)
  }

  if (!is.numeric(pgtol) || length(pgtol) != 1 || pgtol < 0) {
    stop("'pgtol' must be a single non-negative numeric value", call. = FALSE)
  }

  # General optim parameters
  if (!is.numeric(fnscale) || length(fnscale) != 1) {
    stop("'fnscale' must be a single numeric value", call. = FALSE)
  }

  # Set REPORT defaults if not provided
  if (is.null(REPORT)) {
    REPORT <- switch(method,
      "BFGS" = 10,
      "L-BFGS-B" = 10,
      "SANN" = 100,
      NULL
    )
  }

  # BUILD OPTIMIZER-SPECIFIC CONTROL LISTS

  # Build control list for nlminb
  nlminb_control <- list(
    eval.max = as.integer(eval.max),
    iter.max = as.integer(iter.max),
    trace = if (silent) 0L else as.integer(trace),
    abs.tol = abstol,
    rel.tol = rel.tol,
    x.tol = x.tol,
    step.min = step.min,
    step.max = step.max
  )

  # Build method-specific control list for optim
  optim_control <- switch(method,
    "Nelder-Mead" = {
      ctrl <- list(
        maxit = as.integer(maxit),
        trace = if (silent) 0L else as.integer(trace),
        reltol = reltol,
        abstol = abstol,
        alpha = alpha,
        beta = beta,
        gamma = gamma,
        fnscale = fnscale,
        warn.1d.NelderMead = warn.1d.NelderMead
      )
      if (!is.null(parscale)) ctrl$parscale <- parscale
      if (!is.null(ndeps)) ctrl$ndeps <- ndeps
      if (!is.null(REPORT)) ctrl$REPORT <- as.integer(REPORT)
      ctrl
    },
    "BFGS" = {
      ctrl <- list(
        maxit = as.integer(maxit),
        trace = if (silent) 0L else as.integer(trace),
        reltol = reltol,
        abstol = abstol,
        fnscale = fnscale
      )
      if (!is.null(parscale)) ctrl$parscale <- parscale
      if (!is.null(ndeps)) ctrl$ndeps <- ndeps
      if (!is.null(REPORT)) ctrl$REPORT <- as.integer(REPORT)
      ctrl
    },
    "CG" = {
      ctrl <- list(
        type = as.integer(type),
        maxit = as.integer(maxit),
        trace = if (silent) 0L else as.integer(trace),
        reltol = reltol,
        abstol = abstol,
        fnscale = fnscale
      )
      if (!is.null(parscale)) ctrl$parscale <- parscale
      if (!is.null(ndeps)) ctrl$ndeps <- ndeps
      ctrl
    },
    "SANN" = {
      ctrl <- list(
        maxit = as.integer(maxit),
        trace = if (silent) 0L else as.integer(trace),
        temp = temp,
        tmax = as.integer(tmax),
        fnscale = fnscale
      )
      if (!is.null(parscale)) ctrl$parscale <- parscale
      if (!is.null(REPORT)) ctrl$REPORT <- as.integer(REPORT)
      ctrl
    },
    "L-BFGS-B" = {
      ctrl <- list(
        trace = if (silent) 0L else as.integer(trace),
        factr = factr,
        pgtol = pgtol,
        lmm = as.integer(lmm),
        fnscale = fnscale
      )
      # Note: L-BFGS-B does not use maxit, reltol, or abstol
      # It uses factr for convergence control instead
      if (!is.null(parscale)) ctrl$parscale <- parscale
      if (!is.null(ndeps)) ctrl$ndeps <- ndeps
      if (!is.null(REPORT)) ctrl$REPORT <- as.integer(REPORT)
      ctrl
    }
  )

  # Capture additional arguments
  extra_args <- list(...)

  # Merge extra arguments into appropriate control list
  if (method == "nlminb") {
    nlminb_control <- utils::modifyList(nlminb_control, extra_args)
  } else {
    # Only merge compatible extra arguments for each method
    optim_control <- utils::modifyList(optim_control, extra_args)
  }

  # CONSTRUCT CONTROL OBJECT
  control <- list(
    method = method,
    start = start,
    fixed = fixed,
    hessian = hessian,
    maxit = as.integer(maxit),
    reltol = reltol,
    abstol = abstol,
    trace = as.integer(trace),
    silent = silent,
    nlminb_control = nlminb_control,
    optim_control = optim_control,
    # Store method-specific parameters for reference
    method_params = switch(method,
      "Nelder-Mead" = list(alpha = alpha, beta = beta, gamma = gamma),
      "CG" = list(type = type),
      "SANN" = list(temp = temp, tmax = tmax),
      "L-BFGS-B" = list(lmm = lmm, factr = factr, pgtol = pgtol),
      "BFGS" = list(),
      "nlminb" = list()
    )
  )

  class(control) <- "gkw_control"
  return(control)
}


#' @rdname gkw_control
#' @param x An object of class \code{"gkw_control"}.
#' @export
print.gkw_control <- function(x, ...) {
  cat("Generalized Kumaraswamy Control Parameters\n")
  cat("===========================================\n\n")

  cat("Optimization:\n")
  cat("  Method:            ", x$method, "\n")

  # Print method-specific parameters
  if (x$method == "nlminb") {
    cat("  Max evaluations:   ", x$nlminb_control$eval.max, "\n")
    cat("  Max iterations:    ", x$nlminb_control$iter.max, "\n")
    cat("  Relative tolerance:", format(x$nlminb_control$rel.tol, scientific = TRUE), "\n")
    cat("  Absolute tolerance:", format(x$nlminb_control$abs.tol, scientific = TRUE), "\n")
  } else {
    # Print parameters based on method
    switch(x$method,
      "Nelder-Mead" = {
        cat("  Max iterations:    ", x$optim_control$maxit, "\n")
        cat("  Relative tolerance:", format(x$optim_control$reltol, scientific = TRUE), "\n")
        cat("  Absolute tolerance:", format(x$optim_control$abstol, scientific = TRUE), "\n")
        cat("  Alpha (reflection):", x$optim_control$alpha, "\n")
        cat("  Beta (contraction):", x$optim_control$beta, "\n")
        cat("  Gamma (expansion): ", x$optim_control$gamma, "\n")
      },
      "BFGS" = {
        cat("  Max iterations:    ", x$optim_control$maxit, "\n")
        cat("  Relative tolerance:", format(x$optim_control$reltol, scientific = TRUE), "\n")
        cat("  Absolute tolerance:", format(x$optim_control$abstol, scientific = TRUE), "\n")
      },
      "CG" = {
        cat("  Max iterations:    ", x$optim_control$maxit, "\n")
        cat("  Relative tolerance:", format(x$optim_control$reltol, scientific = TRUE), "\n")
        cat("  Absolute tolerance:", format(x$optim_control$abstol, scientific = TRUE), "\n")
        cat("  Update type:       ", c(
          "Fletcher-Reeves", "Polak-Ribiere",
          "Beale-Sorenson"
        )[x$optim_control$type], "\n")
      },
      "SANN" = {
        cat("  Max iterations:    ", x$optim_control$maxit, "\n")
        cat("  Temperature:       ", x$optim_control$temp, "\n")
        cat("  Iterations/temp:   ", x$optim_control$tmax, "\n")
      },
      "L-BFGS-B" = {
        cat("  Convergence factor:", format(x$optim_control$factr, scientific = TRUE), "\n")
        cat("  Proj. grad. tol:   ", format(x$optim_control$pgtol, scientific = TRUE), "\n")
        cat("  Memory (BFGS):     ", x$optim_control$lmm, "\n")
      }
    )
  }

  cat("\nOutput:\n")
  cat("  Compute Hessian:   ", x$hessian, "\n")
  cat("  Silent mode:       ", x$silent, "\n")
  if (!x$silent) {
    cat("  Trace level:       ", x$trace, "\n")
  }

  if (!is.null(x$start)) {
    cat("\nStarting values:\n")
    cat("  Custom values provided for:", paste(names(x$start), collapse = ", "), "\n")
  }

  if (!is.null(x$fixed)) {
    cat("\nFixed parameters:\n")
    cat("  Parameters held fixed:", paste(names(x$fixed), collapse = ", "), "\n")
  }

  invisible(x)
}
