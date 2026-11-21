library(CIMEHR)
pairwise_solver <- get('.cimehr_updated', asNamespace('CIMEHR'))$.lu_fit_pairwise

local({
  state <- new.env(parent = emptyenv())
  state$mode <- 'quadratic'
  state$H <- diag(c(2, 3, 4))
  state$target <- c(.3, -.2, .7)
  state$force_zero <- integer()
  state$anchor <- NULL
  state$penalty <- 0
  state$offset <- 500

  test_objective <- function(par, mu, s2, XO, ZO, counts, singleton,
                             case_weights, s, include_delta, scores,
                             scalar_bvn, nm) {
    weight <- sum(counts) + sum(case_weights[singleton])
    z <- par - state$target
    gradient <- drop(state$H %*% z)
    objective <- drop(crossprod(z, state$H %*% z)) / 2
    if (state$mode %in% c('double_well', 'flat_hessian', 'linear')) {
      gradient <- z
      objective <- sum(z[-1L]^2) / 2
      x <- par[1L]
      if (state$mode == 'double_well') {
        gradient[1L] <- x^3 - x
        objective <- objective + x^4 / 4 - x^2 / 2
      } else if (state$mode == 'flat_hessian') {
        gradient[1L] <- max(x - .25, 0) - 1
        objective <- objective + max(x - .25, 0)^2 / 2 - x
      } else {
        gradient[1L] <- -.001
        objective <- objective - .001 * x
      }
    }
    value <- state$offset + weight * objective
    changed <- !is.null(state$anchor) &&
      any(par != state$anchor)
    if (state$mode == 'penalty' && changed)
      value <- value + state$penalty
    if (state$mode == 'nonfinite' && changed) {
      value <- Inf
      gradient[] <- NA_real_
    }
    n <- nrow(XO)
    list(value = value, gradient = setNames(weight * gradient, nm),
         subject_score = if (scores)
           matrix(rep(-weight * gradient / n, each = n), n,
                  dimnames = list(NULL, nm)) else NULL,
         subject_loglik = rep(-value / n, n))
  }
  premature_optimizer <- function(start, objective, gradient, lower, control) {
    if (length(start) == length(state$target) && length(state$force_zero))
      start[state$force_zero] <- 0
    list(par = start, objective = objective(start), convergence = 0L,
         message = 'Controlled premature stop for polishing regression test')
  }
  replace_optimizer <- function(x) {
    if (is.call(x) && identical(x[[1L]], quote(stats::nlminb)))
      x[[1L]] <- as.name('premature_optimizer')
    if (is.call(x)) for (i in seq_along(x)) {
      if (identical(x[[i]], quote(expr = ))) next
      x[i] <- list(replace_optimizer(x[[i]]))
    }
    x
  }
  fit <- pairwise_solver
  e <- new.env(parent = environment(fit))
  e$cimehr_pairwise_evaluate <- test_objective
  e$premature_optimizer <- premature_optimizer
  environment(fit) <- e
  body(fit) <- replace_optimizer(body(fit))
  XO <- matrix(1, 6, 1, dimnames = list(NULL, 'Intercept'))
  arguments <- list(XO = XO, ZO = XO, m = rep(3, 6),
    s = c(1, 2, 1, 2, 1, 2),
    posterior = list(mu = seq(-1, 1, length.out = 6), s2 = rep(.5, 6)),
    control = list(score_tol = 1e-10), include_delta = TRUE)
  run <- function(start, ...) do.call(fit, c(arguments, list(start = start, ...)))
  solved <- function(ans) {
    stopifnot(ans$converged, is.finite(ans$diagnostics$objective),
              ans$diagnostics$score_tolerance == 1e-10,
              ans$diagnostics$scaled_kkt_error <= 1e-10,
              all(is.finite(ans$par)), all(ans$sigma2_q >= 0))
    invisible(ans)
  }
  no_polish <- function(ans) {
    stopifnot(ans$diagnostics$newton_polish_steps == 0L,
              ans$diagnostics$projected_gradient_polish_steps == 0L,
              ans$diagnostics$roundoff_polish_steps == 0L)
  }

  # Converged interior and boundary optima.
  for (at_boundary in c(FALSE, TRUE)) {
    state$target <- if (at_boundary) c(.3, -.2, -.5) else c(.3, -.2, .7)
    start <- if (at_boundary) c(.3, -.2, 0) else state$target
    ans <- run(start)
    solved(ans)
    no_polish(ans)
    stopifnot(identical(unname(ans$par), start))
  }

  # Polishing at a constrained optimum.
  state$target <- c(.3, -.2, -.5)
  ans <- run(c(.3 + 1e-5, -.2 - 1e-5, 0))
  solved(ans)
  stopifnot(ans$par[3L] == 0, ans$diagnostics$newton_polish_steps > 0L)

  # Boundary release with a negative gradient.
  state$target <- c(.3, -.2, .7)
  state$force_zero <- 3L
  ans <- run(c(.3 + 1e-7, -.2 - 1e-7, 0))
  solved(ans)
  stopifnot(ans$identified, abs(ans$par[3L] - .7) < 1e-10)
  state$force_zero <- integer()

  # Coupled Newton directions at the boundary.
  state$H <- matrix(c(1, 0, 2, 0, 1, 0, 2, 0, 5), 3)
  state$target <- c(48, 0, -19)
  ans <- run(c(0, 0, 0))
  solved(ans)
  stopifnot(abs(ans$par[1L] - 10) < 1e-8, ans$par[3L] == 0)

  # Indefinite Hessian and an uphill Newton direction.
  state$H <- diag(c(2, 3, 4))
  state$target <- c(0, -.2, .7)
  state$mode <- 'double_well'
  first <- .1 - (.1^3 - .1)
  stopifnot(first^4 / 4 - first^2 / 2 < .1^4 / 4 - .1^2 / 2,
            abs(first^3 - first) > abs(.1^3 - .1))
  ans <- run(c(.1, -.2, .7))
  solved(ans)
  stopifnot(abs(ans$par[1L] - 1) < 1e-8,
            ans$diagnostics$projected_gradient_polish_steps > 0L)

  # Zero curvature in a locally linear region.
  state$mode <- 'flat_hessian'
  ans <- run(c(0, -.2, .7))
  solved(ans)
  stopifnot(ans$identified, abs(ans$par[1L] - 1.25) < 1e-8,
            ans$diagnostics$projected_gradient_polish_steps > 0L)

  # Rounded objective near stationarity.
  state$mode <- 'quadratic'
  state$target <- c(.3, -.2, .7)
  ans <- run(state$target + c(2e-9, -2e-9, 1e-9))
  solved(ans)
  stopifnot(ans$diagnostics$newton_polish_steps == 1L,
            ans$diagnostics$projected_gradient_polish_steps == 0L,
            ans$diagnostics$roundoff_polish_steps == 0L)

  # Material objective increase.
  state$mode <- 'penalty'
  state$anchor <- state$target + c(1e-4, 0, 0)
  state$penalty <- 10
  ans <- run(state$anchor)
  stopifnot(!ans$converged, identical(unname(ans$par), state$anchor))
  no_polish(ans)
  # Objective increase above relative and machine-precision bounds.
  state$anchor <- state$target + c(2e-9, 0, 0)
  state$penalty <- 1e-9
  ans <- run(state$anchor)
  stopifnot(!ans$converged, identical(unname(ans$par), state$anchor))
  no_polish(ans)
  # Objective increase within the relative tolerance.
  state$penalty <- 1e-10
  ans <- run(state$anchor)
  solved(ans)
  stopifnot(ans$diagnostics$newton_polish_steps == 1L,
            ans$diagnostics$roundoff_polish_steps == 0L)
  # Machine-precision allowance with a large total weight.
  state$offset <- 0
  state$penalty <- 2e-12
  ans <- run(state$anchor, case_weights = rep(200, 6))
  solved(ans)
  stopifnot(ans$diagnostics$total_composite_weight == 1800,
            ans$diagnostics$newton_polish_steps == 1L,
            ans$diagnostics$roundoff_polish_steps == 1L)
  # Objective increase above the machine-precision bound.
  state$penalty <- 4e-12
  ans <- run(state$anchor, case_weights = rep(200, 6))
  stopifnot(!ans$converged, identical(unname(ans$par), state$anchor))
  no_polish(ans)
  state$offset <- 500

  # Non-finite trial objectives and gradients.
  state$mode <- 'nonfinite'
  state$anchor <- state$target + c(1e-4, 0, 0)
  ans <- run(state$anchor)
  stopifnot(!ans$converged, identical(unname(ans$par), state$anchor))
  no_polish(ans)
  state$anchor <- NULL

  # Iteration limit for an objective without a stationary point.
  state$mode <- 'linear'
  ans <- run(c(.3, -.2, .7))
  stopifnot(!ans$converged, ans$diagnostics$score_tolerance == 1e-10,
            ans$diagnostics$scaled_kkt_error > 1e-10,
            ans$diagnostics$projected_gradient_polish_steps == 50L,
            ans$diagnostics$newton_polish_steps == 0L)
})
