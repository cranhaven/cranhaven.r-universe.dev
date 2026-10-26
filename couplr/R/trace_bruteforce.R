# ==============================================================================
# Reference brute-force LAP solver with frame-by-frame trace
# ==============================================================================
# Mirrors src/solvers/solve_bruteforce.cpp: enumerate every column-permutation,
# compute the assignment cost, keep the best. O(n!) time. The production solver
# is reached via assignment(cost, method = "bruteforce") and is gated to n <= 8.
#
# This trace exists for one pedagogical reason: it makes O(n!) tangible. For
# n = 4 the animation walks through 24 permutations; for n = 6 it would have
# to walk 720 — which is exactly the point. couplr ships fast solvers because
# brute force does not scale, and watching the permutation counter is the
# clearest demonstration.
#
# Frame emission policy:
#   - n <= 4 : emit every permutation tried.
#   - n >= 5 : sample at regular intervals so total trial-frames cap near 60,
#              but always emit when a new best is found and always emit init +
#              final.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_bruteforce <- function(cost, maximize = FALSE, ...) {
  vc <- validate_square_cost(cost, "trace_bruteforce", maximize, solver_hint = "bruteforce")
  cost <- vc$cost; n <- vc$n; m <- vc$m
  if (n > 7L) {
    stop(
      "trace_bruteforce caps at n <= 7 because the animation would enumerate ",
      "n! = ", factorial(n), " permutations. Use a smaller matrix or a ",
      "different method to animate.",
      call. = FALSE
    )
  }

  cost_orig <- cost
  cost_signed <- vc$cost_signed
  finite_mask <- vc$finite_mask

  # Enumerate all permutations of 1..n lexicographically.
  all_perms <- function(x) {
    if (length(x) <= 1L) return(list(x))
    out <- vector("list", factorial(length(x)))
    k <- 0L
    for (i in seq_along(x)) {
      rest <- all_perms(x[-i])
      for (p in rest) {
        k <- k + 1L
        out[[k]] <- c(x[i], p)
      }
    }
    out
  }
  perms <- all_perms(seq_len(n))
  n_perms <- length(perms)

  # Emission sampling cap
  trial_cap <- 60L
  emit_every <- if (n_perms <= trial_cap) 1L
                else as.integer(ceiling(n_perms / trial_cap))

  frames <- list()
  step <- 0L

  best_perm <- NULL
  # Always minimize cost_signed: we negate at the end if maximize was requested.
  best_cost <- Inf
  feasible_count <- 0L

  emit <- function(phase, description, matching, active_edges = list(),
                   path = list()) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step, phase, description,
      matching     = matching,
      active_edges = active_edges,
      path         = path
    )
  }

  emit(
    "init",
    sprintf(
      paste0(
        "Brute force: enumerate every one of n! = %d column-permutations, ",
        "compute the assignment cost, keep the best. This is the algorithm ",
        "in its rawest form - guaranteed optimal, O(n!) time. For n = %d ",
        "there are %d permutations to try."
      ),
      n_perms, n, n_perms
    ),
    matching = integer(n)
  )

  for (k in seq_len(n_perms)) {
    perm <- perms[[k]]

    # Evaluate this permutation: cost[i, perm[i]] for all i
    rows <- seq_len(n)
    feasible <- all(finite_mask[cbind(rows, perm)])
    if (!feasible) {
      # Skip forbidden assignments silently unless we have to emit a frame.
      if (k %% emit_every == 0L) {
        emit(
          "trial_skip",
          sprintf(
            "Permutation %d / %d: skipped (forbidden edge in this matching).",
            k, n_perms
          ),
          matching = perm,
          active_edges = lapply(rows, function(i) c(i, perm[i]))
        )
      }
      next
    }

    this_cost_signed <- sum(cost_signed[cbind(rows, perm)])
    this_cost <- if (maximize) -this_cost_signed else this_cost_signed

    feasible_count <- feasible_count + 1L
    new_best <- this_cost_signed < best_cost

    if (new_best) {
      best_cost <- this_cost_signed
      best_perm <- perm
    }

    emit_now <- (k %% emit_every == 0L) || new_best || k == n_perms
    if (emit_now) {
      desc <- if (new_best) {
        sprintf(
          paste0(
            "Permutation %d / %d -> cost %.6g. New best! Previous best ",
            "now beaten."
          ),
          k, n_perms, this_cost
        )
      } else {
        best_so_far <- if (maximize) -best_cost else best_cost
        sprintf(
          paste0(
            "Permutation %d / %d -> cost %.6g. Worse than current best (%.6g)."
          ),
          k, n_perms, this_cost, best_so_far
        )
      }
      emit(
        if (new_best) "trial_new_best" else "trial",
        desc,
        matching = perm,
        active_edges = lapply(rows, function(i) c(i, perm[i])),
        path = if (new_best) lapply(rows, function(i) c(i, perm[i])) else list()
      )
    }
  }

  if (is.null(best_perm)) {
    stop("trace_bruteforce: no feasible permutation found.", call. = FALSE)
  }

  total <- if (maximize) -best_cost else best_cost
  rows <- seq_len(n)

  emit(
    "final",
    sprintf(
      paste0(
        "Done. Walked %d permutations, %d feasible. Best cost: %.6g. The ",
        "smart algorithms (Hungarian, JV, auction) find this same answer in ",
        "O(n^3) or faster - the brute-force trace is here so you can see ",
        "what they spare you."
      ),
      n_perms, feasible_count, total
    ),
    matching = best_perm,
    path = lapply(rows, function(i) c(i, best_perm[i]))
  )

  list(
    meta = make_meta(
      "bruteforce", n, m, cost_orig, maximize, total,
      description = paste0(
        "Brute force: enumerate every permutation of column assignments, ",
        "compute each one's total cost, keep the best. Trivially optimal, ",
        "trivially O(n!). Animated for the same reason a physics class ",
        "computes orbital mechanics by Euler's method before showing you a ",
        "symplectic integrator - it makes the cost of the naive approach ",
        "concrete."
      )
    ),
    frames = frames
  )
}

register_trace("bruteforce", trace_bruteforce)
