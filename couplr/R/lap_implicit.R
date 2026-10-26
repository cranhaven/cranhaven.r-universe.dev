# ==============================================================================
# Edge generation: the assignment over a complete implicit graph
# ==============================================================================
# The problem is stated over every pair and solved over a fraction of them. A
# restricted master is solved, its potentials are read as assignment duals, the
# pairs it omits are priced against them, the ones pricing below zero are added,
# and the master is re-solved from where it stopped. When nothing prices below
# zero the duals are feasible for every pair of the complete problem and the
# restricted answer is optimal for all of them. That last step is what separates
# this from approximate k-nearest matching, which solves the same restricted
# master and stops.
#
# The loop itself is C++ (src/flow/flow_implicit.h): it owns the compiled
# problem, the candidate set and the cost source, and crosses to R once rather
# than once per round. This file is the R surface -- the orientation the loop is
# handed, the answer read back in the caller's units, and the certificate and
# the search record put where a caller finds them.
#
# `certify` defaults to TRUE here and to FALSE everywhere else, and that is the
# one place a default differs across paths. On a complete solve a certificate is
# extra work over an answer already known to be optimal; here it is the only
# thing that distinguishes the answer from an approximate one, so an uncertified
# implicit solve is not something to hand back by default.
# ==============================================================================

# The search knobs, in one place, so the loop's C++ defaults and R's cannot
# drift apart in what they mean:
#
#   keep_per_row  violators a row contributes per pricing round
#   width         columns the first feasibility round gives a deficient row,
#                 or 0 to size the seed from the number of columns
#   tol           a pair prices out at cbar < -tol; a certifying loop prices at
#                 zero against exact potentials and reads it only in a round
#                 whose master the recovery finds not exactly optimal
#   max_rounds    guard, not a convergence bound
#
# They are not arguments of assignment() or match_couples(). The loop converges
# on any of them, so they stay reachable for the harnesses that measure them and
# off the front doors. `width` is the one with a rule behind it: the harnesses
# read the seed against problem size, variable count and covariate type, and
# implicit_seed_width() in src/flow/flow_implicit.h carries what they found. A
# run reports the width it used as `search$seed_width`.
.implicit_defaults <- function() {
  list(keep_per_row = 5, width = 0, tol = 1e-9, max_rounds = 60)
}

.check_positive_count <- function(x, what) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x != trunc(x)) {
    stop("`", what, "` must be a single whole number of at least 1.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# `width` alone takes 0, which asks the loop for the measured seed rather than
# naming one.
.check_seed_width <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x != trunc(x)) {
    stop("`width` must be a single whole number of at least 0, where 0 sizes ",
         "the seed from the problem.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Solve an implicit assignment by generating the pairs it needs
#'
#' The backend for `memory_mode = "implicit"`. Takes either a `lazy_cost_spec`,
#' which states the complete problem without building it, or a materialized
#' matrix, which states the same problem and lets the loop's answer be held
#' against a complete solve of the same numbers.
#'
#' @return A `lap_solve_result` carrying the duals the last master produced,
#'   the certificate for the complete problem, and `search`, the record of what
#'   the search cost.
#' @keywords internal
.assignment_implicit <- function(cost, maximize = FALSE, certify = TRUE,
                                 method = "auto",
                                 keep_per_row = .implicit_defaults()$keep_per_row,
                                 width = .implicit_defaults()$width,
                                 tol = .implicit_defaults()$tol,
                                 max_rounds = .implicit_defaults()$max_rounds) {
  if (!is.logical(maximize) || length(maximize) != 1L || is.na(maximize)) {
    stop("`maximize` must be TRUE or FALSE.", call. = FALSE)
  }
  # The loop is not one of the solvers `method` chooses between: it solves a
  # sequence of restricted masters with the flow model, and which solver runs
  # is not the caller's to name here. A named one is a request the path cannot
  # honour rather than one it can quietly ignore.
  if (length(method) == 1L && !identical(method, "auto")) {
    stop("method = \"", method, "\" does not apply under memory_mode = ",
         "\"implicit\": the restricted master is solved by the flow model, ",
         "and the loop around it is what the mode names.", call. = FALSE)
  }
  if (!is.logical(certify) || length(certify) != 1L || is.na(certify)) {
    stop("`certify` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol < 0) {
    stop("`tol` must be a single non-negative number.", call. = FALSE)
  }
  .check_positive_count(keep_per_row, "keep_per_row")
  .check_seed_width(width)
  .check_positive_count(max_rounds, "max_rounds")

  lazy <- is_lazy_cost_spec(cost)
  if (!lazy) {
    cost <- as.matrix(cost)
    if (!is.numeric(cost)) {
      stop("`cost` must be a numeric matrix, got ", typeof(cost), call. = FALSE)
    }
    if (any(is.nan(cost))) stop("NaN not allowed in `cost`")
  }

  n <- if (lazy) cost$n_left else nrow(cost)
  m <- if (lazy) cost$n_right else ncol(cost)
  if (n == 0 || m == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  # The loop matches every row, so the short side has to be the rows. This is
  # the same normalization assignment() applies to a dense problem, and for a
  # specification it is a field swap rather than a copy.
  transposed <- n > m
  work <- cost
  if (transposed) {
    work <- if (lazy) transpose_lazy_cost_spec(cost) else t(cost)
  }

  raw <- if (lazy) {
    lap_implicit_lazy(work$left_mat, work$right_mat, work$distance,
                      lazy_cost_spec_inv_cov(work), work$max_distance,
                      lazy_cost_spec_calipers(work), maximize, keep_per_row, width, tol, max_rounds, certify)
  } else {
    lap_implicit_dense(work, maximize, keep_per_row, width, tol, max_rounds,
                       certify)
  }

  .new_implicit_result(raw, n = n, transposed = transposed, tol = tol)
}

# Read the loop's answer back in the caller's orientation. On a transposed
# problem the match inverts into one column per original row and the two dual
# vectors swap sides, which is what .duals_result() does for the same reason.
.new_implicit_result <- function(raw, n, transposed, tol) {
  match_out <- as.integer(raw$match)
  if (transposed) {
    match_out <- .certify_invert_match(match_out, n)
  }
  duals <- .solver_duals(raw, transposed)

  certificate <- NULL
  if (!is.null(raw$certificate)) {
    certificate <- .new_assignment_certificate(raw$certificate,
                                               transposed = transposed,
                                               tol = tol)
  }

  out <- .new_lap_solve_result(
    match       = match_out,
    total_cost  = raw$total_cost,
    status      = raw$status,
    method_used = "implicit",
    dispatch    = list(
      rule = NA_character_, condition = NA_character_,
      reason = paste("edge generation over an implicit complete graph;",
                     "the restricted master is solved by the flow model"),
      explicit = TRUE),
    certificate = certificate
  )

  out$u <- duals$u
  out$v <- duals$v
  out$search <- list(
    seed_width      = as.integer(raw$seed_width),
    candidate_edges = as.numeric(raw$candidate_edges),
    possible_edges  = as.numeric(raw$possible_edges),
    edges_evaluated = as.numeric(raw$edges_evaluated),
    n_rounds        = as.integer(raw$n_rounds),
    rounds          = tibble::as_tibble(raw$rounds)
  )

  # Why no arc set over this source admits a complete matching. Its row and
  # column indices are the orientation the loop solved, which is the caller's
  # only when the problem was not transposed; the flag says which.
  if (!is.null(raw$witness)) {
    witness <- raw$witness
    witness$certified <- raw$witness_certified
    witness$transposed <- transposed
    out$witness <- witness
  }

  out
}

# The mode a lazy specification is solved in: what the caller asked assignment()
# for, and otherwise what the specification was built for. "auto" and "dense"
# both mean "however this specification was built" -- a materialized matrix is
# not something a specification can be turned back into.
.resolve_spec_mode <- function(memory_mode, spec) {
  requested <- if (length(memory_mode) > 1L) "auto" else memory_mode
  if (requested %in% c("lazy", "implicit")) {
    return(requested)
  }
  lazy_cost_spec_mode(spec)
}

# Whether to attach a checked certificate: the caller's answer when there is
# one, and otherwise the path's own default.
.resolve_certify <- function(certify, mode) {
  if (is.null(certify)) {
    return(identical(mode, "implicit"))
  }
  if (!is.logical(certify) || length(certify) != 1L || is.na(certify)) {
    stop("`certify` must be TRUE or FALSE.", call. = FALSE)
  }
  certify
}
