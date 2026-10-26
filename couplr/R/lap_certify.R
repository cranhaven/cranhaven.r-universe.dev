# ==============================================================================
# Optimality certificates
# ==============================================================================
# A certificate is checkable, so its producer never has to be trusted. Given a
# matching M and any candidate duals u, v, the conditions below either prove M
# optimal for the complete problem or fail. Duals invented by a broken solver
# fail. That is why verify_assignment() accepts duals from the same solver that
# produced the matching without circularity: they are input to a check, not an
# answer.
#
# For the rectangular minimum-cost assignment LP with every row matched
#
#   min  sum_ij c_ij x_ij
#   s.t. sum_j x_ij  = 1  for every row i
#        sum_i x_ij <= 1  for every column j
#        x >= 0
#
# the dual is
#
#   max  sum_i u_i + sum_j v_j
#   s.t. u_i + v_j <= c_ij  for every admissible pair
#        v_j <= 0           when there are more columns than rows
#
# and optimality follows from primal feasibility, dual feasibility, and
# complementary slackness in both halves: matched arcs tight, and v_j = 0 on
# every column no row matched.
#
# Each of those conditions is the sign of c_ij - u_i - v_j, and a double is a
# rational number, so each has an exact answer. The check evaluates that sign
# exactly rather than reading the sign of a rounded difference, and the
# certificate says which arithmetic its conclusion is in. The tolerance is what
# remains for the case the exact conditions do not hold: duals that are optimal
# for the matrix in the reals but miss exact tightness by the rounding of the
# arithmetic that produced them.
#
# The sign condition is conditional because the dual objective sums v over all
# columns while a feasible assignment only pays for the ones it uses. With more
# columns than rows any column can be left out, so none may contribute a
# positive term; with as many rows as columns every assignment uses every
# column, the two sums coincide, and v is unrestricted. Jonker-Volgenant returns
# free-sign duals on a square problem, and they are correct there.
#
# The second half is not optional. A verifier checking only dual feasibility and
# tightness on matched arcs certifies a solution whose dual bound equals the
# true optimum while its primal cost sits above it, because a freed column
# carries v_j < 0.
# ==============================================================================

#' Verify that an assignment is optimal
#'
#' Checks a solved assignment against the linear-programming optimality
#' conditions and returns the result of each check. Unlike the `status` field on
#' a solve result, which records what the solver terminated on, this is a
#' statement about the matching: `certified_optimal` is `TRUE` only when every
#' condition holds.
#'
#' # What the certificate proves
#'
#' The conditions are decided in one of two arithmetics, and the certificate
#' says which one it used.
#'
#' An **exact certificate** decides every condition in exact arithmetic, with no
#' tolerance anywhere. It proves that the matching attains the minimum total
#' cost of the cost matrix as supplied. The three quantities in each condition
#' are IEEE doubles, and a double is a rational number, so `c_ij - u_i - v_j`
#' has an exact sign; the check evaluates that sign exactly rather than reading
#' the sign of a rounded difference. A cost matrix that is itself a rounding of
#' something else — Mahalanobis distances, say — is still certified as the
#' matrix it is, which is the problem the solver was given.
#'
#' A **numerical certificate** decides the same conditions within `tol`. It
#' establishes optimality up to that tolerance and no further. It is what is
#' available when the potentials are not exactly optimal for the matrix: a
#' potential computed as `c_ij - v_j` misses exact tightness by the rounding of
#' that subtraction, and a matched arc off by one unit in the last place is
#' enough to put the exact conclusion out of reach.
#'
#' The exact certificate does not depend on the solver's potentials being
#' exact. Optimal duals are sums and differences of cost entries, and a double
#' often cannot hold one: on computed costs such as Euclidean or Mahalanobis
#' distances the solver's potentials miss exact tightness on some matched arc
#' by a unit in the last place. When the duals given pass the numerical
#' reading but not the exact one, exact potentials are recovered from the
#' matching itself, as shortest-path distances over the cost entries held in
#' exact multi-component arithmetic (Shewchuk 1997), and the conditions are
#' decided on those. A matching that is not optimal has no such potentials, so
#' the recovery cannot certify one; it finds the improving cycle or path
#' instead, and the numerical reading stands. Duals that fail even the
#' numerical reading certify nothing, and the check fails on them. The
#' recovery costs one pass over the admissible pairs.
#'
#' Exact potentials generally do not fit in one double each, so they are
#' returned as `exact_u` and `exact_v`, matrices with one row per potential
#' whose row sums are the exact values. Summing each row in rational
#' arithmetic (for example with `gmp::as.bigq`) reproduces them, and passing
#' them back as `duals` has the conditions checked again from the cost matrix
#' alone, so the certificate can be re-checked without trusting this code.
#'
#' The check needs dual variables. If `x` carries them (as
#' [assignment_duals()] results do), they are used. Otherwise they are obtained
#' by solving `cost` with [assignment_duals()], which costs a second solve.
#' Either way the duals are verified, not trusted: dual feasibility is checked
#' over every admissible pair, so duals that do not certify anything cause the
#' verification to fail rather than pass.
#'
#' Optimal duals are shared by all optimal solutions of a linear program, so a
#' matching from one solver can be certified against duals from another. That is
#' what makes it possible to certify solvers that return no duals of their own.
#'
#' @param x A `lap_solve_result` (from [assignment()]), an
#'   `assignment_duals_result` (from [assignment_duals()]), or an integer vector
#'   of assigned columns per row with `0` for unmatched.
#' @param cost Numeric cost matrix the assignment was computed on, or a lazy
#'   cost specification from [compute_distances()]. Required unless `x` already
#'   carries one.
#' @param duals Optional list with elements `u` and `v` giving row and column
#'   potentials in the orientation of `cost`. Overrides any duals on `x`. Either
#'   element may be a matrix with one row per potential, as `exact_u` and
#'   `exact_v` are, in which case each potential is the exact sum of its row.
#' @param maximize Logical; whether the assignment maximized rather than
#'   minimized. Defaults to `FALSE`.
#' @param tol Numeric tolerance for the feasibility and slackness comparisons of
#'   a numerical certificate, ignored by an exact one. The duality-gap
#'   comparison scales it by the magnitude of the objective, since an absolute
#'   tolerance on a sum of many terms is not reachable in double precision.
#' @param arithmetic One of `"auto"`, `"exact"` or `"double"`. `"auto"`, the
#'   default, reports the exact conclusion when the exact conditions hold and
#'   the numerical one otherwise, which is the strongest statement the instance
#'   supports; the exact conditions imply the numerical ones at any
#'   non-negative `tol`, so `"auto"` certifies nothing `"double"` would refuse.
#'   `"exact"` refuses to fall back, so `certified_optimal` is then `TRUE` only
#'   on an exact certificate. `"double"` decides everything within `tol`.
#'
#' @return An object of class `assignment_certificate`, a list with elements:
#' \itemize{
#'   \item `certified_optimal` — logical, the conclusion. `TRUE` only when
#'         every condition below holds.
#'   \item `arithmetic` — `"exact"` or `"double"`, the arithmetic the
#'         conclusion was reached in.
#'   \item `exact_certificate` — logical; whether the conditions hold in exact
#'         arithmetic. `arithmetic = "double"` does not ask the question, and
#'         reports `FALSE` here and in `exact_available`.
#'   \item `exact_available` — logical; whether the exact question was asked of
#'         every condition. It is `FALSE` under `arithmetic = "double"`, and on
#'         a certificate assembled from a scan that priced part of the problem
#'         against a tolerance rather than evaluating it, which is what the
#'         edge-generation loop hands back.
#'   \item `n_exact_violations`, `n_exact_untight` — pairs failing exact dual
#'         feasibility, and matched pairs not exactly tight, under the duals
#'         given. Potentials recovered from the matching prove the matching and
#'         leave these counts describing the duals that missed.
#'   \item `exact_duals_source` — which potentials decided the exact
#'         certificate: `"solver"` for the duals as supplied or solved,
#'         `"supplied"` for expansions passed in `duals`, `"recovered"` for
#'         potentials recovered from the matching, and `"none"` when no exact
#'         certificate was reached.
#'   \item `exact_u`, `exact_v` — on an exact certificate, the row and
#'         column potentials that decided it, one row per potential whose sum
#'         is its exact value (a single column when the solver's doubles
#'         decided it); `NULL` when there is no exact certificate.
#'   \item `structurally_valid_matching` — logical; no column claimed twice, no
#'         forbidden pair matched, no index out of range. Unmatched rows are
#'         permitted, so this holds for a partial matching.
#'   \item `all_rows_matched` — logical; every row of the short side holds a
#'         column.
#'   \item `primal_feasible` — logical; both of the two above. The primal
#'         constrains every row of the short side to hold exactly one pair, so
#'         a partial matching is a valid matching and not a feasible solution,
#'         and no conclusion rests on it. `primal_objective` is still reported
#'         for one, since an unmatched row costs nothing and leaves the sum
#'         meaningful.
#'   \item `dual_feasible` — logical; `c_ij - u_i - v_j >= -tol` over every
#'         admissible pair, and, when there are more columns than rows,
#'         `v_j <= tol` for every column.
#'   \item `complementary_slackness` — logical; both halves.
#'   \item `cs_matched_tight`, `cs_unmatched_free` — the two halves separately.
#'   \item `primal_objective`, `dual_objective`, `duality_gap` — numeric.
#'   \item `max_suboptimality` — numeric; the most any feasible solution can
#'         beat this one by, in the cost unit. It adds to the duality gap the
#'         slack the dual conditions were allowed: `n_rows` times the depth the
#'         reduced costs were permitted below zero, plus, where the sign
#'         condition applies, `n_cols` times the height the column duals were
#'         permitted above it, plus an envelope for each objective's own
#'         rounding. Compensated summation buys back the accumulation error
#'         rather than removing it, so each sum is charged
#'         `(2u + gamma_n^2)` times the sum of its terms' magnitudes and the
#'         assembly is rounded outward at every step. The number is an upper
#'         bound in double arithmetic and not an estimate of one. An exact
#'         certificate proves the matching optimal and reports zero. `NA` when
#'         `primal_feasible` is
#'         `FALSE`: the quantity is what a feasible solution can beat this
#'         one by, and there is no answer for a candidate that is not one.
#'   \item `certified_reduced_cost_floor` — numeric; the lower bound proved for
#'         the reduced cost of every admissible pair, the ones never evaluated
#'         included. Equal to `min_reduced_cost` when every pair was visited,
#'         and below it when a pruning pricer proved only its own threshold.
#'   \item `min_reduced_cost`, `worst_i`, `worst_j` — the most violated pair, if
#'         any.
#'   \item `max_matched_slack`, `max_v_unmatched`, `max_v` — the quantities the
#'         slackness and sign conditions bound.
#'   \item `n_matched`, `n_rows`, `n_cols`, `transposed`, `tolerance`.
#' }
#'
#' @seealso [assignment()], [assignment_duals()], [solver_status_values()]
#'
#' @examples
#' set.seed(1)
#' cost <- matrix(runif(100), 10, 10)
#' verify_assignment(assignment(cost), cost)
#'
#' # A rectangular problem, where the condition on unmatched columns bites.
#' # Passing the duals result reuses its duals instead of solving again.
#' rect <- matrix(runif(120), 6, 20)
#' verify_assignment(assignment_duals(rect), rect)
#'
#' # Integer costs carry an integer optimal dual solution, so the conditions
#' # hold with no tolerance and the certificate is exact.
#' int_cost <- matrix(sample(1:100, 64, replace = TRUE), 8, 8)
#' cert <- verify_assignment(assignment(int_cost), int_cost)
#' cert$arithmetic
#'
#' @export
verify_assignment <- function(x, cost = NULL, duals = NULL,
                              maximize = FALSE, tol = 1e-9,
                              arithmetic = c("auto", "exact", "double")) {
  arithmetic <- match.arg(arithmetic)
  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol < 0) {
    stop("`tol` must be a single non-negative number.", call. = FALSE)
  }
  if (!is.logical(maximize) || length(maximize) != 1L || is.na(maximize)) {
    stop("`maximize` must be TRUE or FALSE.", call. = FALSE)
  }

  match_vec <- .certify_extract_match(x)

  if (is.null(duals) && is.list(x) && !is.null(x[["u"]]) && !is.null(x[["v"]])) {
    duals <- list(u = as.numeric(x[["u"]]), v = as.numeric(x[["v"]]))
  }
  split <- .certify_split_duals(duals)
  duals <- split$duals
  exact <- split$exact

  if (is.null(cost)) {
    stop("`cost` is required: a certificate is a statement about a specific ",
         "cost matrix, and a solve result does not carry one.", call. = FALSE)
  }

  if (is_lazy_cost_spec(cost)) {
    return(.certify_lazy(cost, match_vec, duals, exact, maximize, tol,
                         arithmetic))
  }

  cost <- as.matrix(cost)
  if (!is.numeric(cost)) {
    stop("`cost` must be a numeric matrix, got ", typeof(cost), call. = FALSE)
  }
  n <- nrow(cost)
  m <- ncol(cost)
  if (n == 0L || m == 0L) {
    stop("Cost matrix must have at least one row and one column.", call. = FALSE)
  }
  if (length(match_vec) != n) {
    stop("`match` has length ", length(match_vec), " but `cost` has ", n,
         " rows.", call. = FALSE)
  }

  # The dual sign condition and the slackness condition on unmatched units both
  # attach to the side carrying the "at most once" constraint, which is the
  # long side. Normalize to rows <= columns so there is one set of conditions
  # to check rather than two mirror images of it.
  transposed <- n > m
  if (transposed) {
    inverted <- .certify_invert_match(match_vec, m)
    cost <- t(cost)
    match_vec <- inverted
    if (!is.null(duals)) duals <- list(u = duals$v, v = duals$u)
    if (!is.null(exact)) exact <- list(u = exact$v, v = exact$u)
  }

  if (is.null(duals)) {
    duals <- .certify_solve_duals(cost, maximize)
  }
  .certify_check_dual_lengths(duals, nrow(cost), ncol(cost))

  report <- lap_certify_dense(cost, as.integer(match_vec),
                              as.numeric(duals$u), as.numeric(duals$v),
                              maximize, tol, arithmetic, exact)
  .new_assignment_certificate(report, transposed = transposed, tol = tol)
}

.certify_lazy <- function(cost, match_vec, duals, exact, maximize, tol,
                          arithmetic) {
  n <- cost$n_left
  m <- cost$n_right
  if (length(match_vec) != n) {
    stop("`match` has length ", length(match_vec), " but the specification has ",
         n, " left units.", call. = FALSE)
  }

  # Same normalization as the dense path, for the same reason: the sign and
  # slackness conditions attach to the long side, so put it on the columns.
  transposed <- n > m
  if (transposed) {
    match_vec <- .certify_invert_match(match_vec, m)
    cost <- transpose_lazy_cost_spec(cost)
    if (!is.null(duals)) duals <- list(u = duals$v, v = duals$u)
    if (!is.null(exact)) exact <- list(u = exact$v, v = exact$u)
  }

  if (is.null(duals)) {
    duals <- .certify_solve_duals(cost, maximize)
  }
  .certify_check_dual_lengths(duals, cost$n_left, cost$n_right)

  inv_cov <- lazy_cost_spec_inv_cov(cost)
  caliper_list <- lazy_cost_spec_calipers(cost)
  report <- lap_certify_lazy(cost$left_mat, cost$right_mat, cost$distance,
                             inv_cov, cost$max_distance, caliper_list,
                             as.integer(match_vec),
                             as.numeric(duals$u), as.numeric(duals$v),
                             maximize, tol, arithmetic, exact)
  .new_assignment_certificate(report, transposed = transposed, tol = tol)
}

# Duals arrive as doubles, or as expansions: a matrix per side, one row per
# potential, whose row sums are the exact values. The certificate reads doubles
# for its numerical conclusion and expansions for its exact one, so a matrix is
# split into its rounded row sums and the matrix itself. A side given as a
# vector is its own one-column expansion.
.certify_split_duals <- function(duals) {
  if (is.null(duals) || !is.list(duals) ||
      !(is.matrix(duals$u) || is.matrix(duals$v))) {
    return(list(duals = duals, exact = NULL))
  }
  as_expansion <- function(x) {
    m <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
    storage.mode(m) <- "double"
    if (anyNA(m) || any(!is.finite(m))) {
      stop("`duals` must be finite.", call. = FALSE)
    }
    m
  }
  exact <- list(u = as_expansion(duals$u), v = as_expansion(duals$v))
  list(duals = list(u = rowSums(exact$u), v = rowSums(exact$v)), exact = exact)
}

.certify_extract_match <- function(x) {
  if (is.list(x) && !is.null(x$match)) {
    return(as.integer(x$match))
  }
  if (is.numeric(x) && is.null(dim(x))) {
    return(as.integer(x))
  }
  stop("`x` must be a solve result carrying a `match` element, or an integer ",
       "vector of assigned columns.", call. = FALSE)
}

# match_vec assigns a column to each row; produce the vector assigning a row to
# each column, which is the match of the transposed problem.
.certify_invert_match <- function(match_vec, n_cols) {
  inv <- integer(n_cols)
  for (i in seq_along(match_vec)) {
    j <- match_vec[i]
    if (!is.na(j) && j > 0L) {
      if (j > n_cols) {
        stop("`match` assigns column ", j, " but `cost` has only ", n_cols,
             " columns.", call. = FALSE)
      }
      inv[j] <- i
    }
  }
  inv
}

.certify_solve_duals <- function(cost, maximize) {
  d <- assignment_duals(cost, maximize = maximize)
  list(u = as.numeric(d$u), v = as.numeric(d$v))
}

.certify_check_dual_lengths <- function(duals, n, m) {
  if (!is.list(duals) || is.null(duals$u) || is.null(duals$v)) {
    stop("`duals` must be a list with elements `u` and `v`.", call. = FALSE)
  }
  if (length(duals$u) != n) {
    stop("`duals$u` has length ", length(duals$u), " but the problem has ", n,
         " rows.", call. = FALSE)
  }
  if (length(duals$v) != m) {
    stop("`duals$v` has length ", length(duals$v), " but the problem has ", m,
         " columns.", call. = FALSE)
  }
  invisible(TRUE)
}

.new_assignment_certificate <- function(report, transposed, tol) {
  # The C++ report indexes the worst violating pair from zero, and reports it as
  # -1 when there is none.
  report$worst_i <- if (is.null(report$worst_i) || report$worst_i < 0) 0L
                    else as.integer(report$worst_i) + 1L
  report$worst_j <- if (is.null(report$worst_j) || report$worst_j < 0) 0L
                    else as.integer(report$worst_j) + 1L
  report$transposed <- transposed
  report$tolerance <- tol
  if (transposed && !is.null(report$exact_u)) {
    swap <- report$exact_u
    report$exact_u <- report$exact_v
    report$exact_v <- swap
  }
  report$arithmetic <- if (isTRUE(report$conclusion_is_exact)) "exact" else "double"
  report$conclusion_is_exact <- NULL
  class(report) <- "assignment_certificate"
  report
}

#' @param x An `assignment_certificate` object.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
#' @method print assignment_certificate
#' @rdname verify_assignment
print.assignment_certificate <- function(x, ...) {
  flag <- function(ok) if (isTRUE(ok)) "TRUE " else "FALSE"

  cat("Assignment certificate\n")
  cat("======================\n\n")
  cat(sprintf("  primal_feasible          %s\n", flag(x$primal_feasible)))
  cat(sprintf("    valid matching         %s\n",
              flag(x$structurally_valid_matching)))
  cat(sprintf("    all rows matched       %s\n", flag(x$all_rows_matched)))
  cat(sprintf("  dual_feasible            %s\n", flag(x$dual_feasible)))
  cat(sprintf("  complementary_slackness  %s\n", flag(x$complementary_slackness)))
  cat(sprintf("    matched arcs tight     %s   (max slack %.3e)\n",
              flag(x$cs_matched_tight), x$max_matched_slack))
  cat(sprintf("    unmatched columns free %s   (max |v_j| %.3e)\n",
              flag(x$cs_unmatched_free), x$max_v_unmatched))
  cat(sprintf("  duality_gap              %.6e\n", x$duality_gap))
  if (is.na(x$max_suboptimality)) {
    cat("  max_suboptimality        not reported (primal not feasible)\n")
  } else {
    cat(sprintf("  max_suboptimality        %.6e\n", x$max_suboptimality))
  }
  cat(sprintf("  certified_optimal        %s\n", flag(x$certified_optimal)))
  if (identical(x$arithmetic, "exact")) {
    cat(sprintf("  arithmetic               exact, no tolerance (%s potentials)\n\n",
                if (is.null(x$exact_duals_source)) "solver"
                else x$exact_duals_source))
  } else {
    cat(sprintf("  arithmetic               double, tolerance %.1e\n\n",
                x$tolerance))
  }

  cat(sprintf("  primal objective  %.10g\n", x$primal_objective))
  cat(sprintf("  dual objective    %.10g\n", x$dual_objective))
  cat(sprintf("  matched           %d of %d rows, %d columns\n",
              x$n_matched, x$n_rows, x$n_cols))
  if (!isTRUE(x$dual_feasible) && !is.null(x$worst_i) && x$worst_i > 0) {
    cat(sprintf("  worst reduced cost %.3e at row %d, column %d\n",
                x$min_reduced_cost, x$worst_i, x$worst_j))
  }
  if (isTRUE(x$transposed)) {
    cat("  (checked on the transposed problem, rows <= columns)\n")
  }

  invisible(x)
}
