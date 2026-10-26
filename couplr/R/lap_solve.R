# ==============================================================================
# Core LAP Solver (Low-Level Interface)
# ==============================================================================
# This section contains assignment(), the low-level matrix-based LAP solver.
# For most users, prefer the tidy interface lap_solve() below.
# ==============================================================================
#' Linear assignment solver
#'
#' Solve the linear assignment problem (minimum- or maximum-cost matching)
#' using several algorithms. Forbidden edges can be marked as `NA` or `Inf`.
#'
#' @param cost Numeric matrix; rows = tasks, columns = agents. `NA` or `Inf`
#'   entries are treated as forbidden assignments.
#' @param maximize Logical; if `TRUE`, maximizes the total cost instead of minimizing.
#' @param method Character string indicating the algorithm to use. Options:
#'
#'   **General-purpose solvers:**
#'   \itemize{
#'     \item `"auto"` — Automatic selection based on problem characteristics (default)
#'     \item `"jv"` — 'Jonker-Volgenant', fast general-purpose O(n^3) with warm-start
#'     \item `"hungarian"` — Classic 'Hungarian' (shortest augmenting path) O(n^3)
#'     \item `"munkres"` — Matrix-form 'Kuhn-Munkres' O(n^4), reference implementation
#'   }
#'
#'   **Auction-based solvers:**
#'   \itemize{
#'     \item `"auction"` — 'Bertsekas' auction with adaptive epsilon
#'     \item `"auction_gs"` — 'Gauss-Seidel' variant, good for spatial structure
#'     \item `"auction_scaled"` — 'Epsilon-scaling' auction, bidding at a
#'       decreasing sequence of epsilon values. Not chosen by `"auto"`; a rule
#'       sending heavily tied costs to it was tested and did not meet its
#'       criterion, and it remains available by name for tied costs
#'   }
#'
#'   **Specialized solvers:**
#'   \itemize{
#'     \item `"sap"` — Shortest augmenting path over the shared flow model's
#'       network. `"ssp"` is accepted as a second spelling of this method and
#'       resolves to `"sap"`.
#'     \item `"sap_dense"` — Shortest augmenting path with a linear scan in place
#'       of a heap, O(n * m^2), suited to a dense cost matrix
#'     \item `"lapmod"` — 'Jonker-Volgenant' over the finite entries alone,
#'       stored row by row (CSR). Written for sparse input; see below for why
#'       `"auto"` does not divert sparse matrices to it
#'     \item `"hk01"` — 'Hopcroft-Karp' for binary (0/1) or constant costs.
#'       Constant costs make every perfect matching optimal. On a `{0,1}` matrix
#'       the search runs over the zero-cost edges alone, where a perfect matching
#'       totals zero and is therefore optimal; if none exists the problem is
#'       passed to the weighted solver on the original costs.
#'     \item `"ssap_bucket"` — 'Dial' algorithm for integer costs
#'     \item `"bruteforce"` — Exact enumeration for tiny problems (n <= 8)
#'   }
#'
#'   **Advanced solvers:**
#'   \itemize{
#'     \item `"csa"` — 'Goldberg-Kennedy' cost-scaling assignment (CSA-Q):
#'       epsilon starts at the span of the costs and is divided by 10 each
#'       refine, rows are discharged from a stack by double-push, and each row
#'       keeps its three cheapest arcs between scans (the fourth-best
#'       heuristic). Real-valued costs are read as supplied; the final
#'       epsilon-optimal assignment is repaired to an optimal one
#'     \item `"gabow_tarjan"` — 'Gabow-Tarjan' bit-scaling with complementary
#'       slackness. On a graph of `V` vertices and `E` edges the bound is
#'       O(sqrt(V) * E * log(V * C)), which for an `n` by `n` cost matrix is
#'       O(n^2.5 * log(n * C)). Its optimality bound holds for a matching that
#'       saturates both sides, so a rectangular problem gains a dummy side of
#'       zero cost. The dummies are copies of one node and are carried as a
#'       single unit holding as many partners as there are dummies, so the
#'       problem is solved at its own `n` by `m` shape.
#'     \item `"cycle_cancel"` — Cycle-canceling with 'Karp' algorithm
#'     \item `"csflow"` — Successive shortest paths with 'Johnson' potentials
#'     \item `"network_simplex"` — 'Network simplex' with spanning tree representation
#'     \item `"push_relabel"` — 'Goldberg-Tarjan' cost-scaling push-relabel
#'     \item `"ramshaw_tarjan"` — 'Ramshaw-Tarjan', optimized for rectangular matrices (n != m)
#'   }
#'
#'   One-dimensional problems have their own entry point,
#'   [lap_solve_line_metric()], which takes two point vectors rather than a
#'   cost matrix and runs in O(n log n).
#'
#'   Under `"auto"`, a single pass over `cost` supplies the facts the following
#'   rules need, and the first matching rule wins:
#'   \enumerate{
#'     \item at most 8 rows and 8 columns: `"bruteforce"`, exact and faster
#'       than setting up a general solver;
#'     \item finite entries all equal, or all either 0 or 1: `"hk01"`, which
#'       exploits the absence of a real cost scale;
#'     \item everything else: `"jv"`.
#'   }
#'   Sparsity and aspect ratio used to divert the choice to `"lapmod"` and
#'   `"sap"`. Neither beat `"jv"` on the regime grid, at any of the four
#'   admissibility densities or three aspect ratios measured, so both now fall
#'   through to it; each stays reachable by naming it.
#'
#'   Naming a method skips the pass. Rectangular problems are transposed
#'   internally so the solver always sees at least as many columns as rows, and
#'   the assignment is mapped back afterwards.
#' @param auction_eps Optional numeric epsilon for the 'Auction'/'Auction-GS' methods.
#'   If `NULL`, an internal default (e.g., `1e-9`) is used.
#' @param eps Deprecated. Use `auction_eps`. If provided and `auction_eps` is `NULL`,
#'   its value is used for `auction_eps`.
#' @param memory_mode One of "auto" (default), "dense", "lazy" or "implicit".
#'   `cost` is already a materialized matrix by the time it reaches
#'   `assignment()`, so "auto" here is diagnostic only: it warns if the matrix
#'   is large relative to free system RAM (nothing else can be done post-hoc
#'   once the matrix already exists -- build it via
#'   `compute_distances(memory_mode = ...)` instead to avoid materializing it in
#'   the first place). `"lazy"` and `"implicit"` describe how a cost source is
#'   read rather than how a matrix is stored, so they apply to a lazy cost
#'   specification; `"implicit"` also accepts a matrix, where it solves the same
#'   problem by generating the pairs it needs and saves nothing, which is what
#'   makes it a check on the complete solve rather than a faster one.
#'   Against `"lazy"` on a lazy cost specification, `"implicit"` leads from
#'   5,000 units upward on the eight-covariate problem the benchmarks use, by
#'   1.1x at 5,000 rising to 3.1x at 50,000, and loses below that where the
#'   loop's fixed costs are still visible. What it buys at every size is the
#'   certificate over the complete problem.
#' @param certify Logical; whether to attach a checked `assignment_certificate`
#'   as `certificate`. `NULL`, the default, takes the path's own answer: `TRUE`
#'   under `memory_mode = "implicit"`, where the certificate is what
#'   distinguishes the answer from an approximate one and the loop has already
#'   done most of the scan, and `FALSE` elsewhere. The check reads the duals
#'   the solve returned where it returned some (see `u` and `v` below), so it
#'   costs one pass over the admissible pairs; elsewhere it costs the solve
#'   [verify_assignment()] runs to get them. A solve that did not reach a
#'   complete optimal matching has nothing to certify and gets no certificate.
#' @param cardinality How many pairs to produce.
#'   \itemize{
#'     \item `"complete"` (default) — every row is matched; an input admitting
#'       no complete matching is an error.
#'     \item `"maximum"` — as many pairs as the admissible edges allow, and the
#'       cheapest total among matchings of that size.
#'     \item `"fixed"` — exactly `n_matches` pairs, chosen to minimize total
#'       cost.
#'   }
#'   All three are solved exactly by the same solver: the two non-complete modes
#'   append dummy columns priced so that the solver's own optimum is the
#'   requested objective.
#' @param n_matches Integer; the number of pairs to produce. Required when
#'   `cardinality = "fixed"`, and not accepted otherwise.
#' @param unmatched_penalty Numeric; the cost charged for leaving one row
#'   unmatched, under `cardinality = "maximum"`. Supplying it replaces the
#'   lexicographic objective with a single one: a pair costing more than the
#'   penalty is worth dropping. Left `NULL`, no pair is ever traded away for a
#'   cost saving.
#'
#' @return An object of class `lap_solve_result`, a list with elements:
#' \itemize{
#'   \item `match` — integer vector of length `min(nrow(cost), ncol(cost))`
#'         giving the assigned column for each row (0 if unassigned).
#'   \item `total_cost` — numeric scalar, the objective value.
#'   \item `status` — character scalar drawn from [solver_status_values()],
#'         computed from what the solver terminated on. `"optimal"` means the
#'         solver reached its own optimality condition with every row matched;
#'         it is not a checked proof. Use [verify_assignment()] for that.
#'   \item `method_used` — character scalar, the algorithm actually used.
#'   \item `dispatch` — list recording how `method` was chosen: the rule that
#'         fired under `"auto"`, the condition that triggered it, and whether
#'         the method was named explicitly. See [explain_dispatch()].
#'   \item `certificate` — an `assignment_certificate`, present when one was
#'         checked. See `certify`.
#'   \item `u`, `v` — row and column duals in the orientation of `cost`,
#'         present when the solver produced optimal duals for `cost`: `"jv"`
#'         (and so `"auto"` on every problem it sends there), `"hungarian"`,
#'         the lazy `"jv"` path, and `memory_mode = "implicit"`, where they are
#'         the duals the last restricted master produced. [verify_assignment()]
#'         reads them instead of solving again. Absent under
#'         `cardinality = "maximum"` and `"fixed"`, whose solver duals belong to
#'         the padded problem rather than to `cost`, and for `"gabow_tarjan"`,
#'         whose duals are those of its scaled integer instance.
#' }
#' Under `memory_mode = "implicit"` the result also carries `search`: the columns the
#' first round gave each row (`seed_width`), the pairs the candidate set ended
#' up holding (`candidate_edges`) out of `possible_edges`, the pairs a cost was
#' computed for (`edges_evaluated`), the round count, and `rounds`, one row per
#' round of what the master held, what priced out and what each step cost.
#'
#' @details
#' `method = "auto"` selects an algorithm based on problem size and the costs:
#' \itemize{
#'   \item Very small (n <= 8 and m <= 8): `"bruteforce"` — exact enumeration
#'   \item Binary/constant costs: `"hk01"` — specialized for 0/1 costs
#'   \item Otherwise: `"jv"`
#' }
#' [explain_dispatch()] reports which rule fired and why. The other solvers are
#' available by naming them explicitly.
#'
#' @section Integer conversion for bit-scaling:
#'
#' `"gabow_tarjan"` is a bit-scaling algorithm and runs on integer costs. The
#' conversion is part of the method, so the rule it follows and what it claims
#' afterwards are both properties of the solve rather than of the caller's
#' preparation.
#'
#' Finite costs are shifted so the smallest is zero, multiplied by a scale
#' factor `s`, and rounded to the nearest integer. `s` is set so that
#' `K * s * (max - min)` stays at or below `10^13`, where `K` is `n + 1` on a
#' square problem and `2 * min(n, m) + 1` on a rectangular one. `K` is the
#' separation the algorithm needs between the optimum and a 1-optimal matching,
#' and the bound holds every scaled cost clear of the sentinel that marks a
#' forbidden pair while keeping the path sums the solver forms inside 64-bit
#' arithmetic. Forbidden pairs take the sentinel and take part in neither the
#' range nor the conversion.
#'
#' A matrix whose finite entries are each within `1e-9` of an integer is taken
#' as an integer matrix: `s` is one, the shift is an integer, and every cost
#' reaches the solver as the integer nearest to it. Where those entries are
#' integers the conversion is exact and the optimum is the optimum of the
#' instance as supplied; where they are merely near one, each cost moves by at
#' most `1e-9`, so the matching returned costs at most `2 * min(n, m) * 1e-9`
#' more than that optimum. Such a matrix is refused when its range exceeds
#' `1.25 * 10^14 / K`, since a scale of one is the only one available and no
#' choice brings the instance inside the bound; the error names the limit and
#' points at `"jv"` or `"auction"`.
#'
#' Costs that are not integers are solved on the rounded instance. Rounding
#' moves each cost by at most `1 / (2 * s)`, so the matching returned costs at
#' most `min(n, m) * K * (max - min) / 10^13` more than the optimum of the
#' matrix as supplied. On a square problem that is about
#' `n^2 * (max - min) / 10^13`, which is `10^-7` of the range at `n = 1000` and
#' `10^-5` of it at `n = 10000`. Duals are divided by `s` and shifted back, so
#' they belong to the original matrix; whether the matching is optimal for that
#' matrix and not only for the rounded one is a question
#' [verify_assignment()] answers rather than one this bound settles.
#'
#' @seealso
#' \itemize{
#'   \item [lap_solve()] — Tidy interface returning tibbles
#'   \item [lap_solve_kbest()] — Find k-best assignments ('Murty' algorithm)
#'   \item [assignment_duals()] — Extract dual variables for sensitivity analysis
#'   \item [bottleneck_assignment()] — Minimize maximum edge cost (minimax)
#'   \item [sinkhorn()] — Entropy-regularized optimal transport
#' }
#'
#' @examples
#' cost <- matrix(c(4,2,5, 3,3,6, 7,5,4), nrow = 3, byrow = TRUE)
#' res  <- assignment(cost)
#' res$match; res$total_cost
#'
#' @export
assignment <- function(cost, maximize = FALSE,
                       method = c("auto","jv","hungarian","munkres","auction","auction_gs","auction_scaled",
                                  "sap","ssp","sap_dense","csflow","hk01","bruteforce",
                                  "ssap_bucket","cycle_cancel","gabow_tarjan","lapmod","csa",
                                  "ramshaw_tarjan","push_relabel","network_simplex"),
                       auction_eps = NULL, eps = NULL, memory_mode = "auto",
                       certify = NULL,
                       cardinality = c("complete", "maximum", "fixed"),
                       n_matches = NULL, unmatched_penalty = NULL
                       # , auction_schedule = c("alpha7","pow2","halves"),  # optional (see below)
                       # , auction_final_eps = NULL                          # optional (see below)
                       ) {
  if (is_lazy_cost_spec(cost)) {
    if (!identical(cardinality, c("complete", "maximum", "fixed")) &&
        !identical(cardinality, "complete")) {
      stop("cardinality = \"", cardinality[1],
           "\" needs a materialized cost matrix; use memory_mode = \"dense\".",
           call. = FALSE)
    }
    if (!is.null(eps) && is.null(auction_eps)) auction_eps <- eps

    mode <- .resolve_spec_mode(memory_mode, cost)
    do_certify <- .resolve_certify(certify, mode)
    out <- if (identical(mode, "implicit")) {
      .assignment_implicit(cost, maximize = maximize, certify = do_certify,
                           method = method)
    } else {
      .assignment_lazy(cost, maximize = maximize, method = method,
                       auction_eps = auction_eps)
    }
    return(.attach_certificate(out, cost, maximize, do_certify))
  }

  method <- match.arg(method)
  do_certify <- .resolve_certify(certify, memory_mode)

  # Back-compat: eps → auction_eps
  if (!is.null(eps) && is.null(auction_eps)) auction_eps <- eps
  if (method == "ssp") method <- "sap"

  cost <- as.matrix(cost)

  n <- nrow(cost); m <- ncol(cost)

  # Validate non-empty matrix first (before type check)
  # This ensures empty logical matrices get "empty" error not "must be numeric"
  if (n == 0 || m == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  # Edge generation over a materialized matrix solves the same problem the
  # switch below solves, by generating the pairs it turns out to need. It saves
  # nothing here -- the matrix already exists -- and it is what lets one path's
  # answer be held against the other's on the same numbers.
  if (identical(memory_mode, "implicit")) {
    if (!identical(cardinality, c("complete", "maximum", "fixed")) &&
        !identical(cardinality, "complete")) {
      stop("cardinality = \"", cardinality[1], "\" is not supported under ",
           "memory_mode = \"implicit\"; the loop matches every row.",
           call. = FALSE)
    }
    out <- .assignment_implicit(cost, maximize = maximize, certify = do_certify,
                                method = method)
    return(.attach_certificate(out, cost, maximize, do_certify))
  }

  # Diagnostic-only: `cost` is already materialized by this point, so "auto"
  # can only warn, not avoid the allocation. See resolve_memory_mode().
  resolve_memory_mode(n, m, memory_mode, solver_supports_lazy = FALSE)

  if (!is.numeric(cost)) {
    stop("`cost` must be a numeric matrix, got ", typeof(cost))
  }

  # A cardinality other than "complete" is expressed as dummy columns priced so
  # that the solver's own optimum is the requested objective. Everything after
  # this point solves that matrix; "complete" leaves it untouched.
  card <- .validate_cardinality_args(cardinality, n_matches, unmatched_penalty,
                                     n, m)
  reduction <- .cardinality_reduction(cost, card$cardinality, card$n_matches,
                                      card$unmatched_penalty, maximize)
  solve_cost <- reduction$work
  n <- nrow(solve_cost); m <- ncol(solve_cost)
  n_required <- reduction$n_required

  # One C++ pass supplies the NaN check and every data-dependent input the
  # "auto" branch below needs. Reading them separately in R (any(is.nan()),
  # range(finite = TRUE), mean(is.na() | is.infinite())) allocated a temporary
  # the size of the cost matrix for each test, which at n = 5000 made
  # method = "auto" measurably slower than naming the solver it would pick.
  probe <- lap_probe_cost_matrix(solve_cost)
  if (probe$has_nan) stop("NaN not allowed in `cost`")

  # The rule table lives in R/lap_dispatch.R and is shared with
  # explain_dispatch(), so the reported reason is the one that was acted on.
  dispatch <- NULL
  if (method == "auto") {
    decision <- .dispatch_decision(n, m, probe)
    method <- decision$method
    dispatch <- list(rule = decision$rule, condition = decision$condition,
                     reason = decision$reason, explicit = FALSE)
  } else {
    dispatch <- list(rule = NA_character_, condition = NA_character_,
                     reason = "method named explicitly", explicit = TRUE)
  }

  # auto-transpose if rows > cols
  transposed <- FALSE
  work <- solve_cost
  if (n > m) {
    work <- t(solve_cost); transposed <- TRUE
    tmp <- n; n <- m; m <- tmp
  }

  res_raw <- switch(
    method,
    "bruteforce"    = lap_solve_bruteforce(work, maximize),
    "jv"            = lap_solve_jv(work, maximize),
    "hungarian"     = lap_solve_hungarian(work, maximize),
    "munkres"       = lap_solve_munkres(work, maximize),
    "auction"       = lap_solve_auction(work, maximize, auction_eps),
    "auction_gs"    = lap_solve_auction_gs(work, maximize, auction_eps),
    "auction_scaled"= lap_solve_auction_scaled(work, maximize),
    "sap"           = lap_solve_ssp(work, maximize),
    "csflow"        = lap_solve_csflow(work, maximize),
    "hk01"          = lap_solve_hk01(work, maximize),
    "ssap_bucket"   = lap_solve_ssap_bucket(work, maximize),
    "cycle_cancel"  = lap_solve_cycle_cancel(work, maximize),
    "gabow_tarjan"  = lap_solve_gabow_tarjan(work, maximize),
    "lapmod"        = lap_solve_lapmod(work, maximize),
    "csa"           = lap_solve_csa(work, maximize),
    "ramshaw_tarjan"= lap_solve_ramshaw_tarjan(work, maximize),
    "push_relabel"  = lap_solve_push_relabel(work, maximize),
    "sap_dense"     = lap_solve_sap_dense(work, maximize),
    "network_simplex"= lap_solve_network_simplex_wrapper(work, maximize),
    stop("Unknown or unimplemented method: ", method)
  )

  match_out <- as.integer(res_raw$match)
  if (transposed) {
    n0 <- ncol(work); m0 <- nrow(work)
    inv <- integer(n0); inv[] <- 0L
    for (i in seq_len(m0)) {
      j <- match_out[i]
      if (j > 0L) inv[j] <- i
    }
    match_out <- inv
  }

  total_cost <- res_raw$total_cost
  duals <- if (method %in% .DUAL_METHODS) .solver_duals(res_raw, transposed)
  if (reduction$n_dummy > 0L) {
    duals <- NULL
    # A row that took a dummy column is a row left unmatched, and the objective
    # is recomputed over real pairs so no sentinel price leaks into it.
    restored <- .cardinality_restore(match_out, cost, reduction$n_dummy)
    match_out <- restored$match
    total_cost <- restored$total_cost
  }

  out <- .new_lap_solve_result(
    match       = match_out,
    total_cost  = total_cost,
    status      = .compute_solve_status(match_out, n_required, method,
                                        solver_status = res_raw$status,
                                        auction_eps = auction_eps),
    method_used = method,
    dispatch    = dispatch
  )
  out$cardinality <- card$cardinality
  out$n_matched   <- sum(match_out > 0L)
  out$unmatched   <- which(match_out == 0L)
  if (!is.null(duals)) {
    out$u <- duals$u
    out$v <- duals$v
  }
  .attach_certificate(out, cost, maximize, do_certify)
}

# Solvers whose duals are optimal potentials of the matrix they were handed. A
# result carries them so that a certificate reads them rather than solving
# again. "gabow_tarjan" also returns duals, but they are the 1-optimal duals of
# the scaled integer instance it solves, which certify nothing about the costs
# as given, and handing them to verify_assignment() would fail a matching the
# optimal duals certify.
.DUAL_METHODS <- c("jv", "hungarian")

# The duals a solver returned, in the caller's orientation, or NULL for a solver
# that returns none. A transposed solve ran on t(cost), so its row duals belong
# to the caller's columns.
.solver_duals <- function(res_raw, transposed) {
  if (is.null(res_raw[["u"]]) || is.null(res_raw[["v"]])) {
    return(NULL)
  }
  u <- as.numeric(res_raw[["u"]])
  v <- as.numeric(res_raw[["v"]])
  if (transposed) list(u = v, v = u) else list(u = u, v = v)
}

# Attach a checked certificate to a solve result that does not carry one.
#
# The implicit path certifies as part of terminating, so there is nothing to add
# there. Every other path proves nothing about its answer beyond the status it
# terminated on. Where the solver returned duals they are on the result and the
# check reads them, so it costs one pass over the admissible pairs; where it
# returned none the check also costs the solve verify_assignment() runs to get
# them. A solve that did not reach a complete optimal matching has nothing to
# certify and gets no certificate; `status` is what says so.
.attach_certificate <- function(out, cost, maximize, certify) {
  if (!isTRUE(certify) || !is.null(out$certificate)) {
    return(out)
  }
  if (!identical(out$status, "optimal")) {
    return(out)
  }
  out$certificate <- verify_assignment(out, cost, maximize = maximize)
  out
}

#' Solve a lazy_cost_spec (memory_mode = "lazy" backend for assignment())
#'
#' Mirrors assignment()'s contract (same result shape) but computes costs
#' on demand from the underlying feature data instead of a materialized
#' matrix. Only "jv" and "auction" are supported -- every other method is
#' fundamentally dense (repeated full-matrix scans, or an algorithm not yet
#' templated for a lazy cost source) and gets a clear error here rather than
#' a silent dense fallback that would defeat the point of `memory_mode =
#' "lazy"`.
#'
#' @keywords internal
.assignment_lazy <- function(cost, maximize = FALSE, method = "auto",
                             auction_eps = NULL) {
  if (!is.logical(maximize) || length(maximize) != 1 || is.na(maximize)) {
    stop("maximize must be TRUE or FALSE", call. = FALSE)
  }

  # `method` may still be the full default-enumeration vector from
  # assignment()'s signature if the caller didn't pass one explicitly.
  if (length(method) > 1 || identical(method, "ssp")) method <- "auto"

  # Lazy mode cannot scan a non-materialized matrix for the dense auto-method
  # heuristic (range()/na_rate in assignment()); default to "jv", matching
  # that heuristic's own "otherwise: jv" fallback for the common case.
  if (identical(method, "auto")) method <- "jv"

  if (!method %in% c("jv", "auction")) {
    stop("method = \"", method, "\" does not support memory_mode = \"lazy\" yet; ",
         "use \"jv\", \"auction\", or memory_mode = \"dense\".", call. = FALSE)
  }

  n0 <- cost$n_left
  m0 <- cost$n_right
  if (n0 == 0 || m0 == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  transposed <- FALSE
  work <- cost
  if (n0 > m0) {
    work <- transpose_lazy_cost_spec(cost)
    transposed <- TRUE
  }

  inv_cov <- lazy_cost_spec_inv_cov(work)
  caliper_list <- lazy_cost_spec_calipers(work)

  res_raw <- if (identical(method, "jv")) {
    cpp_lap_solve_jv_lazy(work$left_mat, work$right_mat, work$distance,
                          inv_cov, work$max_distance, caliper_list,
                          maximize)
  } else {
    cpp_lap_solve_auction_lazy(work$left_mat, work$right_mat, work$distance,
                               inv_cov, work$max_distance, caliper_list,
                               maximize, auction_eps)
  }

  match_out <- as.integer(res_raw$match)
  if (transposed) {
    # Same inversion as assignment()'s dense transpose path: match_out has
    # length work$n_left (= original n_right), values in 1..work$n_right
    # (= original n_left); invert into a vector of length original n_left.
    inv <- integer(n0); inv[] <- 0L
    for (i in seq_len(work$n_left)) {
      j <- match_out[i]
      if (j > 0L) inv[j] <- i
    }
    match_out <- inv
  }

  out <- .new_lap_solve_result(
    match       = match_out,
    total_cost  = res_raw$total_cost,
    status      = .compute_solve_status(match_out, min(n0, m0), method,
                                        solver_status = res_raw$status,
                                        auction_eps = auction_eps),
    method_used = method,
    dispatch    = list(rule = NA_character_, condition = NA_character_,
                       reason = "lazy cost source; the dense probe cannot run",
                       explicit = !identical(method, "jv"))
  )
  duals <- .solver_duals(res_raw, transposed)
  if (!is.null(duals)) {
    out$u <- duals$u
    out$v <- duals$v
  }
  out
}

# ==============================================================================
# Tidy LAP Interface (User-Facing)
# ==============================================================================
# This section contains lap_solve() and related tidy wrappers.
# ==============================================================================
#' Solve linear assignment problems
#'
#' Provides a tidy interface for solving the linear assignment problem using
#' 'Hungarian' or 'Jonker-Volgenant' algorithms. Supports rectangular matrices,
#' NA/Inf masking, and data frame inputs.
#'
#' @param x Cost matrix, data frame, or tibble. If a data frame/tibble,
#'   must include columns specified by `source`, `target`, and `cost`.
#' @param source Column name for source/row indices (if `x` is a data frame)
#' @param target Column name for target/column indices (if `x` is a data frame)
#' @param cost Column name for costs (if `x` is a data frame)
#' @param maximize Logical; if TRUE, maximizes total cost instead of minimizing (default: FALSE)
#' @param method Algorithm to use. One of:
#'   - "auto" (default): automatically selects best algorithm
#'   - "jv": 'Jonker-Volgenant' algorithm (general purpose, fast)
#'   - "hungarian": Classic 'Hungarian' algorithm
#'   - "auction": 'Bertsekas' auction algorithm (good for large dense problems)
#'   - "sap": Sparse assignment (good for sparse/rectangular problems)
#'   - "hk01": 'Hopcroft-Karp' for binary/uniform costs
#' @param forbidden Value to mark forbidden assignments (default: NA). Can also use Inf.
#'
#' @return A tibble with columns:
#'   - `source`: row/source indices
#'   - `target`: column/target indices  
#'   - `cost`: cost of each assignment
#'   - `total_cost`: total cost (attribute)
#'
#' @examples
#' # Matrix input
#' cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3)
#' lap_solve(cost)
#'
#' # Data frame input
#' library(dplyr)
#' df <- tibble(
#'   source = rep(1:3, each = 3),
#'   target = rep(1:3, times = 3),
#'   cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
#' )
#' lap_solve(df, source, target, cost)
#'
#' # With NA masking (forbidden assignments)
#' cost[1, 3] <- NA
#' lap_solve(cost)
#'
#' # Grouped data frames
#' df <- tibble(
#'   sim = rep(1:2, each = 9),
#'   source = rep(1:3, times = 6),
#'   target = rep(1:3, each = 3, times = 2),
#'   cost = runif(18, 1, 10)
#' )
#' df |> group_by(sim) |> lap_solve(source, target, cost)
#'
#' @export
lap_solve <- function(x, source = NULL, target = NULL, cost = NULL,
                   maximize = FALSE, method = "auto", forbidden = NA) {
  
  # Check if this is a grouped data frame
  is_grouped <- inherits(x, "grouped_df")
  
  if (is_grouped) {
    # Handle grouped data frames
    return(lap_solve_grouped(x, {{ source }}, {{ target }}, {{ cost }},
                         maximize = maximize, method = method, forbidden = forbidden))
  }
  
  # Handle data frame input
  if (is.data.frame(x)) {
    source_col <- rlang::enquo(source)
    target_col <- rlang::enquo(target)
    cost_col <- rlang::enquo(cost)
    
    if (rlang::quo_is_null(source_col) || rlang::quo_is_null(target_col) || 
        rlang::quo_is_null(cost_col)) {
      stop("For data frame input, must specify `source`, `target`, and `cost` columns")
    }
    
    return(lap_solve_df(x, source_col, target_col, cost_col, 
                    maximize = maximize, method = method, forbidden = forbidden))
  }
  
  # Handle matrix input
  cost_matrix <- as.matrix(x)

  cost_matrix <- mask_forbidden(cost_matrix, forbidden)

  # Call the underlying assignment function
  result <- assignment(cost_matrix, maximize = maximize, method = method)
  
  # Convert to tidy tibble format
  matched_indices <- which(result$match > 0)
  
  if (length(matched_indices) == 0) {
    out <- tibble::tibble(
      source = integer(0),
      target = integer(0),
      cost = numeric(0)
    )
  } else {
    out <- tibble::tibble(
      source = matched_indices,
      target = result$match[matched_indices],
      cost = cost_matrix[cbind(matched_indices, result$match[matched_indices])]
    )
  }
  
  # Add total_cost as attribute
  attr(out, "total_cost") <- result$total_cost
  attr(out, "method_used") <- result$method_used
  class(out) <- c("lap_solve_result", class(out))
  
  out
}

#' @keywords internal
lap_solve_df <- function(df, source_col, target_col, cost_col, 
                     maximize = FALSE, method = "auto", forbidden = NA) {
  
  # Extract columns with error handling
  source_vals <- tryCatch(
    rlang::eval_tidy(source_col, df),
    error = function(e) stop("For data frame input, must specify `source`, `target`, and `cost` columns", call. = FALSE)
  )
  target_vals <- tryCatch(
    rlang::eval_tidy(target_col, df),
    error = function(e) stop("For data frame input, must specify `source`, `target`, and `cost` columns", call. = FALSE)
  )
  cost_vals <- tryCatch(
    rlang::eval_tidy(cost_col, df),
    error = function(e) stop("For data frame input, must specify `source`, `target`, and `cost` columns", call. = FALSE)
  )
  
  built <- long_to_cost_matrix(source_vals, target_vals, cost_vals, forbidden)
  cost_matrix <- built$cost_matrix
  unique_sources <- built$sources
  unique_targets <- built$targets
  
  # Solve
  result <- assignment(cost_matrix, maximize = maximize, method = method)
  
  # Convert back to original indices
  matched_indices <- which(result$match > 0)
  
  if (length(matched_indices) == 0) {
    out <- tibble::tibble(
      source = unique_sources[integer(0)],
      target = unique_targets[integer(0)],
      cost = numeric(0)
    )
  } else {
    out <- tibble::tibble(
      source = unique_sources[matched_indices],
      target = unique_targets[result$match[matched_indices]],
      cost = cost_matrix[cbind(matched_indices, result$match[matched_indices])]
    )
  }
  
  attr(out, "total_cost") <- result$total_cost
  attr(out, "method_used") <- result$method_used
  class(out) <- c("lap_solve_result", class(out))
  
  out
}

#' @keywords internal
lap_solve_grouped <- function(df, source_col, target_col, cost_col,
                          maximize = FALSE, method = "auto", forbidden = NA) {
  
  source_col <- rlang::enquo(source_col)
  target_col <- rlang::enquo(target_col)
  cost_col <- rlang::enquo(cost_col)
  
  # Get group variables
  groups <- dplyr::group_vars(df)
  
  # Split by groups and solve each
  df |>
    dplyr::group_split() |>
    purrr::map_dfr(function(group_df) {
      # Extract group values
      group_vals <- group_df[1, groups, drop = FALSE]
      
      # Solve for this group
      result <- lap_solve_df(group_df, source_col, target_col, cost_col,
                         maximize = maximize, method = method, forbidden = forbidden)
      
      # Add group columns back
      dplyr::bind_cols(group_vals, result)
    })
}

#' Print method for assignment results
#'
#' Nicely prints a `lap_solve_result` object, including the assignments,
#' total cost, and method used.
#'
#' @param x A `lap_solve_result` object.
#' @param ... Additional arguments passed to `print()`. Currently ignored.
#'
#' @return Invisibly returns the input object `x`.
#' @export
#' @method print lap_solve_result
print.lap_solve_result <- function(x, ...) {
  cat("Assignment Result\n")
  cat("=================\n\n")

  total_cost <- attr(x, "total_cost")
  method_used <- attr(x, "method_used")
  # Only the list form carries these; reading them off the tibble form would
  # warn about an uninitialised column.
  status <- NULL
  certificate <- NULL
  search <- NULL

  # Check if this is a tibble (from lap_solve) or a plain list (from assignment)
  if (inherits(x, "tbl_df") || inherits(x, "data.frame")) {
    # It's already a tibble/data.frame, just print it
    print(tibble::as_tibble(x), ...)
  } else {
    # It's a plain list from assignment(), format nicely
    cat("Assignments (1-based indices):\n")
    matched <- which(x$match > 0)
    if (length(matched) > 0) {
      for (i in matched[1:min(10, length(matched))]) {
        cat(sprintf("  Row %d -> Column %d\n", i, x$match[i]))
      }
      if (length(matched) > 10) {
        cat(sprintf("  ... and %d more\n", length(matched) - 10))
      }
    } else {
      cat("  (no assignments)\n")
    }
    total_cost <- x$total_cost
    method_used <- x$method_used
    status <- x$status
    certificate <- x$certificate
    search <- x$search
  }

  cat("\nTotal cost:", total_cost, "\n")
  if (!is.null(method_used)) {
    cat("Method:", method_used, "\n")
  }
  if (!is.null(status)) {
    cat("Status:", status, "\n")
  }
  if (!is.null(certificate)) {
    cat("Certified optimal:", certificate$certified_optimal, "\n")
  }
  if (!is.null(search)) {
    cat(sprintf("Pairs generated: %s of %s (%.4g%%), %s rounds\n",
                format(search$candidate_edges, big.mark = ",", scientific = FALSE),
                format(search$possible_edges, big.mark = ",", scientific = FALSE),
                100 * search$candidate_edges / search$possible_edges,
                search$n_rounds))
  }

  invisible(x)
}

# ============================================================================
# Specialized 1-D Line-Metric LAP Solver
# ============================================================================

#' Solve 1-D Line Assignment Problem
#'
#' Solves the linear assignment problem when both sources and targets are 
#' ordered points on a line. Uses efficient O(n*m) dynamic programming
#' for rectangular problems and O(n) sorting for square problems.
#'
#' This is a specialized solver that exploits the structure of 1-dimensional
#' assignment problems where costs depend only on the distance between points
#' on a line. It is much faster than general LAP solvers for this special case.
#'
#' @param x Numeric vector of source positions (will be sorted internally)
#' @param y Numeric vector of target positions (will be sorted internally)
#' @param cost Cost function for distance. Either:
#'   - "L1" (default): absolute distance ('Manhattan' distance)
#'   - "L2": squared distance (squared 'Euclidean' distance)
#'   Can also use aliases: "abs", "manhattan" for L1; "sq", "squared", "quadratic" for L2
#' @param maximize Logical; if TRUE, maximizes total cost instead of minimizing (default: FALSE)
#'
#' @details
#' The algorithm works as follows:
#' 
#' **Square case (n == m):**
#' Both vectors are sorted and matched in order: \code{x[1] -> y[1]}, \code{x[2] -> y[2]}, etc.
#' This is optimal for any metric cost function on a line.
#'
#' **Rectangular case (n < m):**
#' Uses dynamic programming to find the optimal assignment that matches all n sources
#' to a subset of the m targets, minimizing total distance. The DP recurrence is:
#'
#' \code{dp[i][j] = min(dp[i][j-1], dp[i-1][j-1] + cost(x[i], y[j]))}
#' 
#' This finds the minimum cost to match the first i sources to the first j targets.
#' 
#' **Complexity:**
#' - Time: O(n*m) for rectangular, O(n log n) for square
#' - Space: O(n*m) for DP table
#'
#' @return A list with components:
#'   - `match`: Integer vector of length n with 1-based column indices
#'   - `total_cost`: Total cost of the assignment
#'
#' @examples
#' # Square case: equal number of sources and targets
#' x <- c(1.5, 3.2, 5.1)
#' y <- c(2.0, 3.0, 5.5)
#' result <- lap_solve_line_metric(x, y, cost = "L1")
#' print(result)
#' 
#' # Rectangular case: more targets than sources
#' x <- c(1.0, 3.0, 5.0)
#' y <- c(0.5, 2.0, 3.5, 4.5, 6.0)
#' result <- lap_solve_line_metric(x, y, cost = "L2")
#' print(result)
#' 
#' # With unsorted inputs (will be sorted internally)
#' x <- c(5.0, 1.0, 3.0)
#' y <- c(4.5, 0.5, 6.0, 2.0, 3.5)
#' result <- lap_solve_line_metric(x, y, cost = "L1")
#' print(result)
#'
#' @export
lap_solve_line_metric <- function(x, y, cost = "L1", maximize = FALSE) {
  # Validate inputs
  if (!is.numeric(x) || length(x) == 0) {
    stop("x must be a non-empty numeric vector")
  }
  if (!is.numeric(y) || length(y) == 0) {
    stop("y must be a non-empty numeric vector")
  }
  if (length(x) > length(y)) {
    stop("Number of sources (length of x) must be <= number of targets (length of y)")
  }
  
  # Check for NaN/Inf values
  if (any(!is.finite(x))) {
    stop("x must contain only finite values (no NA, NaN, or Inf)")
  }
  if (any(!is.finite(y))) {
    stop("y must contain only finite values (no NA, NaN, or Inf)")
  }
  
  # Validate cost parameter
  cost_str <- as.character(cost)[1]
  valid_costs <- c("L1", "l1", "abs", "manhattan", "L2", "l2", "sq", "squared", "quadratic")
  if (!(cost_str %in% valid_costs)) {
    stop("cost must be one of: 'L1', 'L2', 'abs', 'manhattan', 'sq', 'squared', 'quadratic'")
  }
  
  # Call C++ implementation
  result <- lap_solve_line_metric_cpp(
    x = as.numeric(x),
    y = as.numeric(y),
    cost = cost_str,
    maximize = as.logical(maximize)
  )
  
  class(result) <- c("lap_line_metric_result", "lap_result", class(result))
  return(result)
}

#' @export
print.lap_line_metric_result <- function(x, ...) {
  cat("1-D Line Assignment Result\n")
  cat("===========================\n\n")
  
  n <- length(x$match)
  cat("Assignments (1-based):\n")
  for (i in seq_len(min(n, 10))) {
    cat(sprintf("  Source %d -> Target %d\n", i, x$match[i]))
  }
  if (n > 10) {
    cat(sprintf("  ... (%d more assignments)\n", n - 10))
  }
  
  cat("\nTotal cost:", x$total_cost, "\n")
  invisible(x)
}

# ==============================================================================
# Bottleneck Assignment Problem (BAP) Solver
# ==============================================================================

#' Solve the Bottleneck Assignment Problem
#'
#' Finds an assignment that minimizes (or maximizes) the maximum edge cost
#' in a perfect matching. Unlike standard LAP which minimizes the sum of costs,
#' BAP minimizes the maximum (bottleneck) cost.
#'
#' @param cost Numeric matrix; rows = tasks, columns = agents. `NA` or `Inf`
#'   entries are treated as forbidden assignments.
#' @param maximize Logical; if `TRUE`, maximizes the minimum edge cost instead
#'   of minimizing the maximum (maximin objective). Default is `FALSE` (minimax).
#'
#' @return A list with class `"bottleneck_result"` containing:
#'   \itemize{
#'     \item `match` - integer vector of length `nrow(cost)` giving the
#'           assigned column for each row (1-based indexing)
#'     \item `bottleneck` - numeric scalar, the bottleneck (max/min edge) value
#'     \item `status` - character scalar, e.g. `"optimal"`
#'   }
#'
#' @details
#' The Bottleneck Assignment Problem (BAP) is a variant of the Linear Assignment
#' Problem where instead of minimizing the sum of assignment costs, we minimize
#' the maximum cost among all assignments (minimax objective).
#'
#' **Algorithm:**
#' Uses binary search on the sorted unique costs combined with 'Hopcroft-Karp'
#' bipartite matching to find the minimum threshold that allows a perfect matching.
#'
#' **Complexity:** O(E * sqrt(V) * log(unique costs)) where E = edges, V = vertices.
#'
#' **Applications:**
#' \itemize{
#'   \item Task scheduling with deadline constraints (minimize latest completion)
#'   \item Resource allocation (minimize maximum load/distance)
#'   \item Network routing (minimize maximum link utilization)
#'   \item Fair division problems (minimize maximum disparity)
#' }
#'
#' @examples
#' # Simple example: minimize max cost
#' cost <- matrix(c(1, 5, 3,
#'                  2, 4, 6,
#'                  7, 1, 2), nrow = 3, byrow = TRUE)
#' result <- bottleneck_assignment(cost)
#' result$bottleneck  # Maximum edge cost in optimal assignment
#'
#' # Maximize minimum (fair allocation)
#' profits <- matrix(c(10, 5, 8,
#'                     6, 12, 4,
#'                     3, 7, 11), nrow = 3, byrow = TRUE)
#' result <- bottleneck_assignment(profits, maximize = TRUE)
#' result$bottleneck  # Minimum profit among all assignments
#'
#' # With forbidden assignments
#' cost <- matrix(c(1, NA, 3,
#'                  2, 4, Inf,
#'                  5, 1, 2), nrow = 3, byrow = TRUE)
#' result <- bottleneck_assignment(cost)
#'
#' @seealso [assignment()] for standard LAP (sum objective), [lap_solve()] for
#'   tidy LAP interface
#'
#' @export
bottleneck_assignment <- function(cost, maximize = FALSE) {
  cost <- as.matrix(cost)

  # Check empty first (before type check) so empty logical matrices get sensible error
  n <- nrow(cost)
  m <- ncol(cost)
  if (n == 0 || m == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  if (!is.numeric(cost)) {
    stop("`cost` must be a numeric matrix, got ", typeof(cost))
  }

  if (any(is.nan(cost))) {
    stop("NaN not allowed in `cost`")
  }

  if (n > m) {
    stop("Bottleneck assignment requires nrow <= ncol. ",
         "Got ", n, " rows and ", m, " columns.")
  }

  # Call C++ implementation
  res_raw <- lap_solve_bottleneck(cost, maximize)

  match_out <- as.integer(res_raw$match)
  out <- list(
    match = match_out,
    bottleneck = as.numeric(res_raw$total_cost),
    status = .compute_solve_status(match_out, n, "bottleneck")
  )
  class(out) <- "bottleneck_result"
  out
}

#' @export
print.bottleneck_result <- function(x, ...) {
  cat("Bottleneck Assignment Result\n")
  cat("============================\n\n")

  n <- length(x$match)
  cat("Assignments (1-based indices):\n")
  for (i in seq_len(min(n, 10))) {
    cat(sprintf("  Row %d -> Column %d\n", i, x$match[i]))
  }
  if (n > 10) {
    cat(sprintf("  ... (%d more assignments)\n", n - 10))
  }

  cat("\nBottleneck value:", x$bottleneck, "\n")
  cat("Status:", x$status, "\n")

  invisible(x)
}

# ==============================================================================
# Internal: Dense-scan Successive Shortest Path Wrapper
# ==============================================================================
# Returns standard LAP format

#' @keywords internal
lap_solve_sap_dense <- function(cost, maximize = FALSE) {
  # Successive shortest paths, Dijkstra with a linear scan over the columns
  # O(n * m^2) complexity
  work <- if (maximize) -cost else cost
  # Treat NA *and* non-finite (e.g. -Inf produced by negating +Inf in maximize
  # mode) as forbidden. The plain `is.na(work)` check missed -Inf and let
  # forbidden cells slip into the solver as extreme-cost real edges.
  work[!is.finite(work)] <- Inf

  result <- sap_dense_solve(work)

  # Recompute total_cost from original cost matrix
  n <- nrow(cost)
  total_cost <- 0
  for (i in seq_len(n)) {
    j <- result$match[i]
    if (j > 0 && is.finite(cost[i, j])) {
      total_cost <- total_cost + cost[i, j]
    }
  }

  list(match = result$match, total_cost = total_cost)
}

#' @keywords internal
lap_solve_network_simplex_wrapper <- function(cost, maximize = FALSE) {
  # Network simplex for minimum-cost flow on assignment network
  work <- if (maximize) -cost else cost
  # Treat NA *and* non-finite (e.g. -Inf produced by negating +Inf in maximize
  # mode) as forbidden. The plain `is.na(work)` check missed -Inf and let
  # forbidden cells slip into the solver as extreme-cost real edges.
  work[!is.finite(work)] <- Inf

  result <- lap_solve_network_simplex(work)

  # Compute total cost from original cost matrix
  n <- nrow(cost)
  total_cost <- 0
  for (i in seq_len(n)) {
    j <- result$match[i]
    if (j > 0 && is.finite(cost[i, j])) {
      total_cost <- total_cost + cost[i, j]
    }
  }

  # The C++ solver reports why its pivot loop stopped; carry that through
  # rather than letting the caller infer optimality from a full matching.
  list(match = result$match, total_cost = total_cost, status = result$status)
}

# ==============================================================================
# Note on Specialized Algorithms
# ==============================================================================
# For specialized algorithms like ssap_bucket, cycle_cancel, gabow_tarjan, and
# sap_dense, use assignment(cost, method = "ssap_bucket"),
# assignment(cost, method = "cycle_cancel"), assignment(cost, method = "gabow_tarjan"),
# or assignment(cost, method = "sap_dense").
#
# These are accessed via the method parameter in assignment() rather than
# separate wrapper functions to keep the API clean.

# ==============================================================================
# Sinkhorn-Knopp (Entropy-Regularized Optimal Transport)
# ==============================================================================

#' 'Sinkhorn-Knopp' optimal transport solver
#'
#' Compute an entropy-regularized optimal transport plan using the 'Sinkhorn-Knopp'
#' algorithm. Unlike other LAP solvers that return a hard 1-to-1 assignment,
#' this returns a soft assignment (doubly stochastic matrix).
#'
#' @param cost Numeric matrix of transport costs. `NA` or `Inf` entries are
#'   treated as very high cost (effectively forbidden).
#' @param lambda Regularization parameter (default 10). Higher values produce
#'   sharper (more deterministic) transport plans; lower values produce smoother
#'   distributions. Typical range: 1-100.
#' @param tol Convergence tolerance (default 1e-9).
#' @param max_iter Maximum iterations (default 1000).
#' @param r_weights Optional numeric vector of row marginals (source distribution).
#'   Default is uniform. Will be normalized to sum to 1.
#' @param c_weights Optional numeric vector of column marginals (target distribution).
#'   Default is uniform. Will be normalized to sum to 1.
#'
#' @return A list with elements:
#' \itemize{
#'   \item `transport_plan` — numeric matrix, the optimal transport plan P.
#'         Row sums approximate r_weights, column sums approximate c_weights.
#'   \item `cost` — the transport cost <C, P> (without entropy term).
#'   \item `u`, `v` — scaling vectors (P = diag(u) * K * diag(v) where K = exp(-lambda*C)).
#'   \item `converged` — logical, whether the algorithm converged.
#'   \item `iterations` — number of iterations used.
#'   \item `lambda` — the regularization parameter used.
#' }
#'
#' @details
#' The 'Sinkhorn-Knopp' algorithm solves the entropy-regularized optimal transport
#' problem:
#'
#' \deqn{P^* = \arg\min_P \langle C, P \rangle - \frac{1}{\lambda} H(P)}
#'
#' subject to row sums = r_weights and column sums = c_weights.
#'
#' The entropy term H(P) encourages spread in the transport plan. As lambda -> Inf,

#' the solution approaches the standard (unregularized) optimal transport.
#'
#' **Key differences from standard LAP solvers:**
#' - Returns a soft assignment (probabilities) not a hard 1-to-1 matching
#' - Supports unequal marginals (weighted distributions)
#' - Differentiable, making it useful in ML pipelines
#' - Very fast: O(n^2) per iteration with typically O(1/tol^2) iterations
#'
#' Use [sinkhorn_to_assignment()] to round the soft assignment to a hard matching.
#'
#' @examples
#' cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
#'
#' # Soft assignment with default parameters
#' result <- sinkhorn(cost)
#' print(round(result$transport_plan, 3))
#'
#' # Sharper assignment (higher lambda)
#' result_sharp <- sinkhorn(cost, lambda = 50)
#' print(round(result_sharp$transport_plan, 3))
#'
#' # With custom marginals (more mass from row 1)
#' result_weighted <- sinkhorn(cost, r_weights = c(0.5, 0.25, 0.25))
#' print(round(result_weighted$transport_plan, 3))
#'
#' # Round to hard assignment
#' hard_match <- sinkhorn_to_assignment(result)
#' print(hard_match)
#'
#' @seealso [assignment()] for hard 1-to-1 matching, [sinkhorn_to_assignment()]
#'   to round soft assignments.
#'
#' @references
#' Cuturi, M. (2013). 'Sinkhorn Distances': Lightspeed Computation of Optimal
#' Transport. *Advances in Neural Information Processing Systems*, 26.
#'
#' @export
sinkhorn <- function(cost, lambda = 10, tol = 1e-9, max_iter = 1000,
                     r_weights = NULL, c_weights = NULL) {
  if (!is.matrix(cost)) {
    cost <- as.matrix(cost)
  }
  if (!is.numeric(cost)) {
    stop("cost must be a numeric matrix")
  }
  if (lambda <= 0) {
    stop("lambda must be positive")
  }

  lap_solve_sinkhorn(cost, lambda, tol, max_iter, r_weights, c_weights)
}

#' Round 'Sinkhorn' transport plan to hard assignment
#'
#' Convert a soft transport plan from [sinkhorn()] to a hard 1-to-1 assignment
#' using greedy rounding.
#'
#' @param result Either a result from [sinkhorn()] or a transport plan matrix.
#'
#' @return Integer vector of column assignments (1-based), same format as
#'   [assignment()].
#'
#' @details
#' Greedy rounding iteratively assigns each row to its most probable column,
#' ensuring no column is assigned twice. This may not give the globally optimal
#' hard assignment; for that, use the transport plan as a cost matrix with
#' [assignment()].
#'
#' @examples
#' cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
#' result <- sinkhorn(cost, lambda = 20)
#' hard_match <- sinkhorn_to_assignment(result)
#' print(hard_match)
#'
#' @seealso [sinkhorn()]
#' @export
sinkhorn_to_assignment <- function(result) {
  if (is.list(result) && "transport_plan" %in% names(result)) {
    P <- result$transport_plan
  } else if (is.matrix(result)) {
    P <- result
  } else {
    stop("result must be a sinkhorn() result or a transport plan matrix")
  }

  sinkhorn_round(P)
}

# ==============================================================================
# Assignment with Dual Variables
# ==============================================================================

#' Solve assignment problem and return dual variables
#'
#' Solves the linear assignment problem and returns dual potentials (u, v)
#' in addition to the optimal matching. The dual variables provide an
#' optimality certificate and enable sensitivity analysis.
#'
#' @param cost Numeric matrix; rows = tasks, columns = agents. `NA` or `Inf`
#'   entries are treated as forbidden assignments. A lazy cost specification
#'   from [compute_distances()] is also accepted, and is solved without
#'   materializing the matrix.
#' @param maximize Logical; if `TRUE`, maximizes the total cost instead of minimizing.
#' @param certify Logical; if `TRUE`, the duals are checked against `cost` with
#'   [verify_assignment()] and the resulting `assignment_certificate` is
#'   attached as `certificate`. The check is one pass over the admissible pairs
#'   and reuses the duals computed here, so it costs no second solve.
#'
#' @return A list with class `"assignment_duals_result"` containing:
#'   \itemize{
#'     \item `match` - integer vector of column assignments (1-based)
#'     \item `total_cost` - optimal objective value
#'     \item `u` - numeric vector of row dual variables (length n)
#'     \item `v` - numeric vector of column dual variables (length m)
#'     \item `status` - character, e.g. "optimal"
#'     \item `certificate` - an `assignment_certificate`, present only under
#'           `certify = TRUE`
#'   }
#'
#' @details
#' The dual variables satisfy the complementary slackness conditions:
#' \itemize{
#'   \item For minimization: `u[i] + v[j] <= cost[i,j]` for all (i,j)
#'   \item For any assigned pair (i,j): `u[i] + v[j] = cost[i,j]`
#' }
#'
#' This implies that `sum(u) + sum(v) = total_cost` (strong duality).
#'
#' **Applications of dual variables:**
#' \itemize{
#'   \item **Optimality verification**: Check that duals satisfy constraints

#'   \item **Sensitivity analysis**: Reduced cost `c[i,j] - u[i] - v[j]` shows
#'         how much an edge cost must decrease before it enters the solution
#'   \item **Pricing in column generation**: Use duals to price new columns
#'   \item **Warm starting**: Reuse duals when costs change slightly
#' }
#'
#' @examples
#' cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
#' result <- assignment_duals(cost)
#'
#' # Check optimality: u + v should equal cost for assigned pairs
#' for (i in 1:3) {
#'   j <- result$match[i]
#'   cat(sprintf("Row %d -> Col %d: u + v = %.2f, cost = %.2f\n",
#'               i, j, result$u[i] + result$v[j], cost[i, j]))
#' }
#'
#' # Verify strong duality
#' cat("sum(u) + sum(v) =", sum(result$u) + sum(result$v), "\n")
#' cat("total_cost =", result$total_cost, "\n")
#'
#' # Reduced costs (how much must cost decrease to enter solution)
#' reduced <- outer(result$u, result$v, "+")
#' reduced_cost <- cost - reduced
#' print(round(reduced_cost, 2))
#'
#' @seealso [assignment()] for standard assignment without duals,
#'   [verify_assignment()] for the check `certify = TRUE` runs
#' @importFrom utils head
#' @export
assignment_duals <- function(cost, maximize = FALSE, certify = FALSE) {
  if (!is.logical(certify) || length(certify) != 1L || is.na(certify)) {
    stop("`certify` must be TRUE or FALSE.", call. = FALSE)
  }

  out <- if (is_lazy_cost_spec(cost)) {
    .assignment_duals_lazy(cost, maximize)
  } else {
    .assignment_duals_dense(cost, maximize)
  }
  class(out) <- "assignment_duals_result"

  if (certify) {
    # verify_assignment() reads the duals off `out` rather than solving again,
    # which is what makes the check an added pass and not an added solve.
    out$certificate <- verify_assignment(out, cost, maximize = maximize)
  }
  out
}

# Both dual paths solve on the orientation with at least as many columns as
# rows, so both read their solver's answer back the same way: on a transposed
# problem the match inverts into one column per original row and u and v swap
# sides. `n` and `m` are the caller's dimensions, before any transpose.
.duals_result <- function(res_raw, n, m, transposed) {
  match_out <- as.integer(res_raw$match)
  if (transposed) {
    match_out <- .certify_invert_match(match_out, n)
  }
  duals <- .solver_duals(res_raw, transposed)

  list(
    match = match_out,
    total_cost = as.numeric(res_raw$total_cost),
    u = duals$u,
    v = duals$v,
    status = .compute_solve_status(match_out, min(n, m), "jv")
  )
}

.assignment_duals_dense <- function(cost, maximize) {
  cost <- as.matrix(cost)
  if (!is.numeric(cost)) {
    stop("`cost` must be a numeric matrix, got ", typeof(cost))
  }
  if (any(is.nan(cost))) stop("NaN not allowed in `cost`")

  n <- nrow(cost)
  m <- ncol(cost)

  if (n == 0 || m == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  transposed <- n > m
  work <- if (transposed) t(cost) else cost

  .duals_result(lap_solve_jv(work, maximize), n, m, transposed)
}

# The dual entry point for a cost source that computes its cells on demand.
# Same solver and same result shape as the dense path; what it avoids is
# materializing the matrix, which is the whole premise of the lazy path.
.assignment_duals_lazy <- function(cost, maximize) {
  n <- cost$n_left
  m <- cost$n_right
  if (n == 0 || m == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  transposed <- n > m
  work <- if (transposed) transpose_lazy_cost_spec(cost) else cost

  res_raw <- cpp_lap_solve_jv_lazy(work$left_mat, work$right_mat,
                                   work$distance,
                                   lazy_cost_spec_inv_cov(work),
                                   work$max_distance,
                                   lazy_cost_spec_calipers(work),
                                   maximize)

  .duals_result(res_raw, n, m, transposed)
}

#' @export
print.assignment_duals_result <- function(x, ...) {
  cat("Assignment Result with Duals\n")
  cat("============================\n\n")

  n <- length(x$match)
  cat("Assignments (1-based indices):\n")
  for (i in seq_len(min(n, 10))) {
    if (x$match[i] > 0) {
      cat(sprintf("  Row %d -> Column %d\n", i, x$match[i]))
    }
  }
  if (n > 10) {
    cat(sprintf("  ... (%d more assignments)\n", n - 10))
  }

  cat("\nTotal cost:", x$total_cost, "\n")
  cat("Status:", x$status, "\n")

  cat("\nDual variables:\n")
  cat("  u (row):", head(x$u, 5))
  if (length(x$u) > 5) cat(" ...")
  cat("\n")
  cat("  v (col):", head(x$v, 5))
  if (length(x$v) > 5) cat(" ...")
  cat("\n")

  invisible(x)
}
