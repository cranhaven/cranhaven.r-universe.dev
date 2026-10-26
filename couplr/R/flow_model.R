# ==============================================================================
# The flow model
# ==============================================================================
# Every matching design is the same object underneath: nodes carrying supplies,
# and arcs carrying a cost and a capacity range. A one-to-one match, a k:1
# match and a full match differ in which bounds their arcs get, not in which
# code runs.
#
# What lives here is the R side of that object, and it is a description rather
# than an engine. Compiling a design, solving it, and checking the answer stay
# three separate steps because the third one is only worth anything while it is
# separate: a solver that hands back its own verdict has proven nothing. The
# flow and the potentials go into verify_flow() as inputs to a check, exactly
# as the assignment duals go into verify_assignment(), and potentials that
# certify nothing make the check fail rather than pass.
#
# Node ids are 1-based here and 0-based in the C++ model. Capacities cross as
# doubles with Inf standing for the model's unbounded capacity, because R has
# no 64-bit integer type and every finite capacity a design produces is a unit
# count well inside the range a double holds exactly.
# ==============================================================================

.FLOW_ARC_COLUMNS <- c("tail", "head", "lower", "upper", "cost")

# A single count: whole, finite, and small enough that a double still holds it
# exactly. Everything the model measures in capacities is a unit count.
.flow_count <- function(x, what, allow_inf = FALSE, allow_negative = FALSE) {
  x <- as.numeric(x)
  if (length(x) != 1L || is.na(x)) {
    stop("`", what, "` must be a single non-NA number.", call. = FALSE)
  }
  if (is.infinite(x)) {
    if (allow_inf && x > 0) return(x)
    stop("`", what, "` must be finite.", call. = FALSE)
  }
  if (x != trunc(x)) {
    stop("`", what, "` must be a whole number, got ", format(x), ".",
         call. = FALSE)
  }
  if (!allow_negative && x < 0) {
    stop("`", what, "` must not be negative.", call. = FALSE)
  }
  x
}

.flow_arcs <- function(arcs, n_nodes) {
  if (!is.data.frame(arcs)) {
    stop("`arcs` must be a data frame with columns ",
         paste(.FLOW_ARC_COLUMNS, collapse = ", "), ".", call. = FALSE)
  }
  absent <- setdiff(.FLOW_ARC_COLUMNS, names(arcs))
  if (length(absent)) {
    stop("`arcs` is missing the column(s) ", paste(absent, collapse = ", "), ".",
         call. = FALSE)
  }

  tail <- as.integer(arcs$tail)
  head <- as.integer(arcs$head)
  lower <- as.numeric(arcs$lower)
  upper <- as.numeric(arcs$upper)
  cost <- as.numeric(arcs$cost)

  if (anyNA(tail) || anyNA(head)) {
    stop("Every arc needs both endpoints; `tail` and `head` carry NA.",
         call. = FALSE)
  }
  if (any(tail < 1L | tail > n_nodes) || any(head < 1L | head > n_nodes)) {
    stop("Arc endpoints must be node ids in 1:", n_nodes, ".", call. = FALSE)
  }
  if (anyNA(lower) || any(is.infinite(lower)) || any(lower < 0) ||
      any(lower != trunc(lower))) {
    stop("`arcs$lower` must be finite whole numbers, none negative.",
         call. = FALSE)
  }
  if (anyNA(upper) || any(upper < lower) ||
      any(is.finite(upper) & upper != trunc(upper))) {
    stop("`arcs$upper` must be whole numbers or Inf, none below `arcs$lower`.",
         call. = FALSE)
  }
  # A non-finite cost is not how an arc is forbidden. Forbidden is expressed by
  # leaving the arc out, and an Inf that reaches the residual search poisons a
  # shortest-path distance and then every potential derived from it.
  if (anyNA(cost) || any(!is.finite(cost))) {
    stop("`arcs$cost` must be finite; omit an arc rather than pricing it at ",
         "Inf.", call. = FALSE)
  }

  tibble::tibble(tail = tail, head = head, lower = lower, upper = upper,
                 cost = cost)
}

# Construct and check a flow problem. The checks are the ones the C++ validator
# runs, moved to the door so a malformed problem is named in R terms before it
# reaches a solver that can only speak in node indices.
.flow_problem <- function(n_nodes, supply, arcs) {
  n_nodes <- as.integer(.flow_count(n_nodes, "n_nodes"))
  if (n_nodes < 1L) {
    stop("A flow problem needs at least one node.", call. = FALSE)
  }

  supply <- as.numeric(supply)
  if (length(supply) != n_nodes) {
    stop("`supply` has length ", length(supply), " but the problem has ",
         n_nodes, " nodes.", call. = FALSE)
  }
  if (anyNA(supply) || any(!is.finite(supply)) || any(supply != trunc(supply))) {
    stop("`supply` must be finite whole numbers.", call. = FALSE)
  }
  # Every unit that leaves a node enters another one, so the surpluses and the
  # deficits are the same total counted from both ends.
  if (sum(supply) != 0) {
    stop("Supplies must sum to zero; they sum to ", format(sum(supply)), ".",
         call. = FALSE)
  }

  structure(list(n_nodes = n_nodes,
                 supply = supply,
                 arcs = .flow_arcs(arcs, n_nodes)),
            class = "couplr_flow_problem")
}

# Accept either a built problem or the three fields it is made of, so a caller
# describing a problem inline never has to reach for the constructor.
.as_flow_problem <- function(x) {
  if (inherits(x, "couplr_flow_problem")) return(x)
  if (is.list(x) && all(c("n_nodes", "supply", "arcs") %in% names(x))) {
    return(.flow_problem(x$n_nodes, x$supply, x$arcs))
  }
  stop("`problem` must be a flow problem, or a list with elements `n_nodes`, ",
       "`supply` and `arcs`.", call. = FALSE)
}

#' Print Method for Flow Problems
#'
#' @param x A flow problem.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#' @keywords internal
#' @export
#' @method print couplr_flow_problem
print.couplr_flow_problem <- function(x, ...) {
  arcs <- x$arcs
  cat("\nFlow problem\n")
  cat("============\n\n")
  cat(sprintf("  Nodes: %d\n", x$n_nodes))
  cat(sprintf("  Arcs:  %d\n", nrow(arcs)))
  cat(sprintf("  Flow to place: %.0f units\n", sum(pmax(x$supply, 0))))
  if (nrow(arcs) > 0) {
    cat(sprintf("  Capacities: %.0f to %s\n", min(arcs$upper),
                if (any(is.infinite(arcs$upper))) "Inf"
                else sprintf("%.0f", max(arcs$upper))))
    cat(sprintf("  Lower bounds on %d arc(s)\n", sum(arcs$lower > 0)))
    cat(sprintf("  Costs: %.6g to %.6g\n", min(arcs$cost), max(arcs$cost)))
  }
  cat("\n")
  invisible(x)
}

# A warm start is aligned to the arc array of the problem it is handed to, and
# is only ever a starting point: the solver restores complementary slackness
# against the potentials before the first search, so a flow that no longer
# satisfies conservation costs augmentations rather than correctness. What it
# may not do is sit outside an arc bound, which is a shape error and not a
# starting point at all.
.flow_warm <- function(x, n, what) {
  if (is.null(x)) return(numeric(0))
  x <- as.numeric(x)
  if (!length(x)) return(numeric(0))
  if (length(x) != n) {
    stop("`", what, "` has length ", length(x), " but the problem has ", n,
         if (identical(what, "warm_flow")) " arcs." else " nodes.",
         call. = FALSE)
  }
  if (anyNA(x)) {
    stop("`", what, "` must not contain NA.", call. = FALSE)
  }
  x
}

# Solve a flow problem and keep the problem with the answer, so the flow, the
# potentials and the network they belong to travel together to the certificate.
#
# `warm_flow` and `warm_potential` start the solve from a known point instead of
# from the arc lower bounds and a relaxation pass. They are what makes a
# sequence of solves over one network cheap: a repricing moves the costs and
# leaves the topology, and an arc-bound edit moves one bound, so the previous
# answer is a near-optimal dual for the next problem. Both default to empty,
# which is the cold solve.
#
# `time_limit` bounds this one solve in seconds. The solver checks it between
# augmentations, so a solve that runs out comes back with status "interrupted",
# a flow inside every arc bound, and no claim about the problem. A user
# interrupt is checked at the same points and raises an R interrupt condition
# rather than returning, since a caller asking to stop is not asking for a
# partial answer.
.flow_solve <- function(problem, tol = 1e-12, relax_eps = 1e-18,
                        max_augmentations = 0, return_potentials = TRUE,
                        warm_flow = NULL, warm_potential = NULL,
                        time_limit = Inf) {
  problem <- .as_flow_problem(problem)
  if (!is.numeric(time_limit) || length(time_limit) != 1L ||
      is.na(time_limit) || time_limit < 0) {
    stop("`time_limit` must be a single non-negative number of seconds, or ",
         "Inf.", call. = FALSE)
  }
  res <- lap_flow_solve(problem$n_nodes, problem$supply,
                        problem$arcs$tail, problem$arcs$head,
                        problem$arcs$lower, problem$arcs$upper,
                        problem$arcs$cost,
                        tol = tol, relax_eps = relax_eps,
                        max_augmentations = .flow_count(max_augmentations,
                                                        "max_augmentations"),
                        return_potentials = return_potentials,
                        warm_flow = .flow_warm(warm_flow, nrow(problem$arcs),
                                               "warm_flow"),
                        warm_potential = .flow_warm(warm_potential,
                                                    problem$n_nodes,
                                                    "warm_potential"),
                        time_limit = as.numeric(time_limit))
  res$status <- .validate_status(res$status)
  res$problem <- problem
  structure(res, class = "flow_solve_result")
}

.flow_extract_flow <- function(x) {
  if (is.list(x) && !is.null(x$flow)) {
    return(as.numeric(x$flow))
  }
  if (is.numeric(x) && is.null(dim(x))) {
    return(as.numeric(x))
  }
  stop("`x` must be a solve result carrying a `flow` element, or a numeric ",
       "vector of arc flows.", call. = FALSE)
}

.new_flow_certificate <- function(report, tol) {
  # The C++ report indexes the worst violating arc from zero, and reports it as
  # -1 when there is none. Counts stay doubles because the model's expansion
  # budget is 2^31 arcs, one past what R's integer holds.
  report$worst_arc <- if (is.null(report$worst_arc) || report$worst_arc < 0) 0
                      else report$worst_arc + 1
  report$tolerance <- tol
  report$arithmetic <- if (isTRUE(report$conclusion_is_exact)) "exact" else "double"
  report$conclusion_is_exact <- NULL
  class(report) <- "flow_certificate"
  report
}

#' Verify that a flow is optimal
#'
#' Checks a flow and a set of node potentials against the linear-programming
#' optimality conditions for the minimum-cost flow problem, and returns the
#' result of each check. Unlike the `status` field on a solve result, which
#' records what the solver terminated on, this is a statement about the flow:
#' `certified_optimal` is `TRUE` only when every condition holds.
#'
#' The conditions are decided in one of two arithmetics, as they are by
#' [verify_assignment()], and the certificate says which. The **exact**
#' reading decides the sign of every residual arc's reduced cost with no
#' tolerance: the flow is integral, so conservation and the bounds are exact
#' already, and the duality gap, being the sum of the slackness violations
#' weighted by `|cbar|`, is then exactly zero. When the potentials supplied
#' pass the numerical reading but miss exactness, which on computed costs they
#' usually do by a unit in the last place, exact potentials are recovered from
#' the flow itself as the shortest-path distances of its residual graph, held
#' in exact multi-component arithmetic. A flow that is not optimal has none,
#' because its residual graph carries a negative cycle, so the recovery cannot
#' certify one, and potentials that fail even the numerical reading certify
#' nothing. The **numerical** reading makes the comparisons within a relative
#' tolerance and establishes optimality to that tolerance.
#'
#' The problem is
#'
#' \preformatted{
#'   min  sum_a cost(a) f(a)
#'   s.t. (flow out of v) - (flow into v) = supply(v)   at every node v
#'        lower(a) <= f(a) <= upper(a)                  on every arc a
#' }
#'
#' and, writing `cbar(a) = cost(a) + pi(tail(a)) - pi(head(a))` for the reduced
#' cost under the potentials `pi`, the conditions checked are primal
#' feasibility, `cbar(a) >= -tol(a)` on every arc that can still take flow, and
#' `cbar(a) <= tol(a)` on every arc carrying more than its lower bound. Their
#' objective form is checked too: the duality gap is the sum of the slackness
#' violations weighted by `|cbar|`, so it is where a violation too small to
#' trip the per-arc tolerance still accumulates.
#'
#' `tol` is relative. Each arc is compared against
#'
#' \preformatted{
#'   tol(a) = tol * max(1, |cost(a)|, |pi(tail(a))|, |pi(head(a))|)
#' }
#'
#' because `cbar(a)` is computed from those three numbers, and its last bits are
#' worth the largest of them times the machine epsilon. A design that stacks
#' lexicographic tier weights, as `cardinality_match()` does to rank cardinality
#' above balance above distance, reaches potentials in the millions, where one
#' unit in the last place is around 1e-9 and an exactly optimal flow cannot meet
#' an absolute 1e-9. The scale never falls below 1, so a problem whose costs and
#' potentials are of order 1 is checked against `tol` itself. The widest
#' tolerance any comparison used is reported as `dual_tolerance`.
#'
#' The check needs potentials. If `x` carries them, as a solve result does,
#' they are used. Otherwise they are obtained by solving `problem`, which costs
#' a second solve. Either way they are verified, not trusted: an arc priced
#' below the tolerance makes the verification fail rather than pass.
#'
#' Optimal potentials are shared by all optimal solutions of a linear program,
#' so a flow from one solver can be certified against potentials from another.
#'
#' @param x A `flow_solve_result`, or a numeric vector giving the flow on each
#'   arc in the order the arcs are listed.
#' @param problem The flow problem the flow belongs to: a `couplr_flow_problem`
#'   or a list with elements `n_nodes`, `supply` and `arcs`, where `arcs` is a
#'   data frame with columns `tail`, `head`, `lower`, `upper` and `cost` and
#'   node ids run from 1. Required unless `x` already carries one.
#' @param potential Optional numeric vector of node potentials, one per node.
#'   Overrides any potentials on `x`. It may also be a matrix with one row per
#'   node, as `exact_potential` is, in which case each potential is the exact
#'   sum of its row.
#' @param tol Relative tolerance for the feasibility and slackness comparisons.
#'   Each arc scales it by the largest of its cost and its two potentials, and
#'   the duality-gap comparison scales it by the magnitude of the objective,
#'   since a threshold below the resolution of the arithmetic that produced a
#'   number is not one a correct answer can meet. Ignored by an exact
#'   certificate.
#' @param arithmetic One of `"auto"`, `"exact"` or `"double"`, as for
#'   [verify_assignment()]. `"auto"`, the default, reports the exact
#'   conclusion when the exact conditions hold and the numerical one otherwise.
#'
#' @return An object of class `flow_certificate`, a list with elements:
#' \itemize{
#'   \item `certified_optimal` - logical, the conclusion. `TRUE` only when
#'         every condition below holds.
#'   \item `primal_feasible` - logical; every arc inside its bounds and every
#'         node's net flow equal to its supply.
#'   \item `n_capacity_violations`, `n_conservation_violations`,
#'         `max_conservation_error` - what primal feasibility failed on.
#'   \item `dual_feasible` - logical; no arc that can still take flow prices
#'         below its own `-tol(a)`.
#'   \item `complementary_slackness` - logical; no arc above its lower bound
#'         prices above its own `tol(a)`.
#'   \item `n_cs_violations`, `min_residual_reduced_cost`, `worst_arc` - the
#'         smallest reduced cost over the residual graph and the arc attaining
#'         it, which is a violation when it falls below that arc's `-tol(a)`.
#'         `worst_arc` is 0 when no arc can either take or give up flow.
#'   \item `dual_tolerance` - the widest `tol(a)` any comparison was made
#'         against, so the verdict names the resolution it was reached at.
#'   \item `primal_objective`, `dual_objective`, `duality_gap` - numeric.
#'   \item `arithmetic` - `"exact"` or `"double"`, the arithmetic the
#'         conclusion was reached in.
#'   \item `exact_available`, `exact_certificate` - whether the exact question
#'         was asked, and whether its conditions hold.
#'   \item `n_exact_violations` - residual arcs whose reduced cost has the
#'         wrong sign exactly, under the potentials given.
#'   \item `exact_potentials_source` - which potentials decided the exact
#'         certificate: `"solver"`, `"supplied"`, `"recovered"`, or `"none"`.
#'   \item `exact_potential` - on an exact certificate, the node potentials
#'         that decided it, one row per node whose sum is its exact value;
#'         `NULL` when there is no exact certificate.
#'   \item `tolerance` - the relative `tol` as supplied.
#' }
#'
#' @seealso [verify_assignment()], [solver_status_values()]
#'
#' @examples
#' # Two supply nodes shipping to two demand nodes, stated directly as a flow.
#' prob <- list(
#'   n_nodes = 4,
#'   supply  = c(2, 1, -2, -1),
#'   arcs = data.frame(
#'     tail  = c(1, 1, 2, 2),
#'     head  = c(3, 4, 3, 4),
#'     lower = c(0, 0, 0, 0),
#'     upper = c(2, 2, 2, 2),
#'     cost  = c(1, 3, 2, 1)
#'   )
#' )
#'
#' # Both of node 1's units go to node 3, node 2's unit goes to node 4.
#' verify_flow(c(2, 0, 0, 1), prob, potential = c(0, 0, 1, 1))
#'
#' # The same flow against potentials that certify nothing.
#' verify_flow(c(2, 0, 0, 1), prob, potential = c(0, 0, 0, 0))
#'
#' @export
verify_flow <- function(x, problem = NULL, potential = NULL, tol = 1e-9,
                        arithmetic = c("auto", "exact", "double")) {
  arithmetic <- match.arg(arithmetic)
  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol < 0) {
    stop("`tol` must be a single non-negative number.", call. = FALSE)
  }

  flow <- .flow_extract_flow(x)

  if (is.null(potential) && is.list(x) && !is.null(x$potential)) {
    potential <- as.numeric(x$potential)
  }
  if (is.null(problem) && is.list(x) && !is.null(x$problem)) {
    problem <- x$problem
  }
  if (is.null(problem)) {
    stop("`problem` is required: a certificate is a statement about a specific ",
         "flow problem, and a flow does not carry one.", call. = FALSE)
  }
  problem <- .as_flow_problem(problem)

  if (length(flow) != nrow(problem$arcs)) {
    stop("`x` has ", length(flow), " flow values but the problem has ",
         nrow(problem$arcs), " arcs.", call. = FALSE)
  }

  if (is.null(potential)) {
    potential <- .flow_solve(problem)$potential
  }
  exact_potential <- NULL
  if (is.matrix(potential)) {
    exact_potential <- potential
    storage.mode(exact_potential) <- "double"
    if (anyNA(exact_potential) || any(!is.finite(exact_potential))) {
      stop("`potential` must be finite.", call. = FALSE)
    }
    potential <- rowSums(exact_potential)
  }
  potential <- as.numeric(potential)
  if (length(potential) != problem$n_nodes) {
    stop("`potential` has length ", length(potential),
         " but the problem has ", problem$n_nodes, " nodes.", call. = FALSE)
  }

  report <- lap_flow_certify(problem$n_nodes, problem$supply,
                             problem$arcs$tail, problem$arcs$head,
                             problem$arcs$lower, problem$arcs$upper,
                             problem$arcs$cost, flow, potential, tol,
                             arithmetic, exact_potential)
  .new_flow_certificate(report, tol)
}

#' @param x A `flow_certificate` object.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
#' @method print flow_certificate
#' @rdname verify_flow
print.flow_certificate <- function(x, ...) {
  flag <- function(ok) if (isTRUE(ok)) "TRUE " else "FALSE"

  cat("Flow certificate\n")
  cat("================\n\n")
  cat(sprintf("  primal_feasible          %s\n", flag(x$primal_feasible)))
  cat(sprintf("    arcs outside bounds    %.0f\n", x$n_capacity_violations))
  cat(sprintf("    nodes out of balance   %.0f   (max error %.3e)\n",
              x$n_conservation_violations, x$max_conservation_error))
  cat(sprintf("  dual_feasible            %s\n", flag(x$dual_feasible)))
  cat(sprintf("  complementary_slackness  %s   (%.0f violating arc(s))\n",
              flag(x$complementary_slackness), x$n_cs_violations))
  cat(sprintf("  duality_gap              %.6e\n", x$duality_gap))
  cat(sprintf("  certified_optimal        %s\n", flag(x$certified_optimal)))
  if (identical(x$arithmetic, "exact")) {
    cat(sprintf("  arithmetic               exact, no tolerance (%s potentials)\n\n",
                if (is.null(x$exact_potentials_source)) "solver"
                else x$exact_potentials_source))
  } else {
    cat(sprintf("  arithmetic               double, tolerance %.1e\n\n",
                x$tolerance))
  }

  cat(sprintf("  primal objective  %.10g\n", x$primal_objective))
  cat(sprintf("  dual objective    %.10g\n", x$dual_objective))
  if (!isTRUE(x$dual_feasible) && x$worst_arc > 0) {
    cat(sprintf("  worst reduced cost %.3e at arc %.0f\n",
                x$min_residual_reduced_cost, x$worst_arc))
    cat(sprintf("  arc tolerance      %.3e\n", x$dual_tolerance))
  }

  invisible(x)
}
