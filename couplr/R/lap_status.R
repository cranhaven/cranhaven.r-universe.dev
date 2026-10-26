# ==============================================================================
# Solver status
# ==============================================================================
# A result carries two different claims, and they are kept apart.
#
#   status       what the solver terminated on. Derived from the solver's own
#                termination state, and it never asserts more than the solver
#                knows.
#   certificate  a checked statement about the answer, present only when one
#                was taken. The only place optimality is asserted rather than
#                reported, and it names the arithmetic it was decided in.
#
# An exact solver reaching its own optimality condition may report
# status = "optimal". Only a verified certificate may report
# certified_optimal = TRUE. See verify_assignment().
# ==============================================================================

#' Solver status values
#'
#' The closed set of values a couplr solve result's `status` field can take.
#'
#' \describe{
#'   \item{`"optimal"`}{The solver terminated on its own optimality condition
#'     and every row it was asked to match is matched.}
#'   \item{`"partial"`}{Feasible, but fewer rows are matched than requested,
#'     under a maximum-cardinality-then-minimum-cost objective.}
#'   \item{`"infeasible"`}{No assignment of the requested cardinality exists.}
#'   \item{`"eps_optimal"`}{An auction solve terminated at a caller-supplied
#'     epsilon. The result is within `n * epsilon` of optimal, which is a bound,
#'     not a certificate.}
#'   \item{`"iteration_limit"`}{The solver stopped on an iteration cap rather
#'     than on optimality. The solution is feasible; its optimality is unproven.}
#'   \item{`"interrupted"`}{The solver stopped on a time budget or a user
#'     interrupt, part way through. What it had placed respects every capacity,
#'     and is short of what the problem asked it to place, so it is neither an
#'     answer nor evidence that no answer exists.}
#'   \item{`"heuristic"`}{Produced by a heuristic. Optimality is neither claimed
#'     nor checked.}
#' }
#'
#' @return Character vector of the permitted status values.
#'
#' @seealso [verify_assignment()] to turn a status into a checked certificate.
#'
#' @examples
#' solver_status_values()
#'
#' @export
solver_status_values <- function() {
  c("optimal", "partial", "infeasible", "eps_optimal",
    "iteration_limit", "interrupted", "heuristic")
}

# Reject anything outside the vocabulary at the point of construction, so an
# invented status is a build-time failure in the test suite rather than a string
# that silently reaches a user.
.validate_status <- function(status) {
  if (!is.character(status) || length(status) != 1L || is.na(status)) {
    stop("`status` must be a single non-NA character string.", call. = FALSE)
  }
  if (!status %in% solver_status_values()) {
    stop("Unknown solver status \"", status, "\"; expected one of ",
         paste(solver_status_values(), collapse = ", "), ".", call. = FALSE)
  }
  status
}

# Derive the status of a completed assignment solve.
#
# `solver_status` is a status the solver computed for itself. It describes how
# the solver's loop stopped, and that fact survives whatever reduction was
# applied around it, so anything short of optimality is authoritative and passes
# through untouched. `"optimal"` is different: it is a claim about the problem
# the solver was handed, which under a cardinality reduction is a padded one
# where a complete matching always exists. The cardinality of the restored
# matching still has to be checked, or a solve that leaves rows unmatched
# reports optimality nobody computed for that problem.
#
# `n_required` is the number of rows the requested cardinality demands.
.compute_solve_status <- function(match, n_required, method,
                                  solver_status = NULL,
                                  auction_eps = NULL) {
  if (!is.null(solver_status)) {
    solver_status <- .validate_status(solver_status)
    if (!identical(solver_status, "optimal")) {
      return(solver_status)
    }
  }

  n_matched <- sum(match > 0L)
  if (n_matched < n_required) {
    return("partial")
  }

  # Epsilon-scaling is what lifts the auction from eps-optimal to exact, and it
  # is driven to its terminal epsilon only when the caller does not fix one.
  # A caller-supplied epsilon leaves a duality-gap slack of up to n * eps, so
  # the result is bounded, not proven optimal.
  if (!is.null(auction_eps) &&
      method %in% c("auction", "auction_gs", "auction_scaled")) {
    return("eps_optimal")
  }

  "optimal"
}

# Derive the status of a matching produced by match_couples().
#
# `solver` is info$solver, which carries the method the C++ layer actually ran,
# one entry per solve: a scalar for a single match, one per block for a blocked
# one. That is the only record that the constrained path fell back to greedy:
# the caller asked for an optimal method, the admissible graph admitted no
# complete matching and the cost range was too wide to pad exactly, and
# method_used comes back "greedy_sorted". A warning already says so; this puts
# it on the object. One block falling back is enough to make the whole matching
# heuristic, because the pairs that block contributed are not the optimal ones.
#
# `n_requested` is how many pairs the design asked for: the left unit count
# times the requested ratio. Deriving the shortfall from unmatched left units
# instead reports a k:1 match optimal as soon as every unit holds one partner,
# however many of its requested partners are missing, because n_matched counts
# pairs while unmatched counts units.
.matching_status <- function(solver, greedy, n_pairs, n_requested) {
  if (isTRUE(greedy)) {
    return("heuristic")
  }
  solver <- solver[!is.na(solver)]
  if (length(solver) > 0L && any(startsWith(solver, "greedy"))) {
    return("heuristic")
  }
  if (n_pairs == 0L) {
    return("infeasible")
  }
  if (n_pairs < n_requested) {
    return("partial")
  }
  "optimal"
}

# Single constructor for the object assignment() and its lazy twin return, so
# the four assembly sites cannot drift apart in field set, class, or status
# vocabulary.
.new_lap_solve_result <- function(match, total_cost, status, method_used,
                                  dispatch = NULL, certificate = NULL) {
  out <- list(
    match       = as.integer(match),
    total_cost  = as.numeric(total_cost),
    status      = .validate_status(status),
    method_used = method_used
  )
  if (!is.null(dispatch))    out$dispatch    <- dispatch
  if (!is.null(certificate)) out$certificate <- certificate
  class(out) <- "lap_solve_result"
  out
}
