# ==============================================================================
# Automatic solver dispatch
# ==============================================================================
# One rule table, read by assignment() to pick a solver and by
# explain_dispatch() to report why. Adding a rule is a list entry, not an edit
# to a branch chain that then has to be mirrored in the documentation.
# ==============================================================================

# Ordered; the first rule whose `test` holds wins. `condition` names the
# triggering property in the same terms as ?assignment, and `reason` states why
# that property favours the method.
#
# A rule earns its place by beating the default on the regime grid in
# paper/bench/bench_regimes.R, and two former rules did not. Sending a matrix
# with more than half its entries forbidden to `lapmod` was never the quickest
# choice at 60, 25, 5 or 1 percent of the entries finite, and sending a problem
# with at least three columns per row to `sap` was the quickest in none of the
# 32 cells where it fired, at a median of 5.75 times the cell's best. Jonker-
# Volgenant is at or under the best time in both regimes, so both properties
# now fall through to it. Either solver stays reachable by name.
.dispatch_rules <- list(
  list(
    id        = "tiny",
    method    = "bruteforce",
    condition = "at most 8 rows and 8 columns",
    reason    = "exact enumeration is cheaper than setting up a general solver",
    test      = function(n, m, probe) n <= 8 && m <= 8
  ),
  list(
    id        = "no_cost_scale",
    method    = "hk01",
    condition = "finite entries all equal, or all either 0 or 1",
    reason    = "there is no cost scale to exploit, so a cardinality algorithm suffices",
    test      = function(n, m, probe) isTRUE(probe$constant) || isTRUE(probe$binary)
  ),
  list(
    id        = "default",
    method    = "jv",
    condition = "no earlier rule applies",
    reason    = "fastest general-purpose solver measured across the regime grid",
    test      = function(n, m, probe) TRUE
  )
)

# Resolve `method = "auto"` against a probed cost matrix. Returns the winning
# rule plus every rule that was tested and did not fire, so a caller can report
# the decision without re-running the tests.
.dispatch_decision <- function(n, m, probe) {
  considered <- list()
  for (rule in .dispatch_rules) {
    fired <- isTRUE(rule$test(n, m, probe))
    considered[[length(considered) + 1L]] <- list(
      id = rule$id, method = rule$method,
      condition = rule$condition, reason = rule$reason, fired = fired
    )
    if (fired) {
      return(list(
        method     = rule$method,
        rule       = rule$id,
        condition  = rule$condition,
        reason     = rule$reason,
        considered = considered
      ))
    }
  }
  # Unreachable: the last rule tests TRUE.
  stop("No dispatch rule matched; the rule table has no catch-all.", call. = FALSE)
}

#' Explain which solver `method = "auto"` selects, and why
#'
#' `assignment(method = "auto")` picks a solver from a small set of rules
#' evaluated against one pass over the cost matrix. This reports the decision
#' without solving: which rule fired, the property that triggered it, the rules
#' that were tested first and did not fire, and the internal representation the
#' problem will be handed to the solver in.
#'
#' The same rule table drives the dispatch itself, so the report cannot drift
#' from the behaviour.
#'
#' @param cost Numeric matrix, as passed to [assignment()]. `NA` or `Inf`
#'   entries are treated as forbidden.
#' @param method Character scalar. `"auto"` (default) reports the automatic
#'   decision. Naming any other method reports that the choice was explicit and
#'   which rule `"auto"` would have picked instead.
#' @param maximize,cardinality,n_matches,unmatched_penalty As in [assignment()].
#'   A cardinality other than `"complete"` appends dummy columns before the
#'   solver sees the problem, which changes its shape and sparsity, so the rules
#'   are evaluated against the same matrix `assignment()` would hand the solver.
#'
#' @return An object of class `dispatch_explanation`, a list with elements:
#' \itemize{
#'   \item `method` - the solver that will run.
#'   \item `explicit` - `TRUE` when `method` was named rather than dispatched.
#'   \item `rule`, `condition`, `reason` - the rule that fired under `"auto"`.
#'   \item `considered` - data frame of every rule tested, in order, with
#'         whether it fired.
#'   \item `n`, `m`, `transposed` - problem shape, and whether the solver sees
#'         the transpose. Rectangular problems are transposed internally so the
#'         solver always has at least as many columns as rows.
#'   \item `probe` - the single-pass summary the rules read.
#' }
#'
#' @seealso [assignment()]
#'
#' @examples
#' explain_dispatch(matrix(runif(400), 20, 20))
#' explain_dispatch(matrix(sample(0:1, 400, TRUE), 20, 20))
#'
#' @export
explain_dispatch <- function(cost, method = "auto", maximize = FALSE,
                             cardinality = c("complete", "maximum", "fixed"),
                             n_matches = NULL, unmatched_penalty = NULL) {
  cost <- as.matrix(cost)
  if (!is.numeric(cost)) {
    stop("`cost` must be a numeric matrix, got ", typeof(cost))
  }
  n <- nrow(cost)
  m <- ncol(cost)
  if (n == 0 || m == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  # Same reduction assignment() applies, so the rules are read off the matrix
  # the solver actually receives.
  card <- .validate_cardinality_args(cardinality, n_matches, unmatched_penalty,
                                     n, m)
  reduction <- .cardinality_reduction(cost, card$cardinality, card$n_matches,
                                      card$unmatched_penalty, maximize)
  solve_cost <- reduction$work
  n <- nrow(solve_cost)
  m <- ncol(solve_cost)

  probe <- lap_probe_cost_matrix(solve_cost)
  if (probe$has_nan) stop("NaN not allowed in `cost`")

  decision <- .dispatch_decision(n, m, probe)

  considered <- do.call(rbind, lapply(decision$considered, function(r) {
    data.frame(rule = r$id, method = r$method, condition = r$condition,
               reason = r$reason, fired = r$fired, stringsAsFactors = FALSE)
  }))

  explicit <- !identical(method, "auto")
  out <- list(
    method     = if (explicit) method else decision$method,
    explicit   = explicit,
    auto_method = decision$method,
    rule       = decision$rule,
    condition  = decision$condition,
    reason     = decision$reason,
    considered  = considered,
    n           = n,
    m           = m,
    transposed  = n > m,
    cardinality = card$cardinality,
    n_dummy     = reduction$n_dummy,
    probe       = probe
  )
  class(out) <- "dispatch_explanation"
  out
}

#' @param x A `dispatch_explanation` object.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
#' @method print dispatch_explanation
#' @rdname explain_dispatch
print.dispatch_explanation <- function(x, ...) {
  cat("Solver dispatch\n")
  cat("===============\n\n")
  cat(sprintf("Problem:  %d x %d", x$n, x$m))
  if (isTRUE(x$n_dummy > 0L)) {
    cat("  (", x$n_dummy, " dummy columns for cardinality = \"", x$cardinality,
        "\")", sep = "")
  }
  if (x$transposed) cat("  (transposed to ", x$m, " x ", x$n, " before solving)", sep = "")
  cat("\n")

  if (x$explicit) {
    cat("Selected: ", x$method, " (named explicitly; no rule was consulted)\n", sep = "")
    cat("Under method = \"auto\" the choice would be ", x$auto_method,
        ", because ", x$condition, ".\n", sep = "")
  } else {
    cat("Selected: ", x$method, "\n", sep = "")
    cat("Rule:     ", x$rule, " -- ", x$condition, "\n", sep = "")
    cat("Why:      ", x$reason, "\n", sep = "")
  }

  cat("\nRules tested, in order:\n")
  for (i in seq_len(nrow(x$considered))) {
    row <- x$considered[i, ]
    cat(sprintf("  [%s] %-16s %-8s %s\n",
                if (row$fired) "x" else " ",
                row$rule, row$method, row$condition))
  }

  cat("\nProbe: ", format(x$probe$n_nonfinite, scientific = FALSE), " of ",
      format(x$probe$n_total, scientific = FALSE), " entries non-finite",
      if (isTRUE(x$probe$constant)) ", constant" else "",
      if (isTRUE(x$probe$binary)) ", binary" else "",
      "\n", sep = "")

  invisible(x)
}
