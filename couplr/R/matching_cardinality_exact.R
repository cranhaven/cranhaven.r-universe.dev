# ==============================================================================
# Balance-constrained cardinality matching
# ==============================================================================
# The largest matched sample meeting a balance requirement is the optimum of the
# flow problem built in matching_balance_flow.R whenever every part of that
# requirement is categorical. This file is the driver for the case where it is
# not: a linear moment constraint on the matched sample has no place in a flow
# network, so it is the only thing relaxed, and everything else stays inside the
# network where it is solved exactly and certified.
#
# Write x for the arc flows of the balance network, so that
#
#     c(x) = P * (K - pairs) + sum_h eps_h * imbalance_h + sum_ij d_ij x_ij
#
# is the lexicographic objective the network encodes, and A x <= 0 for the
# moment rows of matching_moments.R, one row per one-sided bound. The problem
# solved here is
#
#     Z* = min { c(x) : x a flow of the network, A x <= 0 }.
#
# Relaxation. For multipliers lambda >= 0 the Lagrangian
#
#     L(lambda) = min { c(x) + lambda' A x : x a flow of the network }
#
# is a flow problem again: a^r_ij = u^r_i - w^r_j - b^r decomposes additively,
# so lambda touches nothing but the cost of the pair arcs, and the network's
# topology, bounds and every other arc cost are the ones already validated.
# For any x feasible for the original problem, A x <= 0 and lambda >= 0 give
# lambda' A x <= 0, hence L(lambda) <= c(x) and therefore L(lambda) <= Z*. The
# right-hand side of the moment rows is zero, so nothing is subtracted from that
# bound. It holds for every lambda >= 0, so the best bound a node knows is the
# largest L it has seen, and the multipliers are moved toward it by projected
# subgradient ascent, the subgradient of L at lambda being the vector of
# constraint values at the relaxed optimum.
#
# Cardinality from a bound. The objective separates as Z = P * q + r with
# q = S + T the units of slack and transfer, an integer, and
#
#     r = sum_h eps_h * imbalance_h + sum_ij d_ij x_ij,
#     0 <= r <= r_max = P - 1,
#
# the upper bound being what .lex_tier_weights() builds P to exceed. A lower
# bound Z_LB on Z* therefore bounds q from below by ceiling((Z_LB - r_max) / P),
# and since pairs = K - q,
#
#     best_possible = K - max(0, ceiling((Z_LB - r_max) / P))
#
# is an integer-valued upper bound on the cardinality of any feasible matched
# sample. The gap it forms against an incumbent is exact rather than estimated:
# one unit of cardinality is worth P, and P is larger than everything the rest
# of the objective can accumulate.
#
# Branching. A node is the same network with a few arc bounds fixed. Branching
# on the inclusion of left unit i sets the upper bound of its unit arc to 0 in
# one child and the lower bound to 1 in the other; branching on a pair sets the
# bounds of that pair arc the same way. Both children are single arc-bound edits
# on one network and their union covers the parent, so the tree is exhaustive.
# Each branch fixes an arc that was free, and a node whose pair arcs are all
# fixed has its matched set determined, so the tree is finite: unit branching
# hands over to pair branching once every left unit is decided.
#
# Root incumbent. A bound prunes nothing until something feasible is known, and
# the empty matched set leaves every bound below it, so the root cuts a matched
# set out of the relaxed optimum at zero multipliers before the first node is
# opened: pairs come off it until every moment row holds. It counts as an
# incumbent only once those rows are recomputed on the pairs themselves, which
# is what makes c(incumbent) an upper bound on Z* and lets the search prune
# against it.
#
# Certification. The solver is handed the repriced costs rounded to doubles, so
# its optimum speaks for the rounding and not for L(lambda). A node's bound is
# read instead from weak duality: for any node potentials pi, the dual
# objective D(pi) of the network under the exact repriced costs is at most
# L(lambda). It is evaluated exactly in src/flow/flow_lagrangian.h, at the
# flow's own exact potentials when the flow is exactly optimal and at the
# solver's otherwise, so every finished solve proves a bound and none rests on
# a tolerance. The bounds, the incumbent's objective and the comparisons
# between them are held and decided exactly, and so is the cardinality read off
# the bound. The incumbent is a matched set read back from a flow whose moment
# rows are recomputed on its pairs and satisfied exactly, so it is feasible for
# every stated constraint at every point in the search, including every early
# stop.
# ==============================================================================

# Generation. A search over a specification rather than a matrix solves every
# node over the pairs it has generated so far. The network always carries its
# full budget, since a category that cannot fill it with pairs pays slack, so a
# node's solve is always a flow and the only question about the pairs it omits is
# price: a pair arc left out sits at its lower bound of zero, so the solve is
# optimal for the network holding every pair once no omitted pair prices below
# zero against its potentials. The multipliers decompose into a row part and a
# column part, so they fold into those potentials and the pricer reads the
# distances as they are. The potentials are the ones the node's bound is read
# at, held exactly, and a pair prices in when its reduced cost is exactly
# negative, so once none does the omitted pairs add exactly nothing to that
# bound. Pairs that price in are appended to the network and the node is solved
# again, warm; the arcs already there keep their indices, so every bound the
# search placed on one stands.

# The reasons the node loop stops, in the order they are checked. Anything
# outside this set is a name nobody defined, and is refused where it is written
# rather than where it is read.
.CARDINALITY_STOPS <- c("optimality", "bound", "node_limit", "time_limit",
                        "interrupt")

.validate_cardinality_stop <- function(stopped_on) {
  if (!is.character(stopped_on) || length(stopped_on) != 1L ||
      is.na(stopped_on) || !stopped_on %in% .CARDINALITY_STOPS) {
    stop("Unknown stopping reason \"", stopped_on, "\"; expected one of ",
         paste(.CARDINALITY_STOPS, collapse = ", "), ".", call. = FALSE)
  }
  stopped_on
}

# A node problem is the network and the index that reads it. Accepts either the
# pair .balance_flow_problem() returns or the two objects separately.
.cardinality_as_node <- function(problem, index = NULL) {
  if (is.null(index)) {
    if (is.list(problem) && !is.null(problem$problem) &&
        !is.null(problem$index)) {
      return(list(problem = .as_flow_problem(problem$problem),
                  index = problem$index))
    }
    stop("`index` is required unless `problem` carries one, as the pair ",
         "`.balance_flow_problem()` returns does.", call. = FALSE)
  }
  list(problem = .as_flow_problem(problem), index = index)
}

# The empty set of branching decisions.
.cardinality_no_edits <- function() {
  list(arc = integer(0), lower = numeric(0), upper = numeric(0))
}

# One more arc pinned. An arc is edited once and never revisited, so a node's
# edits never disagree with each other.
.cardinality_add_edit <- function(edits, arc, lower, upper) {
  list(arc = c(edits$arc, as.integer(arc)),
       lower = c(edits$lower, as.numeric(lower)),
       upper = c(edits$upper, as.numeric(upper)))
}

# The network with a node's arc bounds in place. Costs are untouched here; the
# multipliers are applied separately so that the same node can be solved at
# several lambda without rebuilding its bounds.
.cardinality_edit_problem <- function(problem, edits) {
  if (is.null(edits) || !length(edits$arc)) {
    return(problem)
  }
  arcs <- problem$arcs
  arcs$lower[edits$arc] <- edits$lower
  arcs$upper[edits$arc] <- edits$upper
  .flow_problem(problem$n_nodes, problem$supply, arcs)
}

# A previous solve's flow, made usable as a starting point for this one. The
# network is the same one throughout a search, so a flow stays aligned to it;
# what moves is an arc bound, and the arc a branching decision pinned is exactly
# the one the previous flow can sit outside of. Clamping is what the starting
# point is for: the solver repairs the conservation that breaks, and a flow
# outside an arc bound is not a starting point it can repair from.
.cardinality_warm_flow <- function(flow, problem) {
  if (is.null(flow) || !length(flow)) {
    return(NULL)
  }
  arcs <- problem$arcs
  if (length(flow) > nrow(arcs)) {
    return(NULL)
  }
  # Pairs generated since the flow was solved sit on the end of the arc list,
  # and a pair nobody placed flow on starts at its lower bound.
  if (length(flow) < nrow(arcs)) {
    flow <- c(flow, arcs$lower[seq.int(length(flow) + 1L, nrow(arcs))])
  }
  pmin(pmax(flow, arcs$lower), arcs$upper)
}

# The index a search should read now. A generating search appends pairs as it
# goes, and the index handed to it at the start does not hold them.
.cardinality_current_index <- function(index) {
  if (is.null(index$gen)) index else index$gen$built$index
}

# A solve's node potentials read in distance terms, with the multipliers folded
# in. A pair arc costs d - shift + sum_r lambda_r (u_ri - w_rj - b_r) and
# reduces by pi(left_i) - pi(right_j), which is d - u_i - v_j for the u and v
# returned here: the reduced cost of a pair is its distance less the two.
.cardinality_pair_duals <- function(index, potential, coefs, lambda) {
  layout <- index$layout
  pi_left <- potential[layout$node_left(seq_len(index$n_left))]
  pi_right <- potential[layout$node_right(seq_len(index$n_right))]
  row_part <- numeric(index$n_left)
  col_part <- numeric(index$n_right)
  coefs <- .as_moment_coefficient_list(coefs)
  for (r in seq_along(coefs)) {
    if (is.null(lambda) || lambda[[r]] == 0) next
    row_part <- row_part + lambda[[r]] * (coefs[[r]]$u - coefs[[r]]$b)
    col_part <- col_part - lambda[[r]] * coefs[[r]]$w
  }
  list(u = index$cost_shift - row_part - pi_left,
       v = pi_right - col_part)
}

# Exact values. A number the search prunes or accepts on is held as the
# components of an expansion (src/core/lap_exact.h), whose exact sum is the
# number, or as -Inf or Inf for a node with no bound yet and a node holding
# nothing. The sign of a difference of two of them is decided exactly.
.exact_cmp <- function(a, b) {
  inf_a <- length(a) == 1L && is.infinite(a)
  inf_b <- length(b) == 1L && is.infinite(b)
  if (inf_a && inf_b) {
    return(if (a == b) 0L else if (a > b) 1L else -1L)
  }
  if (inf_a) {
    return(if (a > 0) 1L else -1L)
  }
  if (inf_b) {
    return(if (b > 0) -1L else 1L)
  }
  lap_exact_compare(a, b)
}

.exact_min <- function(values) {
  out <- Inf
  for (v in values) {
    if (.exact_cmp(v, out) < 0) out <- v
  }
  out
}

.exact_max <- function(a, b) {
  if (.exact_cmp(a, b) >= 0) a else b
}

# The double on the side of an exact value the caller names.
.exact_value <- function(x, direction = c("down", "up")) {
  direction <- match.arg(direction)
  if (length(x) == 1L && is.infinite(x)) {
    return(x)
  }
  lap_exact_round(x, direction)
}

# The moment rows as the exact routines read them: one column of u and of w per
# row, and one b per row.
.moment_matrices <- function(coefs, n_left, n_right) {
  coefs <- .as_moment_coefficient_list(coefs)
  if (!length(coefs)) {
    return(list(u = matrix(0, n_left, 0L), w = matrix(0, n_right, 0L),
                b = numeric(0)))
  }
  list(u = matrix(as.numeric(unlist(lapply(coefs, `[[`, "u"))), n_left),
       w = matrix(as.numeric(unlist(lapply(coefs, `[[`, "w"))), n_right),
       b = vapply(coefs, function(cf) as.numeric(cf$b), numeric(1)))
}

# The moment rows on a matched set, decided exactly: the sign of each row's
# value, zero or below being satisfied, and the sign of their sum weighted by
# the multipliers.
.cardinality_rows_exact <- function(index, coefs, read, lambda = NULL) {
  coefs <- .as_moment_coefficient_list(coefs)
  if (!length(coefs)) {
    return(list(sign = integer(0), weighted_sign = 0L))
  }
  mats <- .moment_matrices(coefs, index$n_left, index$n_right)
  lambda <- if (is.null(lambda)) numeric(length(coefs)) else as.numeric(lambda)
  lap_exact_moment_rows(mats$u, mats$w, mats$b, as.integer(read$left),
                        as.integer(read$right), lambda)
}

# One solve of the network read exactly at `lambda`: the bound D(pi) it proves
# on the node's Lagrangian, whether its potentials were recovered exactly, and
# the pair potentials the omitted pairs are priced against. See
# src/flow/flow_lagrangian.h.
.cardinality_step <- function(problem, index, solved, coefs, lambda) {
  mats <- .moment_matrices(coefs, index$n_left, index$n_right)
  arcs <- problem$arcs
  layout <- index$layout
  lap_flow_lagrangian_step(
    problem$n_nodes, problem$supply, arcs$tail, arcs$head, arcs$lower,
    arcs$upper, arcs$cost, as.numeric(solved$flow),
    as.numeric(solved$potential), as.integer(index$ranges$pair),
    as.integer(index$pair_left), as.integer(index$pair_right),
    as.integer(layout$node_left(seq_len(index$n_left))),
    as.integer(layout$node_right(seq_len(index$n_right))),
    mats$u, mats$w, mats$b, as.numeric(lambda))
}

# Seconds left of a budget stated as an absolute elapsed time, which is what a
# sequence of solves shares. Inf is no budget.
.cardinality_remaining <- function(deadline) {
  if (!is.finite(deadline)) {
    return(Inf)
  }
  max(0, deadline - proc.time()[["elapsed"]])
}

# Pair-arc costs under the multipliers. The additive decomposition means one
# vectorized pass per moment row over the admissible pairs, and no object of
# size n by m is built.
.cardinality_repriced_cost <- function(index, base_cost, coefs, lambda) {
  coefs <- .as_moment_coefficient_list(coefs)
  if (!length(coefs) || is.null(lambda) || all(lambda == 0)) {
    return(base_cost)
  }
  pair <- index$ranges$pair
  if (!length(pair)) {
    return(base_cost)
  }
  base_cost[pair] <- .moment_reprice(
    list(i = index$pair_left, j = index$pair_right, cost = index$pair_cost),
    coefs, lambda)
  base_cost
}

# A shortfall against the supply means no feasible flow exists at all. The
# solver splits that outcome by whether it managed to place any flow before
# running out of augmenting paths, so both halves of the split say the same
# thing about the node: it holds no matched set.
.cardinality_no_flow <- function(status) {
  status %in% c("infeasible", "partial")
}

# Every check the audit makes, as one answer.
.cardinality_audit_ok <- function(audit) {
  isTRUE(audit$feasible) &&
    abs(audit$identity_gap) <= 1e-6 * max(1, abs(audit$arc_cost)) &&
    abs(audit$pairs_identity_gap) <= 1e-9 &&
    audit$n_self_crossing == 0
}

#' One solve of a balance network
#'
#' Applies a node's arc bounds and multipliers, solves the network, reads the
#' solve exactly, audits the flow against the objective identity the design
#' encodes, and reads the matched set back.
#'
#' The solver is handed the multiplier-repriced costs rounded to doubles, and
#' the exact reading is taken against the costs themselves: the bound it
#' returns is D(pi) of the node's Lagrangian at the solve's potentials, which
#' weak duality makes a lower bound on the node whether or not the solve was
#' optimal, and which is the solve's exact Lagrangian value when it was. A
#' generating search prices the pairs it omits against the same potentials,
#' at zero and exactly, and adds any that price below until none do, so the
#' bound holds over every admissible pair.
#'
#' @param problem The network, or the pair `.balance_flow_problem()` returns.
#' @param index The network's index, unless `problem` carries one.
#' @param coefs Moment coefficients, one per one-sided row.
#' @param lambda Multipliers, one per row.
#' @param edits The node's arc-bound decisions.
#' @param cost Optional distance matrix for the audit.
#' @param warm Optional `list(flow, potential)` from an earlier solve of the
#'   same network, used as the solver's starting point.
#' @param time_limit Seconds this one solve may run.
#'
#' @return A list with the solve status, the flow and potentials, the pair
#'   potentials in distance terms with the multipliers folded in
#'   (`pair_duals`, see `.cardinality_pair_duals()`), `bound` and
#'   `bound_exact`, the node bound the solve proves, rounded down and exact,
#'   `certified`, whether the flow is exactly optimal for the node's
#'   Lagrangian over every admissible pair, the audit, the matched set, and the
#'   true objective as `objective_exact` and as `objective`, rounded up. A
#'   solve that ran out of time comes back with status `"interrupted"`, its
#'   flow and potentials, and nothing else: it proved neither an optimum nor
#'   the absence of one, so reading and auditing it would be work spent on a
#'   number no caller may read.
#' @keywords internal
.cardinality_flow <- function(problem, index = NULL, coefs = NULL,
                              lambda = NULL, edits = NULL, cost = NULL,
                              warm = NULL, time_limit = Inf) {
  node <- .cardinality_as_node(problem, index)
  gen <- node$index$gen
  if (!is.null(gen)) {
    node <- gen$built
  }
  coefs <- .as_moment_coefficient_list(coefs)
  lambda <- if (is.null(lambda)) numeric(length(coefs)) else as.numeric(lambda)
  started <- proc.time()[["elapsed"]]
  step <- NULL

  repeat {
    index <- node$index
    base <- .cardinality_edit_problem(node$problem, edits)

    arc_cost <- .cardinality_repriced_cost(index, base$arcs$cost, coefs, lambda)
    solve_problem <- base
    if (!identical(arc_cost, base$arcs$cost)) {
      solve_problem$arcs$cost <- arc_cost
    }

    remaining <- if (is.finite(time_limit)) {
      max(0, time_limit - (proc.time()[["elapsed"]] - started))
    } else {
      Inf
    }
    solved <- .flow_solve(solve_problem,
                          warm_flow = .cardinality_warm_flow(warm$flow,
                                                             solve_problem),
                          warm_potential = warm$potential,
                          time_limit = remaining)
    if (identical(solved$status, "interrupted")) {
      return(list(status = "interrupted",
                  flow = as.numeric(solved$flow),
                  potential = as.numeric(solved$potential),
                  integral = FALSE,
                  bound = -Inf,
                  bound_exact = -Inf,
                  certified = FALSE,
                  audit = NULL,
                  audit_ok = FALSE,
                  read = NULL,
                  objective = NA_real_,
                  objective_exact = Inf,
                  problem = base,
                  index = index))
    }
    if (.cardinality_no_flow(solved$status)) {
      step <- NULL
      break
    }
    step <- .cardinality_step(base, index, solved, coefs, lambda)
    # A pinned node bars every pair it does not use, the ones never generated
    # included, so its omitted pairs sit at an upper bound of zero and add
    # nothing to the bound whatever they price at.
    if (is.null(gen) || isTRUE(edits$closed)) {
      break
    }
    priced <- lap_pricing_price_exact(gen$session, step$pair_u, step$pair_v,
                                      index$cost_shift, gen$keep_per_row)
    if (!length(priced$i)) {
      break
    }
    gen$built <- .balance_add_pairs(gen$built, priced$i, priced$j,
                                    lap_pricing_cost(gen$session, priced$i,
                                                     priced$j))
    node <- gen$built
    warm <- list(flow = step$flow, potential = step$potential)
  }
  # The step's flow is the solver's with any negative cycle the exact costs
  # showed cancelled, and its potentials are the ones the bound was read at,
  # rounded, so both replace the solver's from here on.
  flow <- if (is.null(step)) as.numeric(solved$flow) else step$flow
  potential <- if (is.null(step)) as.numeric(solved$potential) else step$potential
  read <- .balance_flow_read(index, flow)
  audit <- .balance_flow_audit(base, index, flow, cost = cost,
                               tiers = index$tiers)
  objective_exact <- lap_exact_dot(base$arcs$cost, flow)

  list(status = solved$status,
       flow = flow,
       potential = potential,
       pair_duals = .cardinality_pair_duals(index, potential, coefs, lambda),
       integral = all(flow == round(flow)),
       bound = if (is.null(step)) -Inf else step$bound,
       bound_exact = if (is.null(step)) -Inf else step$bound_exact,
       certified = !is.null(step) && isTRUE(step$recovered),
       audit = audit,
       audit_ok = .cardinality_audit_ok(audit),
       read = read,
       objective = .exact_value(objective_exact, "up"),
       objective_exact = objective_exact,
       problem = base,
       index = index)
}

# The value of every moment row on a matched set. Zero or below is satisfied.
.cardinality_violations <- function(coefs, read) {
  coefs <- .as_moment_coefficient_list(coefs)
  if (!length(coefs)) {
    return(numeric(0))
  }
  vapply(coefs, function(cf) .moment_violation(cf, read$left, read$right),
         numeric(1))
}

#' Lagrangian bound for one node
#'
#' Solves the node's network at a sequence of multipliers, each step one
#' `.flow_solve()` read exactly by `.cardinality_flow()`, and returns the
#' largest bound the steps proved. Multipliers move along the projected
#' subgradient with step `t_0 / (1 + k)`, warm-started from whatever `lambda`
#' is handed in, which is the parent's best set during a search.
#'
#' Consecutive steps solve one network. The topology, the arc bounds and every
#' non-pair cost are the ones the previous step solved; only the pair costs
#' moved, by one multiplier step, so the previous step's flow and potentials are
#' a near-optimal starting point for this one and are carried across. `warm`
#' gives the first step the same footing, from whichever solve the caller has
#' to hand.
#'
#' @param problem The node's network, or the pair `.balance_flow_problem()`
#'   returns.
#' @param coefs Moment coefficients, one per one-sided row.
#' @param lambda Starting multipliers, one per row.
#' @param steps How many multiplier updates to take.
#' @param index The network's index, unless `problem` carries one.
#' @param edits The node's arc-bound decisions.
#' @param incumbent The best objective known, which sets the step scale.
#' @param step0 An explicit `t_0`, overriding that scale.
#' @param cost Optional distance matrix for the audit.
#' @param warm Optional `list(flow, potential)` to start the first solve from.
#' @param deadline Elapsed time, on `proc.time()`'s clock, past which a solve
#'   stops where it stands. `Inf` is no budget.
#'
#' @return A list with `bound` and `bound_exact`, the best bound rounded down
#'   and exact, the multipliers that attained it, the relaxed solve at those
#'   multipliers, any moment-feasible solutions the ascent passed through,
#'   `certified`, whether any step proved a bound, and `warm`, the last
#'   complete solve's flow and potentials. A `status` of `"interrupted"` means
#'   a solve ran out of time; the bound and the solutions reported alongside it
#'   came from the steps that finished, and are as valid as any others.
#' @keywords internal
.cardinality_lagrangian <- function(problem, coefs = NULL, lambda = NULL,
                                    steps = 20L, index = NULL, edits = NULL,
                                    incumbent = Inf, step0 = NULL,
                                    cost = NULL, warm = NULL,
                                    deadline = Inf) {
  node <- .cardinality_as_node(problem, index)
  coefs <- .as_moment_coefficient_list(coefs)
  n_rows <- length(coefs)

  lambda <- if (is.null(lambda)) {
    numeric(n_rows)
  } else {
    pmax(0, as.numeric(lambda))
  }
  if (length(lambda) != n_rows) {
    stop("`lambda` must hold one multiplier per moment row; got ",
         length(lambda), " for ", n_rows, " rows.", call. = FALSE)
  }
  steps <- max(1L, as.integer(steps))
  if (!n_rows) {
    steps <- 1L
  }

  bound <- -Inf
  bound_exact <- -Inf
  best_lambda <- lambda
  best_solve <- NULL
  solutions <- list()
  certified <- FALSE
  status <- "optimal"
  n_solves <- 0L
  t0 <- step0

  for (k in seq_len(steps)) {
    fl <- .cardinality_flow(node$problem, node$index, coefs = coefs,
                            lambda = lambda, edits = edits, cost = cost,
                            warm = warm,
                            time_limit = .cardinality_remaining(deadline))
    n_solves <- n_solves + 1L

    if (identical(fl$status, "interrupted")) {
      # The steps that finished stand: each one certified its own relaxed
      # optimum against the network it was solved on, and none of that depends
      # on the step that did not finish. What the caller loses is the rest of
      # the ascent, and what it must not do is read this solve.
      status <- "interrupted"
      break
    }

    if (.cardinality_no_flow(fl$status)) {
      # The node's forced arcs admit no flow meeting the budget, so it holds no
      # matched set.
      return(list(bound = Inf, lambda = lambda, relaxed = fl,
                  solutions = list(), certified = TRUE, status = "infeasible",
                  n_solves = n_solves, warm = warm))
    }

    # A finished solve of this network is the next one's starting point,
    # whether or not it was optimal: the flow sits inside every arc bound and
    # the potentials price the costs this step used, which is all a starting
    # point has to be.
    warm <- list(flow = fl$flow, potential = fl$potential)

    # Every finished solve proves a bound, optimal or not, since D(pi) bounds
    # the node at any potentials; a solve the rounding kept from its exact
    # optimum proves a weaker one.
    if (.exact_cmp(fl$bound_exact, bound_exact) > 0) {
      bound <- fl$bound
      bound_exact <- fl$bound_exact
      best_lambda <- lambda
      best_solve <- fl
    }
    certified <- TRUE
    if (!identical(fl$status, "optimal")) {
      status <- fl$status
    }
    if (is.null(best_solve)) {
      best_solve <- fl
    }

    g <- .cardinality_violations(coefs, fl$read)
    rows <- .cardinality_rows_exact(node$index, coefs, fl$read, lambda)
    if (!length(g) || all(rows$sign <= 0)) {
      # A relaxed optimum that satisfies every row is feasible for the original
      # problem, and when the multipliers price none of the satisfied rows it
      # also attains the bound, which settles the node.
      solutions[[length(solutions) + 1L]] <- fl
      if (!length(g) || rows$weighted_sign >= 0) {
        break
      }
    }

    gnorm2 <- sum(g * g)
    if (gnorm2 <= 0) {
      break
    }
    if (is.null(t0)) {
      reach <- if (is.finite(incumbent) && incumbent > fl$bound) {
        incumbent - fl$bound
      } else {
        max(abs(fl$bound) * 1e-3, 1)
      }
      t0 <- reach / gnorm2
    }
    lambda <- pmax(0, lambda + (t0 / k) * g)
  }

  list(bound = bound, bound_exact = bound_exact, lambda = best_lambda,
       relaxed = best_solve, solutions = solutions, certified = certified,
       status = status, n_solves = n_solves, warm = warm)
}

# The cardinality no feasible matched set can exceed, given a lower bound on the
# objective, held exactly. One unit of cardinality is worth P and the rest of
# the objective reaches at most P - 1, so q = ceiling((bound - (P - 1)) / P),
# which is ceiling((bound + 1) / P) - 1, and the ceiling is decided exactly.
.cardinality_best_possible <- function(index, bound) {
  budget <- index$total_budget
  penalty <- index$tiers$penalty
  if (length(bound) == 1L && is.infinite(bound)) {
    return(if (bound > 0) 0L else as.integer(budget))
  }
  q <- max(0, lap_exact_ceil_quotient(bound, 1, penalty) - 1)
  as.integer(max(0, budget - q))
}

# A matched set every moment row accepts, cut out of one that violates some of
# them. Removing a pair moves row r by exactly that pair's coefficient a^r_ij,
# so which pair to drop is arithmetic on the pairs rather than another solve:
# each step drops the one whose contribution to the violated rows, weighted by
# how far each is violated, is largest. Summed over the pairs still held those
# scores come to sum_r g_r^2 over the violated rows, which is positive, so a
# pair that reduces them exists at every step and the loop ends within one pass
# over the pairs.
#
# A pair is a candidate for removal only when its two units sit in the same cell
# of the deepest exactly balanced level. The levels are nested, so that keeps
# the matched counts equal at every level balance is enforced exactly on, and
# what the loop leaves behind is a matched set the network can carry. With no
# exact level every pair is a candidate.
#
# The loop scores in doubles and stops on the rows' exact signs, so a row the
# doubles put at zero while it is positive by less than their rounding is still
# driven down, and what is returned satisfies every row exactly. Such a row is
# weighted 1 rather than by its rounded value.
#
# Returns NULL when the rows still ask for a removal no candidate supplies, and
# when nothing is left to keep.
.cardinality_feasible_subset <- function(index, read, coefs) {
  coefs <- .as_moment_coefficient_list(coefs)
  left <- read$left
  right <- read$right
  if (!length(coefs) || !length(left)) {
    return(NULL)
  }

  hier <- index$hier
  candidate <- if (hier$exact >= 1L) {
    .balance_codes_at(hier, index$code_left, hier$exact)[left] ==
      .balance_codes_at(hier, index$code_right, hier$exact)[right]
  } else {
    rep(TRUE, length(left))
  }

  contrib <- do.call(cbind, lapply(coefs, function(cf) {
    cf$u[left] - cf$w[right] - cf$b
  }))
  value <- colSums(contrib)
  held <- rep(TRUE, length(left))

  repeat {
    hot <- value > 0
    if (!any(hot)) {
      exact <- .cardinality_rows_exact(index, coefs,
                                       list(left = left[held],
                                            right = right[held]))
      hot <- exact$sign > 0
      if (!any(hot)) {
        break
      }
    }
    weight <- ifelse(value[hot] > 0, value[hot], 1)
    score <- as.numeric(contrib[, hot, drop = FALSE] %*% weight)
    score[!(held & candidate)] <- -Inf
    k <- which.max(score)
    if (!length(k) || !is.finite(score[[k]]) || score[[k]] <= 0) {
      return(NULL)
    }
    held[[k]] <- FALSE
    value <- value - contrib[k, ]
  }

  if (!any(held)) {
    return(NULL)
  }
  list(left = left[held], right = right[held])
}

# A matched set as the arc bounds that hold it: every pair arc it uses forced to
# one and every other barred. A solve under those bounds reads that set back and
# no other, so its objective, its certificate and its audit come off the same
# path every incumbent in the search comes off.
.cardinality_pin_pairs <- function(index, left, right) {
  pair <- index$ranges$pair
  used <- match(left + (right - 1L) * index$n_left, index$pair_key)
  bound <- numeric(length(pair))
  bound[used] <- 1
  edits <- .cardinality_add_edit(.cardinality_no_edits(), pair, bound, bound)
  # Every pair the set does not use is barred, the ones never generated
  # included, so no pair is priced into this solve.
  edits$closed <- TRUE
  edits
}

# The left unit, or the pair, whose contribution to a relaxed row is largest. A
# row counts as relaxed when the current matched set violates it or when the
# multipliers price it, which are the two ways a row keeps the node's bound
# below its optimum.
#
# The decision has to exist whatever the relaxed optimum looks like, or a node
# would be dropped with its subtree unexamined, so the rule falls back to any
# undecided left unit and then to any undecided pair. Every branch decides an
# arc that was free, and a node with every pair arc decided has its matched set
# determined, which is where the recursion ends.
.cardinality_branch_pick <- function(index, read, coefs, g, lambda,
                                     fixed_units, fixed_pairs, branch) {
  left <- read$left
  right <- read$right
  relaxed <- which(g > 0 | lambda > 0)
  score <- rep(0, length(left))
  if (length(relaxed) && length(left)) {
    score <- rep(-Inf, length(left))
    for (r in relaxed) {
      cf <- coefs[[r]]
      score <- pmax(score, abs(cf$u[left] - cf$w[right] - cf$b))
    }
  }

  if (identical(branch, "unit")) {
    free <- which(!(left %in% fixed_units))
    if (length(free)) {
      k <- free[[which.max(score[free])]]
      return(list(kind = "unit", unit = left[[k]],
                  arc = index$ranges$unit_left[[left[[k]]]]))
    }
    rest <- setdiff(seq_len(index$n_left), fixed_units)
    if (length(rest)) {
      return(list(kind = "unit", unit = rest[[1L]],
                  arc = index$ranges$unit_left[[rest[[1L]]]]))
    }
  }

  # `pair_key` locates a pair within its own arc class, and the network's arc
  # ids for that class are what an edit has to name.
  arcs <- index$ranges$pair[match(left + (right - 1L) * index$n_left,
                                  index$pair_key)]
  free <- which(!(arcs %in% fixed_pairs))
  if (length(free)) {
    k <- free[[which.max(score[free])]]
    return(list(kind = "pair", arc = arcs[[k]]))
  }
  rest <- setdiff(index$ranges$pair, fixed_pairs)
  if (length(rest)) {
    return(list(kind = "pair", arc = rest[[1L]]))
  }
  NULL
}

# The two children a branching decision produces: the arc barred, and the arc
# forced. Their union is the parent.
.cardinality_children <- function(node, pick) {
  out <- node
  out$edits <- .cardinality_add_edit(node$edits, pick$arc, 0, 0)
  keep <- node
  keep$edits <- .cardinality_add_edit(node$edits, pick$arc, 1, 1)
  if (identical(pick$kind, "unit")) {
    out$fixed_units <- c(node$fixed_units, pick$unit)
    keep$fixed_units <- out$fixed_units
  } else {
    out$fixed_pairs <- c(node$fixed_pairs, pick$arc)
    keep$fixed_pairs <- out$fixed_pairs
  }
  out$depth <- node$depth + 1L
  keep$depth <- node$depth + 1L
  list(out, keep)
}

#' Branch and bound over the moment constraints
#'
#' Searches the tree of inclusion decisions, bounding every node by its
#' Lagrangian relaxation and keeping an incumbent that satisfies every stated
#' constraint. Returns at any interruption with that incumbent and a global
#' bound that is valid for the whole problem, never with an unproven claim of
#' optimality.
#'
#' `time_limit` reaches the solver. A solve that runs out of budget stops
#' between augmentations and comes back saying so, and the node it belonged to
#' goes back on the frontier unopened, so the bound the search reports still
#' covers the whole tree.
#'
#' @param problem The network, or the pair `.balance_flow_problem()` returns.
#' @param index The network's index, unless `problem` carries one.
#' @param coefs Moment coefficients, one per one-sided row.
#' @param dual_steps Multiplier updates per node.
#' @param branch Whether to branch on left-unit inclusion or on pairs.
#' @param node_limit,time_limit Search budget, in nodes and in seconds.
#' @param should_stop Optional predicate of the search state; `TRUE` stops the
#'   search the way an interrupt would.
#' @param cost Optional distance matrix for the audit.
#'
#' @return A list of class `cardinality_run`.
#' @keywords internal
.cardinality_branch_bound <- function(problem, index = NULL, coefs = NULL,
                                      dual_steps = 20L,
                                      branch = c("unit", "pair"),
                                      node_limit = 500L, time_limit = Inf,
                                      should_stop = NULL, cost = NULL) {
  node0 <- .cardinality_as_node(problem, index)
  index <- node0$index
  coefs <- .as_moment_coefficient_list(coefs)
  branch <- match.arg(branch)
  node_limit <- .flow_count(node_limit, "node_limit", allow_inf = TRUE)
  if (!is.numeric(time_limit) || length(time_limit) != 1L ||
      is.na(time_limit) || time_limit < 0) {
    stop("`time_limit` must be a single non-negative number of seconds, or ",
         "Inf.", call. = FALSE)
  }
  if (!is.null(should_stop) && !is.function(should_stop)) {
    stop("`should_stop` must be a function of the search state.", call. = FALSE)
  }

  # The empty matched set satisfies every moment row, since each row's value is
  # a sum over pairs, so an incumbent that is feasible for every stated
  # constraint exists before the first node is opened. It is taken from the node
  # of the tree that excludes every left unit, so it arrives certified and
  # audited like any other, rather than as a hand-built flow.
  bare <- .cardinality_no_edits()
  for (a in index$ranges$unit_left) {
    bare <- .cardinality_add_edit(bare, a, 0, 0)
  }
  # The one solve the budget does not bound. Every stopping path returns a
  # feasible incumbent, and this is the solve that makes one exist, so a budget
  # that cut it short would buy responsiveness with the guarantee.
  empty <- .cardinality_flow(node0$problem, index, edits = bare, cost = cost)

  started <- proc.time()[["elapsed"]]
  deadline <- if (is.finite(time_limit)) started + time_limit else Inf

  # A bound prunes nothing until something feasible is known, and the empty set
  # leaves every bound below it, so the root looks for a matched set worth
  # pruning against before the first node is opened. The relaxed optimum at zero
  # multipliers is the largest matched set there is; pairs come off it until
  # every moment row holds, and what is left enters the incumbent once the rows
  # are recomputed on those pairs and the solve behind them is audited. When the
  # rows admit nothing there, the empty set stands.
  incumbent <- empty
  root_solves <- 0L
  root_warm <- NULL
  if (length(coefs)) {
    free <- .cardinality_flow(node0$problem, index, cost = cost,
                              time_limit = .cardinality_remaining(deadline))
    root_solves <- root_solves + 1L
    if (!identical(free$status, "interrupted") &&
        !.cardinality_no_flow(free$status)) {
      root_warm <- list(flow = free$flow, potential = free$potential)
      subset <- .cardinality_feasible_subset(.cardinality_current_index(index),
                                             free$read, coefs)
      if (!is.null(subset)) {
        seed <- .cardinality_flow(node0$problem, index, cost = cost,
                                  edits = .cardinality_pin_pairs(
                                    .cardinality_current_index(index),
                                    subset$left, subset$right),
                                  warm = root_warm,
                                  time_limit = .cardinality_remaining(deadline))
        root_solves <- root_solves + 1L
        # The status is asked first, since an interrupted solve has no matched
        # set to read the rows off.
        if (identical(seed$status, "optimal") && seed$audit_ok &&
            seed$integral &&
            all(.cardinality_rows_exact(seed$index, coefs, seed$read)$sign <= 0) &&
            .exact_cmp(seed$objective_exact, incumbent$objective_exact) < 0) {
          incumbent <- seed
        }
      }
    }
  }

  env <- new.env(parent = emptyenv())
  env$incumbent <- incumbent
  env$best <- incumbent$objective
  env$best_exact <- incumbent$objective_exact
  env$n_nodes <- 0L
  env$n_solves <- root_solves
  env$bound_certified <- TRUE
  env$stopped_on <- "optimality"
  env$active <- NULL
  env$frontier <- list(list(id = 0L, edits = .cardinality_no_edits(),
                            lambda = numeric(length(coefs)),
                            bound = -Inf, bound_value = -Inf, depth = 0L,
                            fixed_units = integer(0),
                            fixed_pairs = integer(0)))
  env$root_bound <- -Inf
  env$next_id <- 1L

  # One warm start is kept, and only the two children of the node that produced
  # it may use it. A flow is one number per arc, so keeping one per open node
  # would grow with the frontier on the largest networks the search runs on,
  # which is where a warm start is worth having in the first place. Children
  # inherit their parent's bound and the deeper of two equal bounds is taken
  # first, so the search descends and the node opened next is usually one of the
  # two holding it.
  env$warm <- root_warm
  env$warm_ids <- 0L

  # Bounds and the incumbent's objective are held exactly, so the smallest open
  # bound, the test that closes the tree and the test that prunes a node are
  # decided with no tolerance, and a node pruned on its bound holds nothing
  # below the incumbent at all.
  frontier_bound <- function() {
    open <- lapply(env$frontier, function(nd) nd$bound)
    if (!is.null(env$active)) {
      open <- c(open, list(env$active$bound))
    }
    .exact_min(open)
  }
  global_bound <- function() {
    .exact_min(list(env$best_exact, frontier_bound()))
  }

  tryCatch({
    repeat {
      if (!length(env$frontier)) {
        env$stopped_on <- "optimality"
        break
      }
      if (env$n_nodes >= node_limit) {
        env$stopped_on <- "node_limit"
        break
      }
      if (proc.time()[["elapsed"]] - started > time_limit) {
        env$stopped_on <- "time_limit"
        break
      }
      if (!is.null(should_stop) &&
          isTRUE(should_stop(list(n_nodes = env$n_nodes,
                                  elapsed = proc.time()[["elapsed"]] - started,
                                  incumbent = env$best,
                                  bound = .exact_value(global_bound()))))) {
        env$stopped_on <- "interrupt"
        break
      }
      # The global form of the pruning test: when no open node can reach below
      # the incumbent, the incumbent is optimal and the nodes still listed hold
      # nothing worth opening.
      if (.exact_cmp(frontier_bound(), env$best_exact) >= 0) {
        env$stopped_on <- "bound"
        break
      }

      # Best-first on the bound, since that is what closes a tree, and deepest
      # first among the nodes that share one. Children inherit their parent's
      # bound, so ties are the common case, and taking the deeper of two equal
      # nodes walks down to a matched set instead of across a level. The order
      # is read off the rounded bounds; it decides which node opens next and
      # nothing the certificate rests on.
      open_bound <- vapply(env$frontier, function(nd) nd$bound_value, numeric(1))
      open_depth <- vapply(env$frontier, function(nd) nd$depth, numeric(1))
      pos <- order(open_bound, -open_depth)[[1L]]
      env$active <- env$frontier[[pos]]
      env$frontier <- env$frontier[-pos]
      env$n_nodes <- env$n_nodes + 1L

      warm <- if (env$active$id %in% env$warm_ids) env$warm else NULL
      dual <- .cardinality_lagrangian(node0$problem, coefs = coefs,
                                      lambda = env$active$lambda,
                                      steps = dual_steps, index = index,
                                      edits = env$active$edits,
                                      incumbent = env$best,
                                      cost = cost, warm = warm,
                                      deadline = deadline)
      env$n_solves <- env$n_solves + dual$n_solves
      if (!dual$certified) {
        env$bound_certified <- FALSE
      }

      if (identical(dual$status, "interrupted")) {
        # The budget ran out inside a solve. The node keeps whatever bound the
        # ascent certified before that and goes back on the frontier unopened,
        # which is what keeps the global bound a bound: a node dropped without
        # being either pruned or branched would leave part of the tree
        # unaccounted for and the number reported would cover less than it
        # claims.
        if (dual$certified) {
          env$active$bound <- .exact_max(env$active$bound, dual$bound_exact)
          env$active$bound_value <- .exact_value(env$active$bound)
        }
        if (env$n_nodes == 1L) {
          env$root_bound <- env$active$bound_value
        }
        env$frontier <- c(env$frontier, list(env$active))
        env$active <- NULL
        env$bound_certified <- FALSE
        env$stopped_on <- "time_limit"
        break
      }

      for (sol in dual$solutions) {
        if (!sol$audit_ok) next
        order <- .exact_cmp(sol$objective_exact, env$best_exact)
        ties <- order == 0L && isTRUE(sol$certified) &&
          !isTRUE(env$incumbent$certified)
        if (order < 0L || ties) {
          env$best <- sol$objective
          env$best_exact <- sol$objective_exact
          env$incumbent <- sol
        }
      }

      # A node whose ascent proved no bound keeps the one it inherited.
      node_bound <- if (identical(dual$status, "infeasible")) {
        Inf
      } else if (dual$certified) {
        .exact_max(env$active$bound, dual$bound_exact)
      } else {
        env$active$bound
      }
      node_bound_value <- .exact_value(node_bound)
      if (env$n_nodes == 1L) {
        env$root_bound <- node_bound_value
      }

      if (.exact_cmp(node_bound, env$best_exact) >= 0) {
        env$active <- NULL
        next
      }

      # A moment-feasible relaxed optimum that attains the node's bound has
      # already entered the incumbent, so the prune above is what closes a node
      # that is solved. Anything reaching here has a bound below the incumbent
      # and a subtree still worth opening.
      relaxed <- dual$relaxed
      g <- .cardinality_violations(coefs, relaxed$read)
      pick <- .cardinality_branch_pick(.cardinality_current_index(index),
                                       relaxed$read, coefs, g,
                                       dual$lambda, env$active$fixed_units,
                                       env$active$fixed_pairs, branch)
      if (is.null(pick)) {
        # Every pair arc is decided, so the node's matched set is determined and
        # is either the incumbent already or infeasible for a moment row. A
        # generating search has decided only the pairs it generated, and the
        # subtree using the others is left unexamined, so its bound no longer
        # covers the whole tree.
        if (!is.null(index$gen)) {
          env$bound_certified <- FALSE
        }
        env$active <- NULL
        next
      }
      kids <- .cardinality_children(env$active, pick)
      kids <- lapply(kids, function(kid) {
        kid$bound <- node_bound
        kid$bound_value <- node_bound_value
        kid$lambda <- dual$lambda
        kid$id <- env$next_id
        env$next_id <- env$next_id + 1L
        kid
      })
      # A child differs from its parent by one arc bound, so the parent's flow
      # is a starting point for it once that one arc is clamped back inside the
      # bound the branch moved.
      env$warm <- dual$warm
      env$warm_ids <- vapply(kids, function(kid) kid$id, integer(1))
      env$frontier <- c(env$frontier, kids)
      env$active <- NULL
    }
  }, interrupt = function(e) {
    env$stopped_on <- "interrupt"
  })

  bound_exact <- global_bound()
  bound <- .exact_value(bound_exact)
  best_possible <- .cardinality_best_possible(index, bound_exact)
  n_pairs <- env$incumbent$read$n_pairs
  settled <- env$stopped_on %in% c("optimality", "bound")
  certified <- settled && env$bound_certified &&
    env$incumbent$certified && env$incumbent$audit_ok &&
    identical(env$incumbent$status, "optimal") &&
    best_possible <= n_pairs

  structure(list(index = .cardinality_current_index(index),
                 solution = env$incumbent,
                 objective = env$best,
                 bound = bound,
                 root_bound = env$root_bound,
                 best_possible = max(best_possible, n_pairs),
                 bound_certified = env$bound_certified,
                 certified = certified,
                 stopped_on = .validate_cardinality_stop(env$stopped_on),
                 status = if (certified) "optimal" else "iteration_limit",
                 n_nodes = env$n_nodes,
                 n_solves = env$n_solves,
                 engine = if (length(coefs)) "branch_bound" else "flow",
                 coefs = coefs),
            class = "cardinality_run")
}

# The achieved value of one moment row on a matched set, and the room left under
# its bound. The statistic is the one the row was stated in: a standardized
# difference carries the pooled spread the row fixed once, a mean difference
# carries none. `row_sign` is the exact sign of the row's value on the set,
# which is what the search held it to.
.cardinality_constraint_row <- function(spec, coefs, read, row_sign) {
  k <- length(read$left)
  scale <- if (identical(spec$stat, "std_diff")) spec$denominator else 1
  signed <- if (k) {
    (sum(coefs$u[read$left]) - sum(coefs$w[read$right])) / k
  } else {
    NA_real_
  }
  stated <- k > 0L && is.finite(scale) && scale != 0 && !isTRUE(spec$trivial)
  achieved <- if (stated) signed / (scale * spec$direction) else NA_real_
  slack <- if (is.na(achieved)) NA_real_ else (spec$bound - signed) / scale
  list(kind = spec$stat,
       target = .moment_var_label(spec$var, spec$transform),
       bound = spec$limit,
       achieved = achieved,
       slack = slack,
       satisfied = row_sign <= 0L)
}

# Matched counts per category at one level of the hierarchy.
.cardinality_level_balance <- function(index, read, h) {
  hier <- index$hier
  n_h <- hier$n_cats[[h + 1L]]
  a <- tabulate(.balance_codes_at(hier, index$code_left, h)[read$left],
                nbins = n_h)
  b <- tabulate(.balance_codes_at(hier, index$code_right, h)[read$right],
                nbins = n_h)
  list(a = a, b = b, labels = .balance_label(hier$labels[[h]]))
}

.cardinality_level_label <- function(hier, h) {
  cols <- hier$columns[[h]]
  if (!length(cols)) "(all units)" else paste(cols, collapse = " : ")
}

#' Report of a cardinality match
#'
#' Turns a completed search into the object callers read: the matched sample's
#' size, the largest size the bound admits, the gap between them, and the state
#' of every constraint the match was asked to meet.
#'
#' @param run A `cardinality_run` from [.cardinality_branch_bound()].
#' @param specs The moment rows the run was given, from `.moment_specs()`.
#'
#' @return An object of class `cardinality_report`, a list with elements:
#' \itemize{
#'   \item `n_matched`, `n_left_matched` - matched pairs, and the left units
#'         they use.
#'   \item `best_possible` - the largest matched sample the bound admits.
#'   \item `gap`, `gap_fraction` - `best_possible - n_matched`, in matched
#'         units and as a share of `best_possible`.
#'   \item `certified` - `TRUE` only when the search settled, every solve its
#'         bound rests on was certified, the incumbent's own solve was
#'         certified and audited, and `gap` is zero.
#'   \item `objective`, `bound` - the incumbent's value of the network
#'         objective and the global lower bound on it. `best_possible` is read
#'         from `bound`.
#'   \item `stopped_on`, `n_nodes`, `engine`, `status`.
#'   \item `constraints` - one row per stated constraint, with what it asked
#'         for and what the matched sample achieved.
#'   \item `balance` - matched counts and imbalance per category at every
#'         level.
#'   \item `tiers`, `precision_headroom`, `shift` - the weights that order the
#'         objective, how many times its range fits inside the range a double
#'         orders exactly, and the constant taken off every distance.
#'   \item `total_distance`, `pairs` - the matched set itself.
#'   \item `potentials` - the pair potentials of the solve the matched set
#'         came from, as `.cardinality_pair_duals()` reads them, or `NULL`
#'         when that solve was not certified.
#' }
#' @keywords internal
.cardinality_report <- function(run, specs = NULL) {
  if (!inherits(run, "cardinality_run")) {
    stop("`run` must come from `.cardinality_branch_bound()`.", call. = FALSE)
  }
  index <- run$index
  hier <- index$hier
  read <- run$solution$read
  specs <- if (is.null(specs)) list() else specs
  coefs <- run$coefs

  n_pairs <- read$n_pairs
  best_possible <- max(run$best_possible, n_pairs)
  gap <- best_possible - n_pairs
  gap_fraction <- if (best_possible > 0) gap / best_possible else 0

  row_sign <- .cardinality_rows_exact(index, coefs, read)$sign
  rows <- lapply(seq_along(specs), function(r) {
    .cardinality_constraint_row(specs[[r]], coefs[[r]], read, row_sign[[r]])
  })
  constraints <- tibble::tibble(
    kind = vapply(rows, `[[`, character(1), "kind"),
    target = vapply(rows, `[[`, character(1), "target"),
    bound = vapply(rows, function(x) as.numeric(x$bound), numeric(1)),
    achieved = vapply(rows, function(x) as.numeric(x$achieved), numeric(1)),
    slack = vapply(rows, function(x) as.numeric(x$slack), numeric(1)),
    satisfied = vapply(rows, `[[`, logical(1), "satisfied"))

  exact_levels <- seq_len(hier$exact)
  if (length(exact_levels)) {
    imb <- vapply(exact_levels, function(h) {
      lv <- .cardinality_level_balance(index, read, h)
      sum(pmax(lv$a - lv$b, 0))
    }, numeric(1))
    constraints <- rbind(constraints, tibble::tibble(
      kind = rep("exact_balance", length(exact_levels)),
      target = vapply(exact_levels, function(h)
        .cardinality_level_label(hier, h), character(1)),
      bound = rep(0, length(exact_levels)),
      achieved = imb,
      slack = -imb,
      satisfied = imb == 0))
  }

  balance <- do.call(rbind, lapply(seq_len(hier$n_levels), function(h) {
    lv <- .cardinality_level_balance(index, read, h)
    tibble::tibble(level = rep(h, length(lv$a)),
                   category = lv$labels,
                   n_left = as.integer(lv$a),
                   n_right = as.integer(lv$b),
                   imbalance = as.integer(lv$a - lv$b))
  }))
  if (is.null(balance)) {
    balance <- tibble::tibble(level = integer(0), category = character(0),
                              n_left = integer(0), n_right = integer(0),
                              imbalance = integer(0))
  }

  tiers <- index$tiers
  reach <- tiers$base_magnitude + sum(tiers$counts * tiers$weights)
  headroom <- if (reach > 0) PAD_PRECISION_LIMIT / reach else Inf

  pairs <- tibble::tibble(
    left = as.integer(read$left),
    right = as.integer(read$right),
    distance = index$pair_cost[match(read$left + (read$right - 1L) *
                                       index$n_left, index$pair_key)] +
      index$cost_shift)

  structure(list(n_matched = as.integer(n_pairs),
                 n_left_matched = length(unique(read$left)),
                 best_possible = as.integer(best_possible),
                 gap = as.integer(gap),
                 gap_fraction = gap_fraction,
                 certified = isTRUE(run$certified),
                 objective = run$objective,
                 bound = run$bound,
                 stopped_on = run$stopped_on,
                 n_nodes = as.integer(run$n_nodes),
                 engine = run$engine,
                 constraints = constraints,
                 balance = balance,
                 tiers = tiers,
                 precision_headroom = headroom,
                 shift = index$cost_shift,
                 status = run$status,
                 total_distance = read$total_distance + n_pairs *
                   index$cost_shift,
                 pairs = pairs,
                 potentials = if (isTRUE(run$solution$certified))
                   run$solution$pair_duals else NULL),
            class = "cardinality_report")
}

#' @param x A `cardinality_report`.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
#' @method print cardinality_report
#' @rdname dot-cardinality_report
print.cardinality_report <- function(x, ...) {
  line <- function(label, value) cat(sprintf("%-21s%s\n", label, value))
  line("Matched units:", format(x$n_matched))
  if (isTRUE(x$certified)) {
    line("Best possible:", format(x$best_possible))
    line("Optimality gap:", format(x$gap))
    cat("Certified optimal\n")
  } else {
    line("Global upper bound:", format(x$best_possible))
    line("Cardinality gap:",
         sprintf("%s %s (%.3f%%)", format(x$gap),
                 if (isTRUE(x$gap == 1)) "unit" else "units",
                 100 * x$gap_fraction))
  }
  invisible(x)
}

#' Balance-constrained cardinality match
#'
#' Builds the balance network for a distance matrix and a nested partition,
#' searches it under whatever moment constraints are stated, and reports the
#' matched sample with the bound that goes with it.
#'
#' @param left,right Data frames of units, one row each.
#' @param cost Numeric distance matrix, `nrow(left)` by `nrow(right)`. A cell
#'   that is `Inf` or `NA` is a pair the match may not use.
#' @param refined The nested groupings balance is required on, coarsest first.
#' @param exact How many of the coarsest levels are enforced exactly.
#' @param moments,max_std_diff,vars Moment constraints, as `.moment_specs()`
#'   takes them.
#' @param dual_steps Multiplier updates per node.
#' @param branch Whether to branch on left-unit inclusion or on pairs.
#' @param node_limit,time_limit Search budget.
#' @param should_stop Optional predicate of the search state; `TRUE` stops the
#'   search the way an interrupt would.
#'
#' @return A `cardinality_report`.
#' @keywords internal
.cardinality_solve <- function(left, right, cost, refined = NULL, exact = 1L,
                               moments = NULL, max_std_diff = NULL, vars = NULL,
                               dual_steps = 20L, branch = c("unit", "pair"),
                               node_limit = 500L, time_limit = Inf,
                               should_stop = NULL) {
  hier <- .refined_hierarchy(left, right, refined, exact = exact)
  gen <- NULL
  if (is_lazy_cost_spec(cost)) {
    gen <- .cardinality_generator(cost)
    built <- .balance_flow_problem(gen$pool, hier)
    built$index$gen <- gen
    gen$built <- built
    cost <- NULL
  } else {
    built <- .balance_flow_problem(cost, hier)
  }
  specs <- .moment_specs(moments = moments, max_std_diff = max_std_diff,
                         vars = vars, left = left, right = right)
  coefs <- lapply(specs, .moment_coefficients, left = left, right = right)

  run <- .cardinality_branch_bound(built$problem, built$index, coefs = coefs,
                                   dual_steps = dual_steps,
                                   branch = branch, node_limit = node_limit,
                                   time_limit = time_limit,
                                   should_stop = should_stop, cost = cost)
  report <- .cardinality_report(run, specs = specs)
  if (!is.null(gen) && is.function(gen$spec$distance)) {
    lazy_pair_distances(gen$spec, report$pairs$left, report$pairs$right)
  }
  if (!is.null(gen)) {
    report$search <- list(
      seed_width      = as.integer(gen$seed_width),
      candidate_edges = as.numeric(length(gen$built$index$pair_left)),
      possible_edges  = as.numeric(gen$pool$n_left) * gen$pool$n_right,
      edges_evaluated = as.numeric(lap_pricing_evaluated(gen$session))
    )
  }
  report
}

# The pricing session a search over a specification generates pairs from, and
# the pool its network starts with: each left unit's cheapest admissible right
# units, at the seed width the implicit loop uses, together with the smallest
# and largest admissible distance over every pair, read in one pass, which the
# tier weights have to be built against.
.cardinality_generator <- function(spec) {
  session <- lap_pricing_session(spec$left_mat, spec$right_mat, spec$distance,
                                 lazy_cost_spec_inv_cov(spec), spec$max_distance,
                                 lazy_cost_spec_calipers(spec))
  range <- lap_pricing_range(session)
  seed_width <- lap_implicit_seed_width(spec$n_right)
  seed <- lap_pricing_seed(session, seed_width)
  distance <- lap_pricing_cost(session, seed$i, seed$j)
  some <- range$n_admissible > 0
  gen <- new.env(parent = emptyenv())
  gen$spec <- spec
  gen$session <- session
  gen$seed_width <- seed_width
  gen$keep_per_row <- .implicit_defaults()$keep_per_row
  gen$pool <- structure(list(n_left = spec$n_left, n_right = spec$n_right,
                             left = as.integer(seed$i), right = as.integer(seed$j),
                             distance = as.numeric(distance),
                             min = if (some) range$min else NA_real_,
                             max = if (some) range$max else NA_real_),
                        class = "balance_pair_pool")
  gen
}
