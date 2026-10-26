# The R side of the flow model: the door a problem comes through, the solve, and
# the certificate.
#
# The C++ tests in cpp_tests/ own the solver and the certifier. What is checked
# here is what only exists in R: that a malformed problem is named in R terms
# before it reaches a solver that can only speak in node indices, that the three
# steps stay separable, and that verify_flow() is a check rather than a report.
# A certificate that passed on potentials it never looked at would prove nothing,
# so every failing condition below is asserted to fail.

# Two supply nodes shipping to two demand nodes. Node 1 has two units and node 2
# has one; node 3 wants two and node 4 wants one. Its optimum is unique: both of
# node 1's units go to node 3 and node 2's unit goes to node 4, for a cost of 3.
flow_fixture <- function() {
  list(
    n_nodes = 4,
    supply  = c(2, 1, -2, -1),
    arcs = data.frame(
      tail  = c(1, 1, 2, 2),
      head  = c(3, 4, 3, 4),
      lower = c(0, 0, 0, 0),
      upper = c(2, 2, 2, 2),
      cost  = c(1, 3, 2, 1)
    )
  )
}

FLOW_OPTIMUM <- c(2, 0, 0, 1)

test_that("a problem is checked at the door, in the caller's terms", {
  ok <- flow_fixture()

  expect_error(couplr:::.flow_problem(0, numeric(0), ok$arcs), "at least one node")
  expect_error(couplr:::.flow_problem(2.5, c(1, -1), ok$arcs), "whole number")
  expect_error(couplr:::.flow_problem(4, c(1, -1), ok$arcs), "but the problem has 4 nodes")
  expect_error(couplr:::.flow_problem(4, c(2, 1, -2, NA), ok$arcs), "finite whole numbers")
  expect_error(couplr:::.flow_problem(4, c(2, 1, -2, 0), ok$arcs), "sum to zero")
})

test_that("an arc list is checked column by column", {
  ok <- flow_fixture()
  with_arcs <- function(f) {
    arcs <- ok$arcs
    couplr:::.flow_problem(ok$n_nodes, ok$supply, f(arcs))
  }

  expect_error(couplr:::.flow_problem(4, ok$supply, "not a frame"), "must be a data frame")
  expect_error(with_arcs(function(a) a[, c("tail", "head")]), "missing the column")
  expect_error(with_arcs(function(a) { a$tail[1] <- NA; a }), "carry NA")
  expect_error(with_arcs(function(a) { a$head[1] <- 9L; a }), "node ids in 1:4")
  expect_error(with_arcs(function(a) { a$lower[1] <- -1; a }), "none negative")
  expect_error(with_arcs(function(a) { a$lower[1] <- 0.5; a }), "finite whole numbers")
  expect_error(with_arcs(function(a) { a$upper[1] <- 1.5; a }), "whole numbers or Inf")
  expect_error(with_arcs(function(a) { a$lower[1] <- 2; a$upper[1] <- 1; a }),
               "none below")
  # Forbidden is expressed by leaving the arc out. An Inf cost that reaches the
  # residual search poisons every potential derived from it.
  expect_error(with_arcs(function(a) { a$cost[1] <- Inf; a }), "omit an arc rather")
  expect_error(with_arcs(function(a) { a$cost[1] <- NA_real_; a }), "must be finite")
})

test_that("the arcs arrive typed, and Inf is the only unbounded capacity", {
  ok <- flow_fixture()
  ok$arcs$upper[1] <- Inf
  prob <- couplr:::.flow_problem(ok$n_nodes, ok$supply, ok$arcs)

  expect_s3_class(prob, "couplr_flow_problem")
  expect_identical(prob$n_nodes, 4L)
  expect_type(prob$arcs$tail, "integer")
  expect_type(prob$arcs$head, "integer")
  expect_type(prob$arcs$upper, "double")
  expect_identical(names(prob$arcs),
                   c("tail", "head", "lower", "upper", "cost"))
  expect_true(is.infinite(prob$arcs$upper[1]))
})

test_that("a problem can be given as a built object or as its three fields", {
  ok <- flow_fixture()
  built <- couplr:::.flow_problem(ok$n_nodes, ok$supply, ok$arcs)

  expect_identical(couplr:::.as_flow_problem(built), built)
  expect_equal(couplr:::.as_flow_problem(ok), built)
  expect_error(couplr:::.as_flow_problem(list(n_nodes = 4)), "must be a flow problem")
})

test_that("the print method reports the shape of the network", {
  prob <- couplr:::.as_flow_problem(flow_fixture())

  expect_output(print(prob), "Flow problem")
  expect_output(print(prob), "Nodes: 4")
  expect_output(print(prob), "Arcs:  4")
  expect_output(print(prob), "Flow to place: 3 units")
  expect_output(expect_invisible(print(prob)), "Flow problem")
})

test_that("solving returns the flow, the potentials and a status from the vocabulary", {
  solved <- couplr:::.flow_solve(flow_fixture())

  expect_s3_class(solved, "flow_solve_result")
  expect_identical(solved$status, "optimal")
  expect_true(solved$status %in% solver_status_values())
  expect_equal(solved$flow, FLOW_OPTIMUM)
  expect_equal(solved$total_cost, 3)
  expect_equal(solved$flow_sent, 3)
  expect_equal(solved$flow_required, 3)

  # Potentials are defined up to a constant per component, and the lowering
  # needs one particular representative: the first node is the origin.
  expect_length(solved$potential, 4)
  expect_equal(solved$potential[1], 0)

  # The problem travels with the answer, which is what lets the flow, the
  # potentials and the network they belong to reach the certificate together.
  expect_s3_class(solved$problem, "couplr_flow_problem")
})

test_that("potentials are omitted when they are not asked for", {
  solved <- couplr:::.flow_solve(flow_fixture(), return_potentials = FALSE)

  expect_length(solved$potential, 0)
  expect_equal(solved$flow, FLOW_OPTIMUM)
})

test_that("a solve that cannot place every unit says so rather than reporting optimal", {
  # Node 4 wants a unit and nothing reaches it.
  stranded <- list(
    n_nodes = 4,
    supply  = c(2, 1, -2, -1),
    arcs = data.frame(tail = c(1, 2), head = c(3, 3),
                      lower = c(0, 0), upper = c(2, 2), cost = c(1, 1))
  )
  partial <- couplr:::.flow_solve(stranded)
  expect_identical(partial$status, "partial")
  expect_equal(partial$flow_sent, 2)
  expect_equal(partial$flow_required, 3)

  # An arc that carries nothing is not a route.
  blocked <- list(
    n_nodes = 2,
    supply  = c(1, -1),
    arcs = data.frame(tail = 1, head = 2, lower = 0, upper = 0, cost = 1)
  )
  expect_identical(couplr:::.flow_solve(blocked)$status, "infeasible")
})

test_that("a solve that falls short places the cheapest flow of its value", {
  # Two nodes with a unit each and one deficit node reachable from both, over
  # arcs costing 10 and 1. Only one unit can be placed, and it is the one over
  # the arc costing 1 whichever order the arcs and nodes are given in.
  for (dear_first in c(TRUE, FALSE)) {
    arcs <- data.frame(tail = c(1, 2), head = c(3, 3), lower = 0, upper = 1,
                       cost = c(10, 1))
    if (!dear_first) arcs <- arcs[2:1, ]
    short <- couplr:::.flow_solve(list(n_nodes = 4, supply = c(1, 1, -1, -1),
                                       arcs = arcs))
    expect_identical(short$status, "partial")
    expect_equal(short$flow_sent, 1)
    expect_equal(short$total_cost, 1)
  }
})

test_that("stopping on the augmentation cap is not optimality", {
  capped <- couplr:::.flow_solve(flow_fixture(), max_augmentations = 1)

  expect_identical(capped$status, "iteration_limit")
  expect_lt(capped$flow_sent, capped$flow_required)
  expect_error(couplr:::.flow_solve(flow_fixture(), max_augmentations = -1),
               "must not be negative")
})

test_that("a lower bound is met by the solver, not by the caller", {
  # The cheap arc is 1 -> 3, but 1 -> 4 must carry a unit.
  forced <- flow_fixture()
  forced$arcs$lower[2] <- 1
  solved <- couplr:::.flow_solve(forced)

  expect_identical(solved$status, "optimal")
  expect_equal(solved$flow[2], 1)
  expect_true(verify_flow(solved)$certified_optimal)
  # A unit is pushed off the cheapest route and node 3's second unit comes from
  # node 2 instead: 3 on the forced arc, 1 from node 1 and 2 from node 2.
  expect_equal(solved$total_cost, 6)
})

test_that("verify_flow certifies a solve result end to end", {
  solved <- couplr:::.flow_solve(flow_fixture())
  cert <- verify_flow(solved)

  expect_s3_class(cert, "flow_certificate")
  expect_true(cert$certified_optimal)
  expect_true(cert$primal_feasible)
  expect_true(cert$dual_feasible)
  expect_true(cert$complementary_slackness)
  expect_equal(cert$n_capacity_violations, 0)
  expect_equal(cert$n_conservation_violations, 0)
  expect_equal(cert$n_cs_violations, 0)
  expect_equal(cert$primal_objective, 3)
  expect_equal(cert$dual_objective, 3)
  expect_equal(cert$duality_gap, 0)
  expect_equal(cert$tolerance, 1e-9)
  # worst_arc names the arc attaining the smallest reduced cost over the
  # residual graph, which exists whenever any arc can still take flow. On an
  # optimal flow that minimum is at the tolerance rather than below it, which
  # is what dual_feasible above reads.
  expect_equal(cert$min_residual_reduced_cost, 0)
  expect_equal(cert$worst_arc, 1)
})

test_that("a flow and a problem certify without a solve result to carry them", {
  cert <- verify_flow(FLOW_OPTIMUM, flow_fixture(), potential = c(0, 0, 1, 1))

  expect_true(cert$certified_optimal)
  expect_equal(cert$primal_objective, 3)
})

test_that("potentials that certify nothing fail the check rather than passing it", {
  # The optimal flow, priced by potentials that are not optimal ones. Every arc
  # prices at its own cost, so nothing is underpriced and dual feasibility
  # holds, while the two arcs carrying flow price above zero and slackness does
  # not.
  cert <- verify_flow(FLOW_OPTIMUM, flow_fixture(), potential = c(0, 0, 0, 0))

  expect_false(cert$certified_optimal)
  expect_true(cert$primal_feasible)
  expect_true(cert$dual_feasible)
  expect_false(cert$complementary_slackness)
  expect_equal(cert$n_cs_violations, 2)
  expect_equal(cert$primal_objective, 3)
  expect_equal(cert$dual_objective, 0)
  expect_equal(cert$duality_gap, 3)
})

test_that("a dual violation names the arc that attains it, indexed from one", {
  # Both demand nodes priced far above every arc's cost, so each arc that can
  # still take flow prices below zero. Arc 4 is the worst at 1 - 10.
  cert <- verify_flow(FLOW_OPTIMUM, flow_fixture(), potential = c(0, 0, 10, 10))

  expect_false(cert$certified_optimal)
  expect_false(cert$dual_feasible)
  expect_equal(cert$min_residual_reduced_cost, -9)
  expect_equal(cert$worst_arc, 4)
})

test_that("a flow outside its bounds fails primal feasibility and reports no objective", {
  cert <- verify_flow(c(3, 0, 0, 1), flow_fixture(), potential = c(0, 0, 1, 1))

  expect_false(cert$certified_optimal)
  expect_false(cert$primal_feasible)
  expect_equal(cert$n_capacity_violations, 1)
  # A cost summed over a vector that is not a flow invites comparison with the
  # dual bound while corresponding to no feasible solution.
  expect_true(is.na(cert$primal_objective))
  expect_true(is.na(cert$duality_gap))
})

test_that("a flow that does not conserve is not a flow", {
  cert <- verify_flow(c(1, 0, 0, 1), flow_fixture(), potential = c(0, 0, 1, 1))

  expect_false(cert$primal_feasible)
  expect_equal(cert$n_capacity_violations, 0)
  # Node 1 sends one unit short and node 3 receives one short.
  expect_equal(cert$n_conservation_violations, 2)
  expect_equal(cert$max_conservation_error, 1)
})

test_that("a fractional flow is not rounded into one that certifies", {
  cert <- verify_flow(c(1.5, 0.5, 0, 1), flow_fixture(), potential = c(0, 0, 1, 1))

  expect_false(cert$certified_optimal)
  expect_false(cert$primal_feasible)
  expect_true(is.na(cert$primal_objective))
})

test_that("optimal potentials certify an optimal flow the solver did not return", {
  # Every arc costs the same, so both perfect matchings cost 2 and the solver
  # returns one of them. Optimal potentials are shared by all optimal solutions
  # of a linear program, so they certify the other one too.
  tied <- list(
    n_nodes = 4,
    supply  = c(1, 1, -1, -1),
    arcs = data.frame(tail = c(1, 1, 2, 2), head = c(3, 4, 3, 4),
                      lower = 0, upper = 1, cost = 1)
  )
  solved <- couplr:::.flow_solve(tied)
  expect_equal(solved$total_cost, 2)

  other <- 1 - solved$flow
  expect_equal(sum(other), 2)
  expect_true(verify_flow(other, tied, potential = solved$potential)$certified_optimal)
})

test_that("verify_flow solves for potentials when it is given none", {
  cert <- verify_flow(FLOW_OPTIMUM, flow_fixture())

  expect_true(cert$certified_optimal)
  expect_equal(cert$dual_objective, 3)
})

test_that("the check refuses what it cannot certify", {
  prob <- flow_fixture()

  expect_error(verify_flow(FLOW_OPTIMUM), "`problem` is required")
  expect_error(verify_flow(c(2, 0, 1), prob),
               "3 flow values but the problem has 4 arcs")
  expect_error(verify_flow(FLOW_OPTIMUM, prob, potential = c(0, 0, 1)),
               "has length 3 but the problem has 4 nodes")
  expect_error(verify_flow(FLOW_OPTIMUM, prob, tol = -1), "non-negative")
  expect_error(verify_flow(FLOW_OPTIMUM, prob, tol = c(1e-9, 1e-9)), "single non-negative")
  expect_error(verify_flow("two units", prob), "numeric vector of arc flows")
  expect_error(verify_flow(list(potential = c(0, 0, 1, 1)), prob),
               "numeric vector of arc flows")
})

test_that("the certificate prints its conditions and its conclusion", {
  cert <- verify_flow(couplr:::.flow_solve(flow_fixture()))

  expect_output(print(cert), "Flow certificate")
  expect_output(print(cert), "primal_feasible          TRUE")
  expect_output(print(cert), "certified_optimal        TRUE")
  expect_output(print(cert), "duality_gap")
  expect_output(expect_invisible(print(cert)), "Flow certificate")

  # The worst arc is reported only when there is a dual violation to attribute.
  bad <- verify_flow(FLOW_OPTIMUM, flow_fixture(), potential = c(0, 0, 10, 10))
  expect_output(print(bad), "worst reduced cost")
  expect_output(print(bad), "certified_optimal        FALSE")
})

test_that("full_match carries the certificate of the flow it actually solved", {
  set.seed(4)
  left <- data.frame(id = 1:4, x = rnorm(4))
  right <- data.frame(id = 5:14, x = rnorm(10))

  result <- full_match(left, right, vars = "x")

  expect_s3_class(result$certificate, "flow_certificate")
  expect_true(result$certificate$certified_optimal)
  expect_true(result$certificate$primal_feasible)
  expect_equal(result$certificate$duality_gap, 0, tolerance = 1e-9)
})

# --- warm starts --------------------------------------------------------------
#
# A warm start is a starting point, never an answer: what it may change is how
# many augmentations a solve takes, and what it may not change is what the solve
# returns. Every test here compares a warm solve against the cold solve of the
# same problem and asserts the two agree.

# A network wide enough that a warm start has something to save: 40 rows and 40
# columns over a random cost matrix, compiled the way the balance designs
# compile.
flow_wide_fixture <- function(seed = 7L, n = 40L) {
  set.seed(seed)
  n_nodes <- 2 * n + 2
  rows <- seq_len(n)
  cols <- seq_len(n)
  arcs <- data.frame(
    tail  = c(rep(1, n), rep(1 + rows, each = n), 1 + n + cols),
    head  = c(1 + rows, rep(1 + n + cols, times = n), rep(2 + 2 * n, n)),
    lower = 0,
    upper = 1,
    cost  = c(rep(0, n), round(stats::runif(n * n, 0, 10), 3), rep(0, n))
  )
  list(n_nodes = n_nodes,
       supply = c(n, rep(0, 2 * n), -n),
       arcs = arcs)
}

test_that("a warm start returns the cold answer", {
  prob <- flow_wide_fixture()
  cold <- couplr:::.flow_solve(prob)

  warm <- couplr:::.flow_solve(prob, warm_flow = cold$flow,
                               warm_potential = cold$potential)

  expect_identical(warm$status, "optimal")
  expect_equal(warm$total_cost, cold$total_cost)
  expect_equal(warm$flow, cold$flow)
  expect_true(verify_flow(warm)$certified_optimal)
  # Started from its own optimum, the solve has nothing left to augment.
  expect_identical(warm$n_augmentations, 0)
})

test_that("a warm start from another cost vector still lands on the optimum", {
  prob <- flow_wide_fixture(seed = 9L)
  other <- prob
  other$arcs$cost <- rev(prob$arcs$cost)
  stale <- couplr:::.flow_solve(other)

  cold <- couplr:::.flow_solve(prob)
  warm <- couplr:::.flow_solve(prob, warm_flow = stale$flow,
                               warm_potential = stale$potential)

  expect_identical(warm$status, "optimal")
  expect_equal(warm$total_cost, cold$total_cost)
  expect_true(verify_flow(warm)$certified_optimal)
})

test_that("either half of a warm start is enough on its own", {
  prob <- flow_wide_fixture(seed = 11L)
  cold <- couplr:::.flow_solve(prob)

  flow_only <- couplr:::.flow_solve(prob, warm_flow = cold$flow)
  pot_only <- couplr:::.flow_solve(prob, warm_potential = cold$potential)

  for (res in list(flow_only, pot_only)) {
    expect_identical(res$status, "optimal")
    expect_equal(res$total_cost, cold$total_cost)
    expect_true(verify_flow(res)$certified_optimal)
  }
})

test_that("a warm start of the wrong shape is named in the caller's terms", {
  prob <- flow_fixture()

  expect_error(couplr:::.flow_solve(prob, warm_flow = c(1, 2)),
               "`warm_flow` has length 2")
  expect_error(couplr:::.flow_solve(prob, warm_potential = c(0, 0)),
               "`warm_potential` has length 2")
  expect_error(couplr:::.flow_solve(prob, warm_flow = rep(NA_real_, 4)),
               "must not contain NA")
  expect_error(couplr:::.flow_solve(prob, warm_flow = c(0, 0, 0, 0.5)),
               "not a whole number")
  # A flow above an arc's capacity is a different problem's flow, not a start.
  expect_error(couplr:::.flow_solve(prob, warm_flow = c(9, 0, 0, 0)),
               "outside its bounds")
})

test_that("a warm start agrees with the cold answer across instances", {
  for (seed in 21:32) {
    prob <- flow_wide_fixture(seed = seed, n = 25L)
    cold <- couplr:::.flow_solve(prob)
    shifted <- prob
    shifted$arcs$cost <- prob$arcs$cost + round(stats::runif(nrow(prob$arcs)), 3)
    moved_cold <- couplr:::.flow_solve(shifted)
    moved_warm <- couplr:::.flow_solve(shifted, warm_flow = cold$flow,
                                       warm_potential = cold$potential)

    expect_equal(moved_warm$total_cost, moved_cold$total_cost)
    expect_true(verify_flow(moved_warm)$certified_optimal)
  }
})

# --- time budgets -------------------------------------------------------------

test_that("a spent budget stops the solve and says so", {
  prob <- flow_wide_fixture(seed = 13L)
  stopped <- couplr:::.flow_solve(prob, time_limit = 0)

  expect_identical(stopped$status, "interrupted")
  expect_true("interrupted" %in% solver_status_values())
  expect_lt(stopped$flow_sent, stopped$flow_required)
  # It is not an answer and it is not evidence that no answer exists.
  expect_false(verify_flow(stopped)$certified_optimal)
  expect_identical(couplr:::.flow_solve(prob)$status, "optimal")
})

test_that("an interrupted flow still respects every arc bound", {
  prob <- flow_wide_fixture(seed = 15L)
  stopped <- couplr:::.flow_solve(prob, time_limit = 0)

  expect_length(stopped$flow, nrow(prob$arcs))
  expect_true(all(stopped$flow >= prob$arcs$lower))
  expect_true(all(stopped$flow <= prob$arcs$upper))
  expect_length(stopped$potential, prob$n_nodes)
})

test_that("a budget the solve fits inside changes nothing", {
  prob <- flow_wide_fixture(seed = 17L)
  cold <- couplr:::.flow_solve(prob)
  budgeted <- couplr:::.flow_solve(prob, time_limit = 60)

  expect_identical(budgeted$status, "optimal")
  expect_equal(budgeted$total_cost, cold$total_cost)
})

test_that("a time limit is checked in the caller's terms", {
  expect_error(couplr:::.flow_solve(flow_fixture(), time_limit = -1),
               "non-negative")
  expect_error(couplr:::.flow_solve(flow_fixture(), time_limit = c(1, 2)),
               "single non-negative")
})

# --- the tolerance the reduced costs are read against -------------------------
#
# cbar(a) is computed as cost(a) + pi(tail(a)) - pi(head(a)), so its last bits
# are worth the largest of those three times the machine epsilon. The tolerance
# it is compared against scales with them for that reason, and the tests below
# are the two halves of that: rounding at the problem's own scale is not a
# violation, and a violation at that scale still is one.

test_that("the per-arc tolerance scales with the numbers behind the reduced cost", {
  small <- couplr:::.flow_solve(flow_fixture())
  expect_true(verify_flow(small)$certified_optimal)
  # Costs and potentials of order 1 leave the scale at its floor.
  expect_equal(verify_flow(small, tol = 1e-9)$dual_tolerance, 1e-9)

  big <- flow_fixture()
  big$arcs$cost <- big$arcs$cost * 1e7
  solved <- couplr:::.flow_solve(big)
  cert <- verify_flow(solved, tol = 1e-9)

  expect_true(cert$certified_optimal)
  expect_gt(cert$dual_tolerance, 1e-9)
  expect_equal(cert$dual_tolerance,
               1e-9 * max(1, abs(big$arcs$cost), abs(solved$potential)))
})

test_that("a violation at the problem's scale is named rather than absorbed", {
  big <- flow_fixture()
  big$arcs$cost <- big$arcs$cost * 1e7
  solved <- couplr:::.flow_solve(big)
  arc_tol <- 1e-9 * max(1, abs(big$arcs$cost), abs(solved$potential))

  # An arc that can still take flow is what dual feasibility prices. Its head's
  # potential is moved until the arc prices a thousand tolerances below zero,
  # which is still eight orders of magnitude below the costs themselves.
  a <- which(solved$flow < big$arcs$upper)[[1L]]
  head_node <- big$arcs$head[[a]]
  cbar <- big$arcs$cost[[a]] + solved$potential[[big$arcs$tail[[a]]]] -
    solved$potential[[head_node]]
  broken <- solved$potential
  broken[[head_node]] <- broken[[head_node]] + cbar + 1000 * arc_tol

  cert <- verify_flow(solved, potential = broken, tol = 1e-9)
  expect_false(cert$certified_optimal)
  expect_false(cert$dual_feasible)
  expect_lt(cert$min_residual_reduced_cost, -cert$dual_tolerance)
})

test_that("a balance design large enough to round past 1e-9 still certifies", {
  skip_on_cran()
  # The lexicographic tier weights rank cardinality above balance above
  # distance, so the potentials grow with the problem. At this size they reach
  # the millions, where one unit in the last place is around 1e-9 and the
  # reduced costs cannot be read against an absolute 1e-9.
  set.seed(41L)
  n <- 500L
  left <- data.frame(g = sample.int(4L, n, TRUE),
                     x = stats::rnorm(n, 0.6), y = stats::rnorm(n, 0.4))
  right <- data.frame(g = sample.int(4L, n, TRUE),
                      x = stats::rnorm(n, -0.6), y = stats::rnorm(n, -0.4))
  cost <- abs(outer(left$x, right$x, "-")) + abs(outer(left$y, right$y, "-"))
  cost[matrix(stats::runif(n * n) > 0.05, n, n)] <- Inf

  built <- couplr:::.balance_flow_problem(
    cost, couplr:::.refined_hierarchy(left, right, "g", exact = 1L))
  solved <- couplr:::.flow_solve(built$problem)
  cert <- verify_flow(solved, tol = 1e-9)

  expect_identical(solved$status, "optimal")
  expect_gt(max(abs(solved$potential)), 1e6)

  # One unit in the last place of a potential this size is around 1e-9, and the
  # worst residual arc sits within a few of them: that is the resolution an
  # absolute 1e-9 is read at, rather than above. The tolerance the certificate
  # used scales with the same numbers, which puts it orders of magnitude clear.
  ulp <- .Machine$double.eps * max(abs(solved$potential))
  expect_gt(ulp, 5e-10)
  expect_lt(abs(cert$min_residual_reduced_cost), 4 * ulp)
  expect_gt(cert$dual_tolerance, 1e-3)
  expect_true(cert$certified_optimal)

  report <- couplr:::.cardinality_solve(left, right, cost, refined = "g",
                                        exact = 1L)
  expect_identical(report$stopped_on, "optimality")
  expect_identical(report$gap, 0L)
  expect_true(report$certified)
})
