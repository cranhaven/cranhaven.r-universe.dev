# ==============================================================================
# Balance-constrained cardinality matching
# ==============================================================================
# Three claims are checked here, and they are checked against statements of the
# problem that never touch the search.
#
#   The network solves. .flow_solve() returns an integral flow on the balance
#   network, verify_flow() certifies it, the certificate fails when a potential
#   is moved, and the solved flow reaches the same objective as the cheapest
#   encoding of any matched set.
#
#   The answer is the answer. Over every partial injection of a tiny instance,
#   the matched set the driver returns has the enumerated optimum's objective,
#   its cardinality and its imbalance at every level, and with moment
#   constraints in place the pairs it returns satisfy every row recomputed from
#   the coefficients.
#
#   The gap is never a claim nobody checked. Interrupted at a node limit, at a
#   time limit, or by a predicate, the driver returns a matched set that
#   satisfies every stated constraint, a gap above zero, and no certificate.
# ==============================================================================

# --- the network under the solver ---------------------------------------------

test_that(".flow_solve() returns an integral flow on the balance network", {
  for (seed in 1:25) {
    inst <- card_instance(seed)
    built <- card_build(inst)
    solved <- .flow_solve(built$problem)

    expect_identical(solved$status, "optimal")
    expect_equal(solved$flow, round(solved$flow), tolerance = 1e-9)
  }
})

test_that("the solved optimum is the cheapest matched set there is", {
  for (seed in 1:25) {
    inst <- card_instance(seed)
    built <- card_build(inst)
    solved <- .flow_solve(built$problem)

    enumerated <- Inf
    for (matching in card_all_matchings(inst$cost)) {
      flow <- .balance_flow_encode(matching, built$index)
      if (is.null(flow)) next
      enumerated <- min(enumerated, sum(built$problem$arcs$cost * flow))
    }

    expect_equal(sum(built$problem$arcs$cost * solved$flow), enumerated)
  }
})

test_that("verify_flow() certifies the solve and fails on a moved potential", {
  for (seed in 1:25) {
    inst <- card_instance(seed)
    built <- card_build(inst)
    solved <- .flow_solve(built$problem)

    expect_true(verify_flow(solved)$certified_optimal)

    potential <- solved$potential
    node <- built$index$layout$node_left(1L)
    potential[[node]] <- potential[[node]] + 7.5
    moved <- verify_flow(solved$flow, built$problem, potential = potential)
    expect_false(moved$certified_optimal)
  }
})

test_that(".balance_flow_audit() passes on the solver's own flow", {
  for (seed in 1:25) {
    inst <- card_instance(seed)
    built <- card_build(inst)
    solved <- .flow_solve(built$problem)
    audit <- .balance_flow_audit(built$problem, built$index, solved$flow,
                                 cost = inst$cost)

    expect_true(audit$feasible)
    expect_equal(audit$identity_gap, 0)
    expect_equal(audit$pairs_identity_gap, 0)
    expect_equal(audit$n_self_crossing, 0)
  }
})

# --- brute force is the oracle ------------------------------------------------

test_that("the match is the enumerated optimum with no moment constraint", {
  for (seed in 101:112) {
    inst <- card_instance(seed)
    built <- card_build(inst)
    truth <- card_brute(inst, built)

    report <- .cardinality_solve(inst$left, inst$right, inst$cost,
                                 refined = inst$refined, exact = inst$exact)

    expect_identical(report$engine, "flow")
    expect_true(report$certified)
    expect_identical(report$n_matched, truth$n_pairs)
    expect_equal(report$objective, truth$objective)
    expect_equal(card_report_imbalance(report, inst$n_levels), truth$imbalance)
  }
})

test_that("the match is the enumerated optimum under moment constraints", {
  for (seed in 201:210) {
    inst <- card_instance(seed, max_units = 4L)
    built <- card_build(inst)
    bound <- 0.05 + 0.6 * ((seed %% 5L) / 5)
    rows <- card_coefs(inst, max_std_diff = bound, vars = "x")
    truth <- card_brute(inst, built, rows$coefs)

    report <- .cardinality_solve(inst$left, inst$right, inst$cost,
                                 refined = inst$refined, exact = inst$exact,
                                 max_std_diff = bound, vars = "x",
                                 node_limit = 300L)

    expect_identical(report$engine, "branch_bound")
    expect_false(card_pairs_violate(report, rows$coefs))
    expect_gte(report$best_possible, truth$n_pairs)
    if (report$stopped_on %in% c("optimality", "bound")) {
      expect_identical(report$n_matched, truth$n_pairs)
      expect_equal(report$objective, truth$objective)
      expect_equal(card_report_imbalance(report, inst$n_levels),
                   truth$imbalance)
    }
  }
})

test_that("a mean-difference bound is enumerated the same way", {
  for (seed in 301:308) {
    inst <- card_instance(seed, max_units = 4L)
    built <- card_build(inst)
    spec <- list(list(var = "y", stat = "mean_diff", max = 0.25, min = -0.25))
    rows <- card_coefs(inst, moments = spec)
    truth <- card_brute(inst, built, rows$coefs)

    report <- .cardinality_solve(inst$left, inst$right, inst$cost,
                                 refined = inst$refined, exact = inst$exact,
                                 moments = spec, node_limit = 300L)

    expect_false(card_pairs_violate(report, rows$coefs))
    expect_gte(report$best_possible, truth$n_pairs)
    if (report$stopped_on %in% c("optimality", "bound")) {
      expect_identical(report$n_matched, truth$n_pairs)
      expect_equal(report$objective, truth$objective)
    }
  }
})

# --- exact arithmetic ---------------------------------------------------------

test_that("an optimal balance flow certifies exactly, zero-cost arcs included", {
  # The balance network joins zero-labelled nodes by zero-cost arcs, where a
  # shortest-path filter that took a zero difference for an improvement
  # reported a negative cycle on an optimal flow.
  for (seed in c(1:25, 101:112)) {
    built <- card_build(card_instance(seed))
    solved <- .flow_solve(built$problem)
    cert <- verify_flow(solved, arithmetic = "exact")
    expect_true(cert$certified_optimal, info = paste("seed", seed))
  }
})

test_that("a node's bound never exceeds a moment-feasible matched set", {
  for (seed in 201:208) {
    inst <- card_instance(seed, max_units = 4L)
    built <- card_build(inst)
    coefs <- card_coefs(inst, max_std_diff = 0.2, vars = c("x", "y"))$coefs
    set.seed(seed)
    for (draw in 1:3) {
      lambda <- stats::rexp(length(coefs)) * c(0, 0.1, 10)[[draw]]
      fl <- .cardinality_flow(built$problem, built$index, coefs = coefs,
                              lambda = lambda)
      for (matching in card_all_matchings(inst$cost)) {
        flow <- .balance_flow_encode(matching, built$index)
        if (is.null(flow)) next
        left_idx <- which(matching > 0L)
        read <- list(left = left_idx, right = matching[left_idx])
        rows <- .cardinality_rows_exact(built$index, coefs, read)
        if (any(rows$sign > 0L)) next
        objective <- lap_exact_dot(built$problem$arcs$cost, flow)
        expect_lte(.exact_cmp(fl$bound_exact, objective), 0L)
      }
    }
  }
})

test_that("with no multipliers the bound is the optimum itself, exactly", {
  for (seed in 101:112) {
    inst <- card_instance(seed)
    built <- card_build(inst)
    fl <- .cardinality_flow(built$problem, built$index)
    best <- Inf
    for (matching in card_all_matchings(inst$cost)) {
      flow <- .balance_flow_encode(matching, built$index)
      if (is.null(flow)) next
      best <- .exact_min(list(best, lap_exact_dot(built$problem$arcs$cost, flow)))
    }
    expect_true(fl$certified)
    expect_identical(.exact_cmp(fl$bound_exact, best), 0L)
    expect_identical(.exact_cmp(fl$objective_exact, best), 0L)
  }
})

test_that("a flow short of the optimum is repaired to it before it is read", {
  repaired <- 0L
  for (seed in 101:112) {
    inst <- card_instance(seed)
    built <- card_build(inst)
    solved <- .flow_solve(built$problem)
    optimum <- lap_exact_dot(built$problem$arcs$cost, solved$flow)
    for (matching in card_all_matchings(inst$cost)) {
      flow <- .balance_flow_encode(matching, built$index)
      if (is.null(flow)) next
      if (.exact_cmp(lap_exact_dot(built$problem$arcs$cost, flow), optimum) <= 0L) {
        next
      }
      start <- list(flow = flow, potential = numeric(built$problem$n_nodes))
      step <- .cardinality_step(built$problem, built$index, start, list(),
                                numeric(0))
      expect_true(step$recovered)
      expect_gt(step$n_cancelled, 0)
      reached <- lap_exact_dot(built$problem$arcs$cost, step$flow)
      expect_identical(.exact_cmp(reached, optimum), 0L)
      expect_identical(.exact_cmp(step$bound_exact, optimum), 0L)
      repaired <- repaired + 1L
      break
    }
  }
  expect_gt(repaired, 0L)
})

test_that("a certified report under moment rows closed its bound exactly", {
  checked <- 0L
  for (seed in 201:210) {
    inst <- card_instance(seed, max_units = 4L)
    built <- card_build(inst)
    coefs <- card_coefs(inst, max_std_diff = 0.3, vars = "x")$coefs
    run <- .cardinality_branch_bound(built$problem, built$index, coefs = coefs,
                                     node_limit = 300L)
    if (!isTRUE(run$certified)) next
    expect_true(all(.cardinality_rows_exact(run$index, coefs,
                                            run$solution$read)$sign <= 0L))
    expect_identical(run$best_possible, run$solution$read$n_pairs)
    expect_lte(run$bound, run$objective)
    checked <- checked + 1L
  }
  expect_gt(checked, 0L)
})

# --- the unconstrained reduction ----------------------------------------------

test_that("no balance requirement reduces to maximum-cardinality matching", {
  for (seed in 401:415) {
    set.seed(seed)
    n <- sample(4:20, 1L)
    m <- sample(4:20, 1L)
    left <- data.frame(id = seq_len(n), x = round(stats::rnorm(n), 3))
    right <- data.frame(id = seq_len(m) + n, x = round(stats::rnorm(m), 3))
    cost <- abs(outer(left$x, right$x, "-"))

    report <- .cardinality_solve(left, right, cost)
    reference <- match_couples(left, right, vars = "x", left_id = "id",
                               right_id = "id", distance = "euclidean")

    expect_identical(report$n_matched, reference$info$n_matched)
    expect_equal(report$total_distance, reference$info$total_distance)
    expect_true(report$certified)
    expect_identical(report$gap, 0L)
  }
})

# --- properties ---------------------------------------------------------------

test_that("relaxing a bound never costs matched units", {
  for (seed in 501:508) {
    inst <- card_instance(seed, max_units = 4L)
    tight <- .cardinality_solve(inst$left, inst$right, inst$cost,
                                refined = inst$refined, exact = inst$exact,
                                max_std_diff = 0.1, vars = "x",
                                node_limit = 300L)
    loose <- .cardinality_solve(inst$left, inst$right, inst$cost,
                                refined = inst$refined, exact = inst$exact,
                                max_std_diff = 1.5, vars = "x",
                                node_limit = 300L)
    if (tight$certified && loose$certified) {
      expect_gte(loose$n_matched, tight$n_matched)
    }
    for (report in list(tight, loose)) {
      expect_gte(report$best_possible, report$n_matched)
      expect_gte(report$gap, 0L)
      if (report$certified) expect_identical(report$gap, 0L)
    }
  }
})

# --- interruption -------------------------------------------------------------

test_that("a node limit returns a feasible match and an honest gap", {
  hard <- card_hard_instance()
  rows <- card_coefs(list(left = hard$left, right = hard$right),
                     max_std_diff = hard$max_std_diff, vars = hard$vars)

  report <- .cardinality_solve(hard$left, hard$right, hard$cost,
                               max_std_diff = hard$max_std_diff,
                               vars = hard$vars, node_limit = 5L)

  expect_identical(report$status, "iteration_limit")
  expect_identical(report$stopped_on, "node_limit")
  expect_gt(report$gap, 0L)
  expect_false(report$certified)
  expect_false(card_pairs_violate(report, rows$coefs))
  expect_true(all(report$constraints$satisfied))
})

test_that("a time limit returns a feasible match and an honest gap", {
  hard <- card_hard_instance()
  rows <- card_coefs(list(left = hard$left, right = hard$right),
                     max_std_diff = hard$max_std_diff, vars = hard$vars)

  report <- .cardinality_solve(hard$left, hard$right, hard$cost,
                               max_std_diff = hard$max_std_diff,
                               vars = hard$vars, node_limit = 10000L,
                               time_limit = 0.05)

  expect_identical(report$status, "iteration_limit")
  expect_identical(report$stopped_on, "time_limit")
  expect_gt(report$gap, 0L)
  expect_false(report$certified)
  expect_false(card_pairs_violate(report, rows$coefs))
})

test_that("a stop predicate returns a feasible match and an honest gap", {
  hard <- card_hard_instance()
  rows <- card_coefs(list(left = hard$left, right = hard$right),
                     max_std_diff = hard$max_std_diff, vars = hard$vars)

  report <- .cardinality_solve(hard$left, hard$right, hard$cost,
                               max_std_diff = hard$max_std_diff,
                               vars = hard$vars, node_limit = 10000L,
                               should_stop = function(state) {
                                 state$n_nodes >= 3L
                               })

  expect_identical(report$status, "iteration_limit")
  expect_identical(report$stopped_on, "interrupt")
  expect_identical(report$n_nodes, 3L)
  expect_gt(report$gap, 0L)
  expect_false(report$certified)
  expect_false(card_pairs_violate(report, rows$coefs))
})

test_that("an interrupt condition leaves the incumbent standing", {
  hard <- card_hard_instance()
  rows <- card_coefs(list(left = hard$left, right = hard$right),
                     max_std_diff = hard$max_std_diff, vars = hard$vars)

  report <- .cardinality_solve(hard$left, hard$right, hard$cost,
                               max_std_diff = hard$max_std_diff,
                               vars = hard$vars, node_limit = 10000L,
                               should_stop = function(state) {
                                 if (state$n_nodes >= 2L) {
                                   signalCondition(structure(
                                     class = c("interrupt", "condition"),
                                     list(message = "", call = NULL)))
                                 }
                                 FALSE
                               })

  expect_identical(report$stopped_on, "interrupt")
  expect_identical(report$status, "iteration_limit")
  expect_false(report$certified)
  expect_gt(report$gap, 0L)
  expect_false(card_pairs_violate(report, rows$coefs))
})

# --- degenerate inputs --------------------------------------------------------

test_that("an empty pool matches nothing and says so", {
  left <- data.frame(g = character(0), x = numeric(0))
  right <- data.frame(g = c("a", "b"), x = c(0.1, 0.4))
  cost <- matrix(numeric(0), nrow = 0L, ncol = 2L)

  report <- .cardinality_solve(left, right, cost, refined = "g")
  expect_identical(report$n_matched, 0L)
  expect_identical(report$gap, 0L)
  expect_true(report$certified)

  both <- .cardinality_solve(left, left, matrix(numeric(0), 0L, 0L),
                             refined = "g")
  expect_identical(both$n_matched, 0L)
  expect_true(both$certified)
})

test_that("one unit a side matches the pair or nothing", {
  left <- data.frame(g = "a", x = 0)
  right <- data.frame(g = "a", x = 1)

  joined <- .cardinality_solve(left, right, matrix(2, 1L, 1L), refined = "g")
  expect_identical(joined$n_matched, 1L)
  expect_true(joined$certified)

  apart <- .cardinality_solve(left, right, matrix(Inf, 1L, 1L), refined = "g")
  expect_identical(apart$n_matched, 0L)
  expect_true(apart$certified)
})

test_that("every pair forbidden gives an empty certified match", {
  set.seed(9L)
  left <- data.frame(g = rep("a", 4L), x = stats::rnorm(4))
  right <- data.frame(g = rep("a", 5L), x = stats::rnorm(5))

  report <- .cardinality_solve(left, right, matrix(Inf, 4L, 5L), refined = "g")
  expect_identical(report$n_matched, 0L)
  expect_identical(nrow(report$pairs), 0L)
  expect_true(report$certified)
})

test_that("a bound no pair can meet returns zero pairs, certified", {
  left <- data.frame(x = c(0, 1, 2))
  right <- data.frame(x = c(5, 6, 7))
  cost <- abs(outer(left$x, right$x, "-"))

  report <- .cardinality_solve(left, right, cost, max_std_diff = 0,
                               vars = "x", node_limit = 300L)
  expect_identical(report$n_matched, 0L)
  expect_identical(report$gap, 0L)
  expect_true(report$certified)
  expect_true(all(report$constraints$satisfied))
})

# --- the report ---------------------------------------------------------------

test_that("a certified report prints its gap", {
  set.seed(3L)
  left <- data.frame(x = round(stats::rnorm(8), 3))
  right <- data.frame(x = round(stats::rnorm(12), 3))
  report <- .cardinality_solve(left, right,
                               abs(outer(left$x, right$x, "-")))
  expect_snapshot(print.cardinality_report(report))
})

test_that("an interrupted report prints its bound", {
  hard <- card_hard_instance()
  report <- .cardinality_solve(hard$left, hard$right, hard$cost,
                               max_std_diff = hard$max_std_diff,
                               vars = hard$vars, node_limit = 5L)
  expect_snapshot(print.cardinality_report(report))
})

test_that("the report carries the balance and the constraints it was given", {
  set.seed(4L)
  left <- data.frame(g = rep(c("a", "b"), each = 4L),
                     x = round(stats::rnorm(8), 3))
  right <- data.frame(g = rep(c("a", "b"), each = 5L),
                      x = round(stats::rnorm(10), 3))
  cost <- abs(outer(left$x, right$x, "-"))

  report <- .cardinality_solve(left, right, cost, refined = "g",
                               max_std_diff = 0.4, vars = "x",
                               node_limit = 300L)

  expect_s3_class(report, "cardinality_report")
  expect_named(report$constraints,
               c("kind", "target", "bound", "achieved", "slack", "satisfied"))
  expect_named(report$balance,
               c("level", "category", "n_left", "n_right", "imbalance"))
  expect_true(all(c("std_diff", "exact_balance") %in% report$constraints$kind))
  expect_setequal(report$balance$category, c("a", "b"))
  expect_identical(report$balance$n_left - report$balance$n_right,
                   report$balance$imbalance)
  expect_gt(report$precision_headroom, 1)
  expect_equal(report$shift, min(cost))
})
