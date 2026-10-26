# The certification layer: verify_assignment() and the computed status field.
#
# The gap this fills: jv is the de facto oracle for roughly seventeen per-solver
# test files, and jv itself is only ever ground-truthed against bruteforce at
# n <= 6. A certificate does not rely on any solver being right. Dual
# feasibility is checked against the cost matrix itself, so if the duals were
# wrong the check fails rather than agreeing with a wrong answer.

test_that("a certificate proves a small dense solve optimal", {
  set.seed(101)
  cost <- cert_problem(8, 8)
  res  <- assignment(cost, method = "jv")
  cert <- verify_assignment(res, cost)

  expect_s3_class(cert, "assignment_certificate")
  expect_true(cert$primal_feasible)
  expect_true(cert$dual_feasible)
  expect_true(cert$complementary_slackness)
  expect_true(cert$certified_optimal)
  expect_equal(cert$primal_objective, res$total_cost, tolerance = CERT_COST_TOL)
  expect_lt(abs(cert$duality_gap), CERT_COST_TOL)
  # An exhaustive scan proves the minimum it observed, so the bound carries
  # nothing but the gap.
  expect_equal(cert$certified_reduced_cost_floor, cert$min_reduced_cost)
  expect_lt(cert$max_suboptimality, CERT_COST_TOL)
})

test_that("the certificate agrees with brute-force enumeration", {
  skip_if_not_installed("combinat")
  set.seed(102)
  for (n in 3:5) {
    cost <- cert_problem(n, n)
    perms <- combinat::permn(n)
    best <- min(vapply(perms, function(p) sum(cost[cbind(seq_len(n), p)]), numeric(1)))

    cert <- verify_assignment(assignment(cost, method = "jv"), cost)
    expect_true(cert$certified_optimal, info = paste("n =", n))
    expect_equal(cert$primal_objective, best, tolerance = CERT_COST_TOL,
                 info = paste("n =", n))
    # The dual objective is a lower bound on every feasible assignment, so it
    # must equal the enumerated optimum too.
    expect_equal(cert$dual_objective, best, tolerance = CERT_COST_TOL,
                 info = paste("n =", n))
  }
})

test_that("every solver in the registry returns a certifiable answer", {
  skip_on_cran()
  set.seed(103)
  shapes <- list(c(6, 6), c(5, 12), c(10, 10))
  kinds  <- c("continuous", "integer", "binary")

  for (shape in shapes) {
    n <- shape[1]; m <- shape[2]
    for (kind in kinds) {
      cost <- cert_problem(n, m, kind = kind)
      for (method in cert_methods_for(n, m, kind)) {
        if (cert_is_known_suboptimal(method, n, m)) next
        res <- cert_try_solve(cost, method)
        if (is.null(res)) next
        cert <- verify_assignment(res, cost)
        label <- sprintf("%s on %dx%d %s", method, n, m, kind)
        expect_true(cert$primal_feasible, info = label)
        expect_true(cert$certified_optimal, info = label)
      }
    }
  }
})

test_that("the known-suboptimal solvers still are", {
  # Each entry in cert_known_suboptimal() is a live bug, skipped by the sweep
  # above. This asserts it still fails, so an entry has to be deleted when the
  # bug is fixed instead of quietly masking a solver that now works. The
  # registry is empty, so the sweep covers every solver and this holds
  # vacuously.
  skip_on_cran()
  set.seed(116)
  cost <- cert_problem(3, 8)

  fixed <- Filter(Negate(is.null), lapply(cert_known_suboptimal(), function(known) {
    if (!identical(known$shape, "wide")) return(NULL)
    res <- cert_try_solve(cost, known$method)
    if (is.null(res)) return(NULL)
    if (!verify_assignment(res, cost)$certified_optimal) return(NULL)
    paste(known$method, "is fixed; remove its entry from",
          "cert_known_suboptimal() -", known$issue)
  }))

  expect_equal(fixed, list())
})

test_that("the certificate catches an assignment that is not optimal", {
  set.seed(104)
  cost <- cert_problem(6, 6)
  res  <- assignment(cost, method = "jv")

  # Swap two rows' partners. The result is still a valid permutation, so a
  # feasibility-only check passes it; only slackness against the duals rejects
  # it.
  broken <- res$match
  broken[c(1, 2)] <- broken[c(2, 1)]
  expect_gt(sum(cost[cbind(seq_len(6), broken)]), res$total_cost)

  cert <- verify_assignment(broken, cost)
  expect_true(cert$primal_feasible)
  expect_true(cert$dual_feasible)
  expect_false(cert$certified_optimal)
  expect_false(cert$cs_matched_tight)
})

test_that("the certificate rejects duals that do not certify", {
  set.seed(105)
  cost <- cert_problem(7, 7)
  res  <- assignment_duals(cost)

  # Raising a row potential above what any arc supports breaks dual
  # feasibility; the check must not take the duals on trust.
  bad <- list(u = res$u, v = res$v)
  bad$u[1] <- bad$u[1] + 10
  cert <- verify_assignment(res, cost, duals = bad)
  expect_false(cert$dual_feasible)
  expect_false(cert$certified_optimal)
  expect_lt(cert$min_reduced_cost, -CERT_TOL)
  expect_gt(cert$worst_i, 0L)
})

test_that("complementary slackness on unmatched columns is checked", {
  # This is the condition a naive verifier omits. A rectangular problem is
  # optimal only if every column no row took carries v_j = 0; lowering one of
  # them leaves dual feasibility and matched-arc tightness intact while making
  # the dual objective no longer equal the primal.
  set.seed(106)
  cost <- cert_problem(4, 12)
  res  <- assignment_duals(cost)

  clean <- verify_assignment(res, cost)
  expect_true(clean$certified_optimal)
  expect_true(clean$cs_unmatched_free)
  expect_lt(clean$max_v_unmatched, CERT_TOL)

  free_col <- setdiff(seq_len(12), res$match[res$match > 0])[1]
  tampered <- list(u = res$u, v = res$v)
  tampered$v[free_col] <- -0.25

  cert <- verify_assignment(res, cost, duals = tampered)
  expect_true(cert$dual_feasible)
  expect_true(cert$cs_matched_tight)
  expect_false(cert$cs_unmatched_free)
  expect_false(cert$certified_optimal)
})

test_that("certification handles rectangular problems in both orientations", {
  set.seed(107)
  wide <- cert_problem(5, 15)
  tall <- t(wide)

  cw <- verify_assignment(assignment(wide), wide)
  ct <- verify_assignment(assignment(tall), tall)

  expect_true(cw$certified_optimal)
  expect_true(ct$certified_optimal)
  expect_false(cw$transposed)
  expect_true(ct$transposed)
  expect_equal(cw$primal_objective, ct$primal_objective, tolerance = CERT_COST_TOL)
})

test_that("certification handles maximization and forbidden edges", {
  set.seed(108)
  cost <- cert_problem(7, 9, sparsity = 0.3)

  cmin <- verify_assignment(assignment(cost), cost)
  expect_true(cmin$certified_optimal)
  expect_equal(cmin$n_forbidden_matched, 0)

  res  <- assignment(cost, maximize = TRUE)
  cmax <- verify_assignment(res, cost, maximize = TRUE)
  expect_true(cmax$certified_optimal)
  expect_equal(cmax$primal_objective, res$total_cost, tolerance = CERT_COST_TOL)
})

test_that("negative costs certify", {
  set.seed(109)
  cost <- cert_problem(6, 6, kind = "negative")
  cert <- verify_assignment(assignment(cost), cost)
  expect_true(cert$certified_optimal)
})

test_that("a matching that uses a forbidden pair is not primal feasible", {
  set.seed(110)
  cost <- cert_problem(5, 5)
  cost[1, 1] <- NA_real_
  res <- assignment(cost)

  forced <- res$match
  taken  <- which(forced == 1L)
  forced[taken] <- forced[1]
  forced[1] <- 1L

  cert <- verify_assignment(forced, cost)
  expect_false(cert$primal_feasible)
  expect_gt(cert$n_forbidden_matched, 0)
  expect_false(cert$certified_optimal)
})

test_that("a matching that claims a column twice is not primal feasible", {
  set.seed(111)
  cost <- cert_problem(5, 5)
  broken <- assignment(cost)$match
  broken[2] <- broken[1]

  cert <- verify_assignment(broken, cost)
  expect_false(cert$primal_feasible)
  expect_gt(cert$n_duplicate_cols, 0)
  expect_false(cert$certified_optimal)
})

test_that("verify_assignment validates its inputs", {
  cost <- cert_problem(4, 4)
  res  <- assignment(cost)

  expect_error(verify_assignment(res), "cost.*required")
  expect_error(verify_assignment(res, cost, tol = -1), "non-negative")
  expect_error(verify_assignment(res, cost, maximize = NA), "TRUE or FALSE")
  expect_error(verify_assignment(res, cost[1:2, ]), "length")
  expect_error(verify_assignment(res, cost, duals = list(u = 1, v = 1)), "length")
  expect_error(verify_assignment("nope", cost), "match")
})

# ---------------------------------------------------------------------------
# Computed status
# ---------------------------------------------------------------------------

test_that("status is drawn from the closed vocabulary on every solver", {
  skip_on_cran()
  set.seed(112)
  cost <- cert_problem(8, 10)
  for (method in cert_methods_for(8, 10, "continuous")) {
    res <- cert_try_solve(cost, method)
    if (is.null(res)) next
    expect_true(res$status %in% solver_status_values(), info = method)
    expect_identical(res$status, "optimal", info = method)
  }
})

test_that("a caller-supplied auction epsilon is reported as eps_optimal", {
  set.seed(113)
  cost <- cert_problem(10, 10)
  expect_identical(assignment(cost, method = "auction")$status, "optimal")
  expect_identical(assignment(cost, method = "auction", auction_eps = 0.1)$status,
                   "eps_optimal")
})

test_that("an invalid status cannot be constructed", {
  expect_error(couplr:::.validate_status("nearly"), "Unknown solver status")
  expect_error(couplr:::.validate_status(c("optimal", "partial")), "single")
  expect_error(couplr:::.validate_status(NA_character_), "non-NA")
})

# ---------------------------------------------------------------------------
# Status on the matching layer
# ---------------------------------------------------------------------------

test_that("match_couples reports a status computed from what it achieved", {
  set.seed(117)
  left  <- data.frame(id = paste0("L", 1:20), x = stats::runif(20))
  right <- data.frame(id = paste0("R", 1:20), x = stats::runif(20))

  m <- match_couples(left, right, vars = "x")
  expect_true(m$status %in% solver_status_values())
  expect_identical(m$status, "optimal")
  expect_equal(length(m$unmatched$left), 0L)
})

test_that("a caliper that leaves units unmatched is reported as partial", {
  set.seed(118)
  # Five right units sit within reach of the left units and ten do not, so a
  # tight max_distance leaves most of the left side unmatched while the problem
  # stays solvable.
  left  <- data.frame(id = paste0("L", 1:15), x = stats::runif(15, 0, 1))
  right <- data.frame(
    id = paste0("R", 1:15),
    x  = c(stats::runif(5, 0, 1), stats::runif(10, 20, 21))
  )

  m <- suppressWarnings(
    match_couples(left, right, vars = "x", max_distance = 1)
  )
  expect_identical(m$status, "partial")
  expect_gt(length(m$unmatched$left), 0L)
  expect_gt(nrow(m$pairs), 0L)
})

test_that("an explicit greedy request is reported as heuristic, not optimal", {
  set.seed(119)
  left  <- data.frame(id = paste0("L", 1:12), x = stats::runif(12))
  right <- data.frame(id = paste0("R", 1:12), x = stats::runif(12))

  m <- match_couples(left, right, vars = "x", method = "greedy")
  expect_identical(m$status, "heuristic")
})

test_that("status survives return_unmatched and return_diagnostics being off", {
  set.seed(120)
  left  <- data.frame(id = paste0("L", 1:10), x = stats::runif(10))
  right <- data.frame(id = paste0("R", 1:10), x = stats::runif(10))

  # Both inputs the status is derived from are removed by these defaults, which
  # is why it is computed before they go and stored at the top level.
  m <- match_couples(left, right, vars = "x",
                     return_unmatched = FALSE, return_diagnostics = FALSE)
  expect_identical(m$status, "optimal")
  expect_null(m$unmatched)
  expect_null(m$info$solver)
})

test_that("a k:1 match short of its requested pairs is partial", {
  # Four left units ask for two partners each, and five controls exist, so three
  # of the eight requested pairs cannot be placed. Every left unit still holds a
  # partner, which is what a status derived from unmatched units alone reads as
  # a complete matching.
  left  <- data.frame(id = paste0("L", 1:4), x = c(1, 2, 3, 4))
  right <- data.frame(id = paste0("R", 1:5), x = c(1.1, 2.1, 3.1, 4.1, 2.5))

  m <- match_couples(left, right, vars = "x", ratio = 2, check_costs = FALSE)

  expect_identical(m$status, "partial")
  expect_lt(nrow(m$pairs), nrow(left) * 2L)
  expect_length(m$unmatched$left, 0L)
})

# Block "a" is Hall-deficient and its admissible costs are far enough apart that
# the sentinel cannot order matchings by cardinality in double precision, so it
# takes the greedy fallback; block "b" solves normally.
hall_deficient_blocks <- function() {
  list(
    left = data.frame(id = c("L1", "L2", "L3", "M1", "M2"),
                      x = c(0, 0, 1, 5, 6),
                      y = c(0, 1e15, 0, 0, 0),
                      blk = c("a", "a", "a", "b", "b")),
    right = data.frame(id = c("R1", "R2", "R3", "S1", "S2"),
                       x = c(0, 1, 1, 5.1, 6.1),
                       y = c(0, 0, 1e15, 0, 0),
                       blk = c("a", "a", "a", "b", "b"))
  )
}

test_that("a blocked match reports the method that ran, not the one requested", {
  d <- hall_deficient_blocks()

  blocked <- suppressWarnings(match_couples(
    d$left, d$right, vars = c("x", "y"), calipers = list(x = 0.5),
    block_id = "blk", return_diagnostics = TRUE, check_costs = FALSE
  ))

  # The same fallback reached without blocking, which is the contract the
  # blocked path has to meet: both report the fallback, not the request.
  unblocked <- suppressWarnings(match_couples(
    d$left[d$left$blk == "a", ], d$right[d$right$blk == "a", ],
    vars = c("x", "y"), calipers = list(x = 0.5),
    return_diagnostics = TRUE, check_costs = FALSE
  ))

  expect_true("greedy_sorted" %in% blocked$info$solver)
  expect_false("auto" %in% blocked$info$solver)
  expect_identical(blocked$status, "heuristic")
  expect_identical(blocked$status, unblocked$status)
})

test_that("the two blocked branches return the same info", {
  d <- hall_deficient_blocks()
  args <- list(
    d$left, d$right, vars = c("x", "y"), calipers = list(x = 0.5),
    block_id = "blk", return_diagnostics = TRUE, check_costs = FALSE
  )

  seq_m <- suppressWarnings(do.call(match_couples, c(args, parallel = FALSE)))
  par_m <- suppressWarnings(do.call(match_couples, c(args, parallel = TRUE)))

  expect_identical(names(seq_m$info), names(par_m$info))
  expect_identical(seq_m$info$solver, par_m$info$solver)
  expect_identical(seq_m$info$block_summary, par_m$info$block_summary)
  expect_identical(seq_m$status, par_m$status)
})

test_that("every block gets a summary row, including one with nothing to match", {
  # Block "b" has a left unit and no right unit, so it runs no solve.
  left  <- data.frame(id = c("L1", "L2", "M1"), x = c(1, 2, 9),
                      blk = c("a", "a", "b"))
  right <- data.frame(id = c("R1", "R2"), x = c(1.1, 2.1),
                      blk = c("a", "a"))

  m <- match_couples(left, right, vars = "x", block_id = "blk",
                     return_diagnostics = TRUE, check_costs = FALSE)

  expect_equal(nrow(m$info$block_summary), 2L)
  expect_equal(m$info$block_summary$n_matched, c(2L, 0L))
  expect_equal(m$info$block_summary$n_unmatched_left, c(0L, 1L))
})

# ---------------------------------------------------------------------------
# Dispatch transparency
# ---------------------------------------------------------------------------

test_that("explain_dispatch reports the rule assignment actually acts on", {
  set.seed(114)
  cases <- list(
    list(cost = cert_problem(4, 4),                    rule = "tiny",
         method = "bruteforce"),
    list(cost = cert_problem(20, 20, kind = "binary"), rule = "no_cost_scale",
         method = "hk01"),
    list(cost = cert_problem(20, 20),                  rule = "default",
         method = "jv"),
    # Shape and sparsity are not properties any rule reads, so both of these
    # reach the default rather than a solver of their own.
    list(cost = cert_problem(10, 40),                  rule = "default",
         method = "jv"),
    list(cost = cert_problem(30, 30, sparsity = 0.7),  rule = "default",
         method = "jv")
  )
  for (case in cases) {
    ex  <- explain_dispatch(case$cost)
    res <- assignment(case$cost)
    expect_identical(ex$method, case$method, info = case$rule)
    expect_identical(ex$rule, case$rule, info = case$rule)
    expect_identical(res$method_used, ex$method, info = case$rule)
    expect_identical(res$dispatch$rule, ex$rule, info = case$rule)
    expect_false(res$dispatch$explicit)
  }
})

test_that("explain_dispatch records that a named method skipped the rules", {
  cost <- cert_problem(20, 20)
  ex <- explain_dispatch(cost, method = "csa")
  expect_true(ex$explicit)
  expect_identical(ex$method, "csa")
  expect_identical(ex$auto_method, "jv")

  res <- assignment(cost, method = "csa")
  expect_true(res$dispatch$explicit)
  expect_true(is.na(res$dispatch$rule))
})

test_that("every dispatch rule is reachable and exactly one fires", {
  set.seed(115)
  probes <- list(cert_problem(4, 4), cert_problem(20, 20, kind = "binary"),
                 cert_problem(20, 20), cert_problem(10, 40),
                 cert_problem(30, 30, sparsity = 0.7))
  fired <- character(0)
  for (cost in probes) {
    ex <- explain_dispatch(cost)
    expect_equal(sum(ex$considered$fired), 1L)
    fired <- c(fired, ex$rule)
  }
  expect_setequal(fired, c("tiny", "no_cost_scale", "default"))

  # Every rule in the table is reachable: the three that fired above are the
  # three the table holds.
  expect_setequal(
    vapply(couplr:::.dispatch_rules, function(r) r$id, character(1)),
    c("tiny", "no_cost_scale", "default")
  )
})

test_that("no suboptimality bound is reported for an infeasible candidate", {
  # "the most any feasible solution can beat this one by" has no answer when
  # the candidate is not feasible. On a zero-cost problem with an uncovered
  # row and zero duals the two objectives agree at zero, and the bound came
  # back as 0 beside primal_feasible = FALSE, which reads as a guarantee.
  cm <- matrix(0, 3, 3)
  partial <- c(1L, 2L, 0L)
  v <- verify_assignment(partial, cost = cm,
                         duals = list(u = rep(0, 3), v = rep(0, 3)))
  expect_false(v$primal_feasible)
  expect_true(v$structurally_valid_matching)
  expect_false(v$all_rows_matched)
  expect_true(is.na(v$max_suboptimality))

  # The complete matching on the same problem still reports a bound.
  full <- verify_assignment(c(1L, 2L, 3L), cost = cm,
                            duals = list(u = rep(0, 3), v = rep(0, 3)))
  expect_true(full$primal_feasible)
  expect_equal(full$max_suboptimality, 0)
})

test_that("the suboptimality bound carries the objectives own rounding", {
  # max_suboptimality is assembled from two compensated sums. Compensated
  # summation buys back the accumulation error rather than removing it, so a
  # bound that ignored the sums' own envelope was an estimate presented as a
  # bound. On costs near 1e6 the envelope is the 2u * sum|term| scale.
  set.seed(1)
  m <- matrix(runif(400, 1, 1e6), 20, 20)
  v <- verify_assignment(assignment(m, method = "jv"), cost = m,
                         arithmetic = "double")
  expect_true(v$certified_optimal)
  expect_gt(v$max_suboptimality, 0)
  # Loose enough not to pin the constant, tight enough to catch an envelope
  # that has gone from ulps to something visible in the objective.
  expect_lt(v$max_suboptimality, 1e-6 * abs(v$primal_objective))

  # An exact certificate proves the matching attains the optimum, so the bound
  # it reports is the number the double one was bounding.
  e <- verify_assignment(assignment(m, method = "jv"), cost = m)
  expect_identical(e$arithmetic, "exact")
  expect_identical(e$max_suboptimality, 0)

  # Where every term is exactly zero the bound is exactly zero: rounding
  # outward on a zero term would report slack the arithmetic proved absent.
  z <- verify_assignment(c(1L, 2L, 3L), cost = matrix(0, 3, 3),
                         duals = list(u = rep(0, 3), v = rep(0, 3)))
  expect_identical(z$max_suboptimality, 0)
  expect_true(z$certified_optimal)
})
