# ==============================================================================
# Cost-scaling push-relabel
# ==============================================================================
# The solver reaches the optimum by a route no other solver here takes: a
# sequence of eps-optimal flows, each phase saturating what the smaller eps no
# longer admits and clearing the excess with pushes and relabels. Its answers
# are checked against a solver that gets there another way, and against the
# certificate, which is checked against the cost matrix itself.
# ==============================================================================

pr_total <- function(cost, maximize = FALSE) {
  assignment(cost, method = "push_relabel", maximize = maximize)$total_cost
}

jv_total <- function(cost, maximize = FALSE) {
  assignment(cost, method = "jv", maximize = maximize)$total_cost
}

test_that("push_relabel matches jv on integer costs", {
  set.seed(4242)
  for (trial in 1:40) {
    n <- sample(2:8, 1)
    m <- n + sample(0:3, 1)
    cost <- matrix(sample(0:50, n * m, replace = TRUE), n, m)
    expect_equal(pr_total(cost), jv_total(cost),
                 info = paste("trial", trial, "n =", n, "m =", m))
  }
})

test_that("push_relabel matches jv on real costs", {
  set.seed(99)
  for (trial in 1:30) {
    n <- sample(2:7, 1)
    m <- n + sample(0:2, 1)
    cost <- matrix(round(stats::runif(n * m, 0, 10), 3), n, m)
    expect_equal(pr_total(cost), jv_total(cost), tolerance = 1e-6,
                 info = paste("trial", trial))
  }
})

test_that("push_relabel matches jv when maximizing", {
  set.seed(11)
  for (trial in 1:25) {
    n <- sample(2:7, 1)
    cost <- matrix(sample(0:40, n * n, replace = TRUE), n, n)
    expect_equal(pr_total(cost, maximize = TRUE),
                 jv_total(cost, maximize = TRUE),
                 info = paste("trial", trial))
  }
})

test_that("push_relabel respects forbidden pairs", {
  set.seed(7)
  for (trial in 1:25) {
    n <- sample(3:6, 1)
    cost <- matrix(sample(0:30, n * n, replace = TRUE), n, n)
    cost[sample(seq_along(cost), max(1L, floor(n * n / 4)))] <- Inf

    ref <- tryCatch(assignment(cost, method = "jv"), error = function(e) e)
    got <- tryCatch(assignment(cost, method = "push_relabel"),
                    error = function(e) e)

    expect_equal(inherits(got, "error"), inherits(ref, "error"),
                 info = paste("trial", trial))
    if (inherits(ref, "error")) next

    expect_equal(got$total_cost, ref$total_cost, info = paste("trial", trial))
    expect_true(all(is.finite(cost[cbind(seq_len(n), got$match)])),
                info = paste("trial", trial))
  }
})

test_that("push_relabel answers certify against the cost matrix", {
  set.seed(313)
  for (trial in 1:10) {
    n <- sample(3:7, 1)
    cost <- matrix(sample(0:25, n * n, replace = TRUE), n, n)
    res <- assignment(cost, method = "push_relabel")
    cert <- verify_assignment(res, cost)
    expect_true(cert$certified_optimal, info = paste("trial", trial))
  }
})

test_that("push_relabel reports an infeasible instance rather than a matching", {
  # Both rows can only take column 1, so no complete matching exists.
  cost <- matrix(c(1, Inf, 1, Inf), nrow = 2, byrow = TRUE)
  expect_error(assignment(cost, method = "push_relabel"))
})

test_that("a one-cell problem is the whole matching", {
  res <- assignment(matrix(4.5, 1, 1), method = "push_relabel")
  expect_equal(res$match, 1L)
  expect_equal(res$total_cost, 4.5)
})

# ------------------------------------------------------------------------------
# The trace reads the solver's own per-phase record
# ------------------------------------------------------------------------------

test_that("the push_relabel trace reports the solver's matching", {
  set.seed(515)
  cost <- matrix(sample(0:20, 20, replace = TRUE), 4, 5)

  tr <- couplr:::trace_push_relabel(cost)
  expect_identical(tr$meta$algorithm, "push_relabel")
  expect_gt(length(tr$frames), 2L)

  final <- tr$frames[[length(tr$frames)]]
  expect_identical(final$phase, "final")
  expect_equal(final$matching, assignment(cost, method = "jv")$match)
  expect_equal(tr$meta$total_cost, jv_total(cost))
})

test_that("every scaling phase reports a smaller eps than the one before", {
  set.seed(517)
  cost <- matrix(sample(0:100, 25, replace = TRUE), 5, 5)

  run <- couplr:::lap_flow_trace_push_relabel(cost)
  eps <- vapply(run$phases, function(p) p$eps, numeric(1))

  expect_gt(length(eps), 1L)
  expect_true(all(diff(eps) < 0))
  expect_lte(eps[length(eps)], 1 / (ncol(cost) + 1))
})

test_that("the phases end on the matching the solver returns", {
  set.seed(519)
  cost <- matrix(sample(0:60, 36, replace = TRUE), 6, 6)

  run <- couplr:::lap_flow_trace_push_relabel(cost)
  last <- run$phases[[length(run$phases)]]

  expect_equal(last$match, run$match)
  expect_false(anyNA(run$match))
})

test_that("every phase leaves its flow eps-optimal", {
  # eps-optimality is a statement about the residual arcs. A pair carrying flow
  # is residual only backwards, at reduced cost -rc, so rc <= eps; a pair
  # carrying none is residual forwards, so rc >= -eps. Both bounds tighten to
  # zero as the phases divide eps, and at the last phase they are the
  # complementary slackness the optimum satisfies.
  set.seed(521)
  cost <- matrix(sample(0:40, 25, replace = TRUE), 5, 5)
  n <- nrow(cost); m <- ncol(cost)

  run <- couplr:::lap_flow_trace_push_relabel(cost)
  expect_gt(length(run$phases), 1L)

  for (ph in run$phases) {
    rc <- cost - outer(ph$dual_u, rep(1, m)) - outer(rep(1, n), ph$dual_v)
    carried <- matrix(FALSE, n, m)
    idx <- which(!is.na(ph$match))
    if (length(idx) > 0L) carried[cbind(idx, ph$match[idx])] <- TRUE

    expect_true(all(rc[carried] <= ph$eps + 1e-9),
                info = paste("carried, eps =", ph$eps))
    expect_true(all(rc[!carried] >= -ph$eps - 1e-9),
                info = paste("free, eps =", ph$eps))
  }
})

test_that("the last phase runs at the eps the integer bound needs", {
  # The scaling stops where n * eps is under the 1 that separates two distinct
  # integer totals, which is the whole reason the last phase's flow is optimal
  # and not merely close.
  set.seed(523)
  cost <- matrix(sample(0:40, 25, replace = TRUE), 5, 5)
  n <- nrow(cost)

  run <- couplr:::lap_flow_trace_push_relabel(cost)
  eps <- run$phases[[length(run$phases)]]$eps

  expect_lt(n * eps, 1)
  expect_equal(sum(cost[cbind(seq_len(n), run$match)]), jv_total(cost))
})
