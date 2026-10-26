# test-gabow_tarjan_solver.R
# High-level tests for lap_solve_gabow_tarjan()

library(testthat)

# check_complementary_slackness() and assignment_cost() are in helper-gabow_tarjan.R

# Helper: build col_match from row_match
build_col_match <- function(row_match, m) {
  col_match <- rep(NA_integer_, m)
  for (i in seq_along(row_match)) {
    j <- row_match[i]
    if (!is.na(j) && j >= 1 && j <= m) {
      col_match[j] <- i
    }
  }
  col_match
}

test_that("Gabow-Tarjan solves simple 3x3 matrix", {
  cost <- matrix(c(
    4, 1, 3,
    2, 0, 5,
    3, 2, 2
  ), nrow = 3, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, 5)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles identity-like matrix", {
  cost <- matrix(c(
    1,   100, 100,
    100,   1, 100,
    100, 100,   1
  ), nrow = 3, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, 3)
  expect_equal(res$match, c(1L, 2L, 3L))

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles maximization", {
  # Use a profit matrix where min and max assignments differ
  profit <- matrix(c(
    10,  5,  3,
     7, 12,  4,
     6,  8, 15
  ), nrow = 3, byrow = TRUE)

  # Brute-force reference min / max (no dependency on other solvers)
  perms <- list(
    c(1L, 2L, 3L),
    c(1L, 3L, 2L),
    c(2L, 1L, 3L),
    c(2L, 3L, 1L),
    c(3L, 1L, 2L),
    c(3L, 2L, 1L)
  )

  costs <- vapply(
    perms,
    function(p) sum(profit[cbind(1:3, p)]),
    numeric(1)
  )

  ref_min_cost <- min(costs)
  ref_max_cost <- max(costs)

  # Sanity check: this matrix actually has different min/max
  expect_true(ref_max_cost > ref_min_cost)

  # Run Gabow–Tarjan in min and max modes
  res_gt_min <- lap_solve_gabow_tarjan(profit, maximize = FALSE)
  res_gt_max <- lap_solve_gabow_tarjan(profit, maximize = TRUE)

  # All rows should be matched
  expect_equal(res_gt_min$n_matched, 3)
  expect_equal(res_gt_max$n_matched, 3)

  gt_min_cost <- res_gt_min$total_cost
  gt_max_cost <- res_gt_max$total_cost

  # Costs must match the brute-force optimum for each objective
  expect_equal(gt_min_cost, ref_min_cost)
  expect_equal(gt_max_cost, ref_max_cost)

  # Maximization must not be worse than minimization on a profit matrix
  expect_true(gt_max_cost > gt_min_cost)

  # Optional: if you want complementary slackness here too, use row/col matches
  # and duals from the new interface. Comment out if you prefer not to check.
  #
  # build col_match from row_match for the CS check
  col_match_min <- rep(NA_integer_, ncol(profit))
  for (i in seq_len(nrow(profit))) {
    j <- res_gt_min$row_match[i]
    if (!is.na(j) && j >= 1 && j <= ncol(profit)) {
      col_match_min[j] <- i
    }
  }
  col_match_max <- rep(NA_integer_, ncol(profit))
  for (i in seq_len(nrow(profit))) {
    j <- res_gt_max$row_match[i]
    if (!is.na(j) && j >= 1 && j <= ncol(profit)) {
      col_match_max[j] <- i
    }
  }

  expect_true(check_complementary_slackness(
    profit,
    res_gt_min$row_match,
    col_match_min,
    res_gt_min$u,
    res_gt_min$v
  ))
})




test_that("Gabow-Tarjan handles 4x4 matrix", {
  cost <- matrix(c(
    10, 19,  8, 15,
    10, 18,  7, 17,
    13, 16,  9, 14,
    12, 19,  8, 18
  ), nrow = 4, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 4)
  expect_true(res$total_cost > 0)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles 5x5 matrix", {
  cost <- matrix(c(
    7, 2, 1, 9, 4,
    9, 6, 9, 5, 5,
    3, 8, 3, 1, 8,
    7, 9, 4, 2, 2,
    8, 4, 7, 4, 8
  ), nrow = 5, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 5)
  expect_true(res$total_cost > 0)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles negative costs", {
  cost <- matrix(c(
    -1, 5, 3,
     4, -2, 6,
     2, 1, -3
  ), nrow = 3, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, -6)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles zero costs", {
  cost <- matrix(0, nrow = 3, ncol = 3)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, 0)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles uniform costs", {
  cost <- matrix(5, nrow = 3, ncol = 3)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, 15)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles large cost differences", {
  cost <- matrix(c(
      1, 1000, 1000,
   1000,    1, 1000,
   1000, 1000,    1
  ), nrow = 3, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, 3)
  expect_equal(res$match, c(1L, 2L, 3L))

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles rectangular matrices (4x3)", {
  cost <- matrix(c(
     1,  2,  3,
     4,  5,  6,
     7,  8,  9,
    10, 11, 12
  ), nrow = 4, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  # Three columns can hold three rows, so one row comes back unmatched. This
  # used to report four, the fourth pointing at a padding column the caller's
  # matrix does not have.
  expect_equal(res$n_matched, 3)
  matched <- res$match[!is.na(res$match)]
  expect_true(all(matched >= 1L & matched <= ncol(cost)))
  expect_equal(length(unique(matched)), length(matched))
  expect_equal(res$total_cost, assignment(cost, method = "jv")$total_cost)
})

test_that("Gabow-Tarjan is optimal on wide problems", {
  # The 1-optimality bound holds for a matching that saturates both sides, so
  # a rectangular instance has to be padded to square. It was not, and 179 of
  # 200 random wide problems came back worse than jv.
  cost <- matrix(c(
    14,  9, 17,  3,  4, 12,
    14,  5, 14,  3, 12, 11,
    12, 13,  1, 19,  4, 17
  ), nrow = 3, byrow = TRUE)
  storage.mode(cost) <- "double"

  res <- assignment(cost, method = "gabow_tarjan")
  expect_equal(res$total_cost, 8)
  expect_true(verify_assignment(res, cost)$certified_optimal)

  skip_on_cran()
  set.seed(31)
  for (i in seq_len(40)) {
    n <- sample(3:7, 1)
    m <- n + sample(1:8, 1)
    cst <- matrix(round(stats::runif(n * m, 1, 100), 4), n, m)
    gt <- assignment(cst, method = "gabow_tarjan")
    expect_equal(gt$total_cost, assignment(cst, method = "jv")$total_cost,
                 info = paste0(n, " x ", m, ", seed 31 draw ", i))
  }
})

test_that("Gabow-Tarjan solves a wide problem at its own shape", {
  # The dummy side of the padded square is one node repeated, so it is carried
  # as a single row of that capacity and the instance stays n + 1 rows tall.
  # At 10 x 400 the square would be 400 x 400.
  set.seed(19)
  n <- 10
  m <- 400
  cst <- matrix(round(stats::runif(n * m, 1, 1000)), n, m)
  storage.mode(cst) <- "double"

  gt <- assignment(cst, method = "gabow_tarjan")
  expect_equal(gt$total_cost, assignment(cst, method = "jv")$total_cost)
  expect_equal(sum(gt$match > 0), n)
  expect_equal(length(unique(gt$match)), n)
  expect_true(verify_assignment(gt, cst)$certified_optimal)
})

test_that("Gabow-Tarjan is optimal on wide problems with forbidden edges", {
  skip_on_cran()

  set.seed(77)
  for (i in seq_len(30)) {
    n <- sample(2:6, 1)
    m <- n + sample(1:15, 1)
    cst <- matrix(round(stats::runif(n * m, 1, 100), 3), n, m)
    # A feasible skeleton keeps every row reachable; the rest may be forbidden.
    keep <- cbind(seq_len(n), sample(seq_len(m), n))
    blocked <- matrix(stats::runif(n * m) < 0.4, n, m)
    blocked[keep] <- FALSE
    cst[blocked] <- Inf

    maximize <- i %% 3 == 0
    gt <- assignment(cst, method = "gabow_tarjan", maximize = maximize)
    jv <- assignment(cst, method = "jv", maximize = maximize)
    expect_equal(gt$total_cost, jv$total_cost,
                 info = paste0(n, " x ", m, ", seed 77 draw ", i))
    expect_true(all(is.finite(cst[cbind(seq_len(n), gt$match)])))
  }
})

test_that("Gabow-Tarjan is optimal on tall problems", {
  # A tall problem is the same problem read the other way round, so it is
  # transposed onto the wide path: the m columns are the side that must be
  # saturated and n - m rows come back unmatched.
  skip_on_cran()

  set.seed(53)
  for (i in seq_len(30)) {
    m <- sample(2:6, 1)
    n <- m + sample(1:12, 1)
    cst <- matrix(round(stats::runif(n * m, 1, 100), 3), n, m)

    gt <- assignment(cst, method = "gabow_tarjan")
    jv <- assignment(cst, method = "jv")
    expect_equal(gt$total_cost, jv$total_cost,
                 info = paste0(n, " x ", m, ", seed 53 draw ", i))
    matched <- gt$match[gt$match > 0]
    expect_equal(length(matched), m)
    expect_equal(length(unique(matched)), m)
  }
})

test_that("Gabow-Tarjan handles a single row against many columns", {
  set.seed(11)
  cst <- matrix(round(stats::runif(500, 1, 1000)), nrow = 1)
  storage.mode(cst) <- "double"

  gt <- assignment(cst, method = "gabow_tarjan")
  expect_equal(gt$total_cost, min(cst))
  expect_equal(gt$match, which.min(cst))
})

test_that("Gabow-Tarjan matches Hungarian on small matrices", {
  set.seed(42)
  cost <- matrix(sample(1:20, 9, replace = TRUE), nrow = 3)

  res_gt <- lap_solve_gabow_tarjan(cost, maximize = FALSE)
  res_h  <- couplr:::lap_solve_hungarian(cost, maximize = FALSE)

  cost_h <- if (!is.null(res_h$cost)) {
    res_h$cost
  } else if (!is.null(res_h$total_cost)) {
    res_h$total_cost
  } else if (!is.null(res_h$assignment)) {
    assignment_cost(cost, res_h$assignment)
  } else {
    NA_real_
  }

  expect_false(is.na(cost_h))
  expect_equal(res_gt$total_cost, cost_h, tolerance = 1e-6)

  col_match <- build_col_match(res_gt$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res_gt$match, col_match, res_gt$row_duals, res_gt$col_duals
  ))
})

test_that("Gabow-Tarjan matches JV on larger matrices", {
  set.seed(123)
  n <- 10
  cost <- matrix(sample(1:100, n * n, replace = TRUE), nrow = n)

  res_gt <- lap_solve_gabow_tarjan(cost, maximize = FALSE)
  res_jv <- couplr:::lap_solve_jv(cost, maximize = FALSE)

  cost_jv <- if (!is.null(res_jv$cost)) {
    res_jv$cost
  } else if (!is.null(res_jv$total_cost)) {
    res_jv$total_cost
  } else if (!is.null(res_jv$assignment)) {
    assignment_cost(cost, res_jv$assignment)
  } else {
    NA_real_
  }

  expect_false(is.na(cost_jv))
  expect_equal(res_gt$total_cost, cost_jv, tolerance = 1e-6)

  col_match <- build_col_match(res_gt$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res_gt$match, col_match, res_gt$row_duals, res_gt$col_duals
  ))
})

# The fixed-point conversion rounds the costs to a quantum of span * K / 1e13
# in the caller's units, K being the multiplier the bit-scaling uses, and an
# optimal matching pays min(n, m) of them. Every accuracy claim below is the
# distance to jv against that bound.
gt_within_quantization <- function(cst, maximize = FALSE, info = NULL) {
  gt <- assignment(cst, method = "gabow_tarjan", maximize = maximize)
  jv <- assignment(cst, method = "jv", maximize = maximize)
  n <- nrow(cst)
  m <- ncol(cst)
  k <- min(n, m)
  mult <- if (n == m) n + 1 else 2 * k + 1
  span <- max(cst, na.rm = TRUE) - min(cst, na.rm = TRUE)
  bound <- k * mult * span / 1e13 + 1e-9 * abs(jv$total_cost)
  expect_lt(abs(gt$total_cost - jv$total_cost), bound, label = info)
  invisible(gt)
}

test_that("Gabow-Tarjan is optimal when one entry dwarfs the rest", {
  # The quantum the conversion rounds to is the span of the costs over the
  # integer range available to it. A single large entry stretches the span
  # while the pairs an optimal matching uses stay small, so the quantum has to
  # be read off the range for those pairs to survive the conversion.
  set.seed(20)
  for (n in c(20L, 40L)) {
    cst <- matrix(stats::runif(n * n), n, n)
    cst[1, 1] <- 1e5
    gt <- gt_within_quantization(cst, info = paste0(n, " x ", n, ", 1e5 entry"))
    expect_true(verify_assignment(gt, cst)$certified_optimal)
  }

  # The same spread on a wide instance, on a maximization, and over costs that
  # run across twelve orders of magnitude.
  set.seed(21)
  wide <- matrix(stats::runif(6 * 25), 6, 25)
  wide[2, 7] <- 1e5
  gt_within_quantization(wide, info = "6 x 25, 1e5 entry")

  mx <- matrix(stats::runif(400), 20, 20)
  mx[3, 4] <- 1e5
  gt_within_quantization(mx, maximize = TRUE, info = "20 x 20 maximized")

  gt_within_quantization(matrix(10^stats::runif(400, -6, 6), 20, 20),
                         info = "20 x 20 over twelve orders")
})

test_that("Gabow-Tarjan is optimal when the costs sit far from zero", {
  # Costs offset from the origin carry their structure in the part above the
  # offset, which is what the conversion has to resolve. Scaling by the span
  # rather than by the magnitude leaves that part intact: on a span of one the
  # quantum is 2e-12 and the whole matching is exact in double.
  set.seed(22)
  off <- matrix(stats::runif(400, 2e6, 2e6 + 1), 20, 20)
  expect_equal(assignment(off, method = "gabow_tarjan")$total_cost,
               assignment(off, method = "jv")$total_cost, tolerance = 1e-12)

  neg <- -off
  expect_equal(assignment(neg, method = "gabow_tarjan")$total_cost,
               assignment(neg, method = "jv")$total_cost, tolerance = 1e-12)
  expect_equal(assignment(off, method = "gabow_tarjan", maximize = TRUE)$total_cost,
               assignment(off, method = "jv", maximize = TRUE)$total_cost,
               tolerance = 1e-12)

  # A forbidden pair carries the sentinel rather than a cost, so it stays out
  # of the span the scale is built from.
  sparse <- matrix(stats::runif(300, 1e9, 1e9 + 1), 10, 30)
  sparse[cbind(1:10, 21:30)] <- NA_real_
  expect_equal(assignment(sparse, method = "gabow_tarjan")$total_cost,
               assignment(sparse, method = "jv")$total_cost, tolerance = 1e-12)
})

test_that("Gabow-Tarjan keeps integer costs exact far from the origin", {
  # Integer costs go in as they are, so what the bit-scaling has to hold is
  # their range and not their distance from zero.
  set.seed(23)
  cst <- matrix(1e14 + sample.int(100L, 100L, replace = TRUE), 10, 10)
  storage.mode(cst) <- "double"
  expect_true(assignment(cst, method = "gabow_tarjan")$total_cost ==
              assignment(cst, method = "jv")$total_cost)

  big <- matrix(2e15 + sample.int(100L, 100L, replace = TRUE), 10, 10)
  storage.mode(big) <- "double"
  expect_true(assignment(big, method = "gabow_tarjan")$total_cost ==
              assignment(big, method = "jv")$total_cost)
})

test_that("Gabow-Tarjan refuses an integer cost range it cannot represent", {
  # Past the range the scaled costs reach the value the solver reads as a
  # forbidden pair, which would come back as a wrong answer reported optimal.
  set.seed(24)
  cst <- matrix(round(stats::runif(100, 0, 1e14)), 10, 10)
  storage.mode(cst) <- "double"
  expect_error(assignment(cst, method = "gabow_tarjan"), "method = 'jv'")

  # Inside the range it is solved, and exactly.
  ok <- matrix(round(stats::runif(100, 0, 1e13)), 10, 10)
  storage.mode(ok) <- "double"
  expect_true(assignment(ok, method = "gabow_tarjan")$total_cost ==
              assignment(ok, method = "jv")$total_cost)
})

test_that("Gabow-Tarjan holds its accuracy across dynamic ranges", {
  skip_on_cran()

  # Spans built from outliers, offsets and exponents, over both objectives and
  # all three shapes, so the distance to jv is measured where the quantum is
  # largest relative to the pairs an optimal matching uses.
  set.seed(25)
  for (i in seq_len(30)) {
    n <- sample(4:12, 1)
    m <- if (i %% 3 == 0) n else n + sample(1:8, 1)
    if (i %% 5 == 0) { swap <- n; n <- m; m <- swap }
    cst <- switch(
      1 + (i %% 4),
      matrix(stats::runif(n * m), n, m),
      { x <- matrix(stats::runif(n * m), n, m); x[1, 1] <- 1e5; x },
      matrix(stats::runif(n * m, 1e6, 1e6 + 1), n, m),
      matrix(10^stats::runif(n * m, -6, 6), n, m)
    )
    gt_within_quantization(cst, maximize = i %% 2 == 0,
                           info = paste0("seed 25 draw ", i))
  }
})
