# The row cover as a primal feasibility condition.
#
# The primal constrains every row of the short side to hold exactly one pair.
# A matching that leaves a row free is a valid partial matching and not a
# feasible solution, so no conclusion may rest on it in any arithmetic. These
# tests fix that across the three `arithmetic` values, both cost sources and
# both shapes, because the condition is a property of the model rather than of
# the arithmetic the conditions are decided in.

test_that("an unmatched row blocks the conclusion in every arithmetic", {
  # Uniform costs make every dual vector of zeros feasible and every objective
  # zero, so the duality gap cannot see the uncovered rows and the row cover is
  # the only condition standing between this input and a certificate.
  cost <- matrix(1, 2, 2)
  duals <- list(u = c(0, 0), v = c(0, 0))

  for (arith in c("auto", "double", "exact")) {
    cert <- verify_assignment(c(0L, 0L), cost, duals = duals, arithmetic = arith)

    expect_false(cert$certified_optimal, info = arith)
    expect_false(cert$primal_feasible, info = arith)
    expect_false(cert$all_rows_matched, info = arith)
    expect_true(cert$structurally_valid_matching, info = arith)
    expect_equal(cert$duality_gap, 0, info = arith)
  }
})

test_that("one unmatched row of many blocks the conclusion in every arithmetic", {
  set.seed(23)
  cost <- matrix(sample(1:200, 64, replace = TRUE), 8, 8)
  duals <- assignment_duals(cost)
  dropped <- duals$match
  dropped[3] <- 0L

  for (arith in c("auto", "double", "exact")) {
    cert <- verify_assignment(dropped, cost,
                              duals = list(u = duals$u, v = duals$v),
                              arithmetic = arith)

    expect_false(cert$certified_optimal, info = arith)
    expect_false(cert$all_rows_matched, info = arith)
    expect_equal(cert$n_matched, 7L, info = arith)
  }
})

test_that("the row cover is required on rectangular and transposed problems", {
  set.seed(29)
  # Wide: 4 rows, 9 columns. The equality belongs to the short side, so the
  # cover is over rows. Tall: the same problem transposed, which the verifier
  # transposes back internally, so the condition has to survive that.
  wide <- matrix(stats::runif(36), 4, 9)
  tall <- t(wide)

  for (cost in list(wide, tall)) {
    duals <- assignment_duals(cost)
    dropped <- duals$match
    dropped[1] <- 0L

    for (arith in c("auto", "double", "exact")) {
      cert <- verify_assignment(dropped, cost,
                                duals = list(u = duals$u, v = duals$v),
                                arithmetic = arith)
      expect_false(cert$certified_optimal, info = arith)
      expect_false(cert$all_rows_matched, info = arith)
    }
  }
})

test_that("every row unmatched blocks the conclusion at any cost sign", {
  # Zero and negative costs are where an all-zero dual vector is feasible and
  # the two objectives coincide most easily.
  set.seed(31)
  cases <- list(
    positive = matrix(stats::runif(16, 1, 5), 4, 4),
    zero     = matrix(0, 4, 4),
    negative = matrix(stats::runif(16, -5, -1), 4, 4)
  )

  for (nm in names(cases)) {
    cost <- cases[[nm]]
    duals <- list(u = rep(0, 4), v = rep(0, 4))

    for (arith in c("auto", "double", "exact")) {
      cert <- verify_assignment(rep(0L, 4), cost, duals = duals,
                                arithmetic = arith)
      expect_false(cert$certified_optimal, info = paste(nm, arith))
      expect_equal(cert$n_matched, 0L, info = paste(nm, arith))
    }
  }
})

test_that("a complete matching still certifies in every arithmetic", {
  # The guard against a fix that refuses everything.
  set.seed(37)
  cost <- matrix(sample(1:200, 100, replace = TRUE), 10, 10)
  m <- assignment(cost)

  for (arith in c("auto", "double", "exact")) {
    cert <- verify_assignment(m, cost, arithmetic = arith)
    expect_true(cert$certified_optimal, info = arith)
    expect_true(cert$primal_feasible, info = arith)
    expect_true(cert$all_rows_matched, info = arith)
  }
})

test_that("the partial matching's objective is still reported", {
  # primal_feasible is FALSE but the cost of the pairs that were made is a
  # meaningful number, and the report keeps it for diagnosis.
  set.seed(41)
  cost <- matrix(sample(1:50, 36, replace = TRUE), 6, 6)
  duals <- assignment_duals(cost)
  dropped <- duals$match
  dropped[2] <- 0L

  cert <- verify_assignment(dropped, cost,
                            duals = list(u = duals$u, v = duals$v))

  expect_false(cert$primal_feasible)
  expect_true(cert$structurally_valid_matching)
  expect_false(is.na(cert$primal_objective))
  kept <- which(dropped > 0L)
  expect_equal(cert$primal_objective,
               sum(cost[cbind(kept, dropped[kept])]))
})

test_that("a structurally invalid matching reports no objective", {
  # The other side of the split: a duplicated column is not a matching at all,
  # so the sum would correspond to nothing and is withheld.
  cost <- matrix(1, 3, 3)
  cert <- verify_assignment(c(1L, 1L, 3L), cost,
                            duals = list(u = rep(0, 3), v = rep(0, 3)))

  expect_false(cert$structurally_valid_matching)
  expect_false(cert$primal_feasible)
  expect_true(is.na(cert$primal_objective))
})

test_that("the row cover is required on a lazy cost source", {
  # The implicit and lazy paths reach the same certificate through a different
  # source, so the condition is checked there too.
  skip_if_not_installed("couplr")
  set.seed(43)
  n_left <- 12L
  n_right <- 24L
  left <- data.frame(id = paste0("L", seq_len(n_left)),
                     x = stats::runif(n_left), y = stats::runif(n_left))
  right <- data.frame(id = paste0("R", seq_len(n_right)),
                      x = stats::runif(n_right), y = stats::runif(n_right))

  m <- match_couples(left, right, vars = c("x", "y"), distance = "euclidean",
                     left_id = "id", right_id = "id", memory_mode = "implicit")

  expect_true(m$certificate$certified_optimal)
  expect_true(m$certificate$primal_feasible)
  expect_true(m$certificate$all_rows_matched)
})
