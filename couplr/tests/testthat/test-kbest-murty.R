test_that("kbest: k=1 equals single best", {
  set.seed(1)
  for (n in 2:6) {
    M <- matrix(runif(n*n), n)
    a1 <- assignment(M, method = "jv")
    kb <- kbest_assignment(M, k = 1, method = "murty")
    expect_equal(kb$cost_total[1], a1$total_cost, tolerance = 1e-10)
    # reconstruct row->col from kb row 1
    col_by_row <- kb$col[kb$rank == 1][order(kb$row[kb$rank == 1])]
    expect_equal(col_by_row, a1$match)
  }
})

test_that("kbest: totals are non-decreasing by rank", {
  set.seed(2)
  n <- 4
  M <- matrix(runif(n*n), n)
  kb <- kbest_assignment(M, k = 5)
  by_rank <- tapply(kb$cost_total, kb$rank, function(v) unique(v))
  by_rank <- as.numeric(by_rank)
  expect_true(all(diff(by_rank) >= -1e-12))
})

test_that("kbest: handles NA, rectangular, and maximize", {
  set.seed(3)
  M <- matrix(runif(4*6), 4, 6)
  M[sample.int(length(M), 6)] <- NA
  # ensure feasibility
  for (i in 1:4) if (all(is.na(M[i,]))) M[i, sample.int(6,1)] <- runif(1)

  kb1 <- kbest_assignment(M, k = 3, maximize = FALSE)
  kb2 <- kbest_assignment(M, k = 3, maximize = TRUE)
  expect_equal(nrow(kb1), 3*4)
  expect_equal(nrow(kb2), 3*4)
  # k=1 should match assignment()
  a1 <- assignment(M, method = "jv")
  first_cols <- kb1$col[kb1$rank==1][order(kb1$row[kb1$rank==1])]
  expect_equal(first_cols, a1$match)
})
