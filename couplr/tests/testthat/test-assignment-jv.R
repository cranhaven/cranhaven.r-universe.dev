test_that("jv handles rectangular matrices (n < m)", {
  set.seed(42)
  for (n in 2:6) {
    m <- n + 2
    M <- matrix(runif(n*m), n, m)
    a <- assignment(M, method = "jv")
    b <- assignment(M, method = "bruteforce")
    expect_equal(a$total_cost, b$total_cost, tolerance = 1e-10)
    # match length & range
    expect_length(a$match, n)
    expect_true(all(a$match >= 1L & a$match <= m))
    # uniqueness of assigned columns
    expect_equal(length(unique(a$match)), n)
  }
})

test_that("jv copes with many NAs but remains feasible", {
  set.seed(7)
  n <- 5; m <- 8
  M <- matrix(runif(n*m), n, m)
  M[sample.int(length(M), size = 15)] <- NA
  # Ensure at least one allowed per row
  for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)
  a <- assignment(M, method = "jv")
  b <- assignment(M, method = "bruteforce")
  expect_equal(a$total_cost, b$total_cost, tolerance = 1e-10)
})

test_that("jv errors when a row is entirely forbidden", {
  M <- matrix(c(1, 2, 3, 4), 2, byrow = TRUE)
  M[1, ] <- NA
  expect_error(assignment(M, method = "jv"), "Infeasible")
})

test_that("NA and +Inf behave as forbidden identically", {
  set.seed(3)
  n <- 4; m <- 6
  M1 <- matrix(runif(n*m), n, m)
  M2 <- M1
  idx <- sample.int(length(M1), size = 6)
  M1[idx] <- NA
  M2[idx] <- Inf
  a1 <- assignment(M1, method = "jv")
  a2 <- assignment(M2, method = "jv")
  expect_equal(a1$total_cost, a2$total_cost, tolerance = 1e-12)
})

test_that("deterministic tie-breaking on equal costs", {
  # All finite and equal -> any perfect matching is optimal; JV impl should pick smallest columns
  n <- 4; m <- 6
  M <- matrix(1, n, m)
  a <- assignment(M, method = "jv")
  expect_equal(a$match, seq_len(n))  # ties broken by first-available columns
  expect_equal(a$total_cost, n * 1)
})

test_that("maximize equals minimize on negated finite costs (same feasibility)", {
  set.seed(9)
  n <- 5; m <- 7
  M <- matrix(runif(n*m), n, m)
  # sprinkle some NA (keep feasibility)
  M[sample.int(length(M), 8)] <- NA
  for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)
  # Build negated version: keep NA as NA
  Mneg <- M
  Mneg[!is.na(Mneg)] <- -Mneg[!is.na(Mneg)]
  a_max <- assignment(M, maximize = TRUE, method = "jv")
  a_min_neg <- assignment(Mneg, maximize = FALSE, method = "jv")
  expect_equal(a_max$total_cost, -a_min_neg$total_cost, tolerance = 1e-10)
})

test_that("scaling and shifting finite costs preserve the argmin", {
  set.seed(11)
  n <- 5; m <- 5
  M <- matrix(runif(n*m), n, m)
  M[sample.int(length(M), 5)] <- NA
  # add row-wise and column-wise constants to finite entries
  row_shift <- runif(n)
  col_shift <- runif(m)
  A <- M
  for (i in 1:n) for (j in 1:m) if (!is.na(A[i,j])) A[i,j] <- A[i,j] + row_shift[i] + col_shift[j]
  a <- assignment(M, method = "jv")
  b <- assignment(A, method = "jv")
  expect_equal(a$match, b$match)
})

test_that("output is valid: indices in range and 1-1 matching", {
  set.seed(13)
  n <- 6; m <- 9
  M <- matrix(runif(n*m), n, m)
  a <- assignment(M, method = "jv")
  expect_true(all(a$match %in% seq_len(m)))
  expect_equal(length(unique(a$match)), n)
})
