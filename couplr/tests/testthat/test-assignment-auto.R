test_that("auto matches JV on small squares", {
  set.seed(2)
  for (n in 2:10) {
    M <- matrix(runif(n*n), n)
    a <- assignment(M, method = "auto")
    j <- assignment(M, method = "jv")
    expect_equal(a$total_cost, j$total_cost, tolerance = 1e-10)

    a <- assignment(M, maximize = TRUE, method = "auto")
    j <- assignment(M, maximize = TRUE, method = "jv")
    expect_equal(a$total_cost, j$total_cost, tolerance = 1e-10)
  }
})

test_that("auto handles very rectangular + many NA like SSP", {
  set.seed(3)
  n <- 40; m <- 120
  M <- matrix(runif(n*m), n, m)
  M[sample.int(length(M), size = floor(0.40 * length(M)))] <- NA
  # ensure feasibility
  for (i in 1:n) if (all(is.na(M[i, ]))) M[i, sample.int(m, 1)] <- runif(1)

  a <- assignment(M, method = "auto")
  s <- assignment(M, method = "sap")
  expect_equal(a$total_cost, s$total_cost, tolerance = 1e-8)

  a <- assignment(M, maximize = TRUE, method = "auto")
  s <- assignment(M, maximize = TRUE, method = "sap")
  expect_equal(a$total_cost, s$total_cost, tolerance = 1e-8)
})

test_that("auto is consistent on large dense", {
  set.seed(4)
  n <- 120
  M <- matrix(runif(n*n), n)
  a <- assignment(M, method = "auto")
  j <- assignment(M, method = "jv")
  expect_equal(a$total_cost, j$total_cost, tolerance = 1e-8)

  a <- assignment(M, maximize = TRUE, method = "auto")
  j <- assignment(M, maximize = TRUE, method = "jv")
  expect_equal(a$total_cost, j$total_cost, tolerance = 1e-8)
})
