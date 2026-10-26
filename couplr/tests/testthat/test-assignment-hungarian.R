test_that("hungarian == jv on small squares", {
  set.seed(1)
  for (n in 2:7) {
    M <- matrix(runif(n*n), n)
    a <- assignment(M, method = "jv")
    h <- assignment(M, method = "hungarian")
    expect_equal(a$total_cost, h$total_cost, tolerance = 1e-10)
  }
})

test_that("hungarian handles rectangles and NA", {
  set.seed(2)
  n <- 5; m <- 9
  M <- matrix(runif(n*m), n, m)
  M[sample.int(length(M), size = 8)] <- NA
  for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)

  h <- assignment(M, method = "hungarian")
  j <- assignment(M, method = "jv")
  expect_equal(h$total_cost, j$total_cost, tolerance = 1e-8)
  expect_equal(length(unique(h$match)), n)
  expect_true(all(h$match >= 1L & h$match <= m))
})

test_that("hungarian errors when a row is entirely forbidden", {
  M <- matrix(1, 3, 5)
  M[1,] <- NA
  expect_error(assignment(M, method = "hungarian"), "Infeasible")
})

test_that("maximize works for hungarian", {
  set.seed(3)
  n <- 6
  M <- matrix(runif(n*n), n)
  a <- assignment(M, maximize = TRUE,  method = "hungarian")
  b <- assignment(-M, maximize = FALSE, method = "hungarian")
  expect_equal(a$total_cost, -b$total_cost, tolerance = 1e-10)
})
