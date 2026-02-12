# Filename: test-constructArestrSTRS.R
# Date: 31.12.2025
# Author: Felix Willems

# function: constructArestr
test_that("constructArestrSTRS() works as expected 'fpc = TRUE')", {
  X <- matrix(c(1, 2, 3,
                4, 5, 6,
                7, 8, 9), 3, 3, byrow = TRUE)

  list <- list(list(stratum_id = 1, variate = 1, measure = "RSE",
                    bound = 0.05, name = "D1"),
               list(stratum_id = 2:3, variate = 1, measure = "RSE",
                    bound = 0.06, name = "D2"),
               list(stratum_id = 1:3, variate = 2, measure = "RSE",
                    bound = 0.07, name = "D3"),
               list(stratum_id = 1:3, variate = 3, measure = "RSE",
                    bound = 0.08, name = "D4"))
  Aa <- constructArestrSTRS(X, X, X[, 1], list, fpc = TRUE)

  # precision components
  expect_equal(Aa$A[1, 1], 1**2 * 1 / 1**2)
  expect_equal(unname(Aa$A[1, -1]), c(0, 0))
  expect_equal(Aa$A[2, 1], 0)
  expect_equal(unname(Aa$A[2, -1]), c(4**3, 7**3) / (4 + 7)**2)
  expect_equal(unname(Aa$A[3, ]), c(1**2 * 2, 4**2 * 5, 7**2 * 8) / 15**2)
  expect_equal(unname(Aa$A[4, ]), c(1**2 * 3, 4**2 * 6, 7**2 * 9) / 18**2)

  # fpc + bound
  expect_equal(Aa$a[[1]], 0.05**2 + 1 * 1 / 1**2)
  expect_equal(Aa$a[[2]], 0.06**2 + (4 * 4 + 7 * 7) / (4 + 7)**2)
  expect_equal(Aa$a[[3]], 0.07**2 + (1 * 2 + 4 * 5 + 7 * 8) / 15**2)
  expect_equal(Aa$a[[4]], 0.08**2 + (1 * 3 + 4 * 6 + 7 * 9) / 18**2)
})

test_that("constructArestrSTRS() works as expected 'fpc = FALSE')", {
  X <- matrix(c(1, 2, 3,
                4, 5, 6,
                7, 8, 9), 3, 3, byrow = TRUE)

  list <- list(list(stratum_id = 1, variate = 1, measure = "RSE",
                    bound = 0.05, name = "D1"),
               list(stratum_id = 2:3, variate = 1, measure = "RSE",
                    bound = 0.06, name = "D2"),
               list(stratum_id = 1:3, variate = 2, measure = "RSE",
                    bound = 0.07, name = "D3"),
               list(stratum_id = 1:3, variate = 3, measure = "RSE",
                    bound = 0.08, name = "D4"))
  Aa <- constructArestrSTRS(X, X, X[, 1], list, fpc = FALSE)

  # precision components
  expect_equal(Aa$A[1, 1], 1**2 * 1 / 1**2)
  expect_equal(unname(Aa$A[1, -1]), c(0, 0))
  expect_equal(Aa$A[2, 1], 0)
  expect_equal(unname(Aa$A[2, -1]), c(4**3, 7**3) / (4 + 7)**2)
  expect_equal(unname(Aa$A[3, ]), c(1**2 * 2, 4**2 * 5, 7**2 * 8) / 15**2)
  expect_equal(unname(Aa$A[4, ]), c(1**2 * 3, 4**2 * 6, 7**2 * 9) / 18**2)

  # fpc + bound
  expect_equal(Aa$a[[1]], 0.05**2)
  expect_equal(Aa$a[[2]], 0.06**2)
  expect_equal(Aa$a[[3]], 0.07**2)
  expect_equal(Aa$a[[4]], 0.08**2)
})