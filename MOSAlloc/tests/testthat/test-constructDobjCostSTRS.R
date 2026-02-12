# Filename: test-constructDobjCost.R
# Date: 31.12.2025
# Author: Felix Willems

# function: constructDobjCostSTRS()
test_that("constructDobjCostSTRS() works as expected)", {

  X <- matrix(c(1, 1, 1, 1,
                4, 5, 6, 7), 4, 2)
  list <- list(list(stratum_id = 1:4, c_type = 1, name = "Population"),
               list(stratum_id = 1:4, c_type = 2, name = "Population"))

  # rows correspond to strata
  expect_error(constructDobjCostSTRS(t(X), NULL, list), "Stratum out of range!")

  # constructDobjCostSTRS() returns a list
  expect_identical(is.list(constructDobjCostSTRS(X, NULL, list)), TRUE)

  list <- list(list(stratum_id = 1:4, c_type = "ssize", name = "Population"),
               list(stratum_id = 1:4, c_type = "$ US", name = "Population"))
  # cost types not specified in X
  expect_error(constructDobjCostSTRS(X, NULL, list))

  colnames(X) <- c("ssize", "$ US")
  Dd <- constructDobjCostSTRS(X, NULL, list)
  expect_identical(rownames(Dd$D), names(Dd$d))
  expect_equal(unname(Dd$D[1, ]), rep(1, 4))
  expect_equal(unname(Dd$D[2, ]), 4:7)
  expect_equal(unname(Dd$d), c(0, 0))

  # with fixed cost matrix
  Dd <- constructDobjCostSTRS(X, X, list)
  val <- colSums(X)
  names(val) <- paste0(names(val), "_Population")
  expect_equal(Dd$d, val)
})