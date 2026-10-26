test_that("assignment() works on a 3x3 example", {
  M <- matrix(c(4,1,3,
                2,0,5,
                3,2,2), 3, byrow = TRUE)
  out <- assignment(M, maximize = FALSE, method = "bruteforce")
  expect_equal(out$total_cost, 5)
  expect_equal(out$match, c(2L,1L,3L))
})

test_that("assignment() respects NA as forbidden", {
  M <- matrix(c(1, NA,
                NA, 1), nrow = 2, byrow = TRUE)
  out <- assignment(M, maximize = FALSE, method = "bruteforce")
  expect_equal(out$total_cost, 2)
  expect_equal(out$match, c(1L,2L))
})

test_that("maximize works", {
  M <- matrix(c(4,1,3,
                2,0,5,
                3,2,2), 3, byrow = TRUE)
  out <- assignment(M, maximize = TRUE, method = "bruteforce")
  # best sum is 4 + 5 + 2 = 11 via cols (1,3,2)
  expect_equal(out$total_cost, 11)
  expect_equal(out$match, c(1L,3L,2L))
})
