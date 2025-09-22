test_that("rsd works", {
  vals <- c(5244.86, 4497.89, 5230.10) # Blanks 581
  expected <- 0.08556813

  result <- rsd(vals)
  expect_equal(result, expected)
})
