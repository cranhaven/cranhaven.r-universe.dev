library(ADSIHT); library(testthat);

# Run tests for each function
test_that("function runs correctly", {
  n <- 200
  m <- 100
  d <- 10
  s <- 5
  s0 <- 5
  data <- gen.data(n, m, d, s, s0)

  testthat::expect_no_error(
    ADSIHT(data$x, data$y, data$group,s0)
  )

})

