simdata <- simulate_data(n = 4000, percentage_censored = 0.7, starting_seed = 10)

test_that("censored parameter is working", {
  expect_equal(sum(simdata$data$delta == 0)/nrow(simdata$data), 0.7,
               tolerance = 1)
})

test_that("number of observations is as expected", {
  expect_equal(4000, nrow(simdata$data))
})
