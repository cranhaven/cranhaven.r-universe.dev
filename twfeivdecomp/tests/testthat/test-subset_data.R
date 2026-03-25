# Tests for subset_data() function

test_that("subset_data filters by cohort and time correctly when exposed > unexposed", {
  data <- data.frame(
    id = rep(1:4, each = 2),
    time = rep(c(2000, 2001), times = 4),
    cohort = c(2001, 2001, 999999, 999999, 2000, 2000, 2000, 2000)
  )

  result <- subset_data(data, exposed_cohort = 2001, unexposed_cohort = 2000)

  expect_true(all(result$cohort %in% c(2001, 2000)))
  expect_true(all(result$time >= 2000))  
})

test_that("subset_data filters by cohort and time correctly when unexposed = 999999", {
  data <- data.frame(
    id = rep(1:4, each = 2),
    time = rep(c(2000, 2001), times = 4),
    cohort = c(2001, 2001, 999999, 999999, 2000, 2000, 2000, 2000)
  )

  result <- subset_data(data, exposed_cohort = 2000, unexposed_cohort = 999999)

  expect_true(all(result$cohort %in% c(2000, 999999)))
  expect_true(all(result$time < 999999))  
})

test_that("subset_data filters by cohort and time correctly when exposed < unexposed", {
  data <- data.frame(
    id = rep(1:4, each = 2),
    time = rep(c(2000, 2001), times = 4),
    cohort = c(2001, 2001, 999999, 999999, 2000, 2000, 2000, 2000)
  )

  result <- subset_data(data, exposed_cohort = 2000, unexposed_cohort = 2001)

  expect_true(all(result$cohort %in% c(2000, 2001)))
  expect_true(all(result$time < 2001))
})

