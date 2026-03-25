# Tests for the calculate_weight_numerator() function

test_that("calculate_weight_numerator works for Exposed vs Unexposed", {
  data <- data.frame(
  id = rep(1:4, each = 6),
  time = rep(2000:2005, times = 4),
  instrument = c(
    0, 0, 0, 1, 1, 1,   
    0, 0, 1, 1, 1, 1,   
    0, 0, 0, 0, 0, 0,   
    0, 1, 1, 1, 1, 1    
  ),
  treatment = c(
    10, 10, 10, 20, 20, 20,   
    10, 10, 20, 20, 20, 20,   
    10, 10, 10, 10, 10, 10,   
    10, 20, 20, 20, 20, 20    
  ),
  cohort = rep(c(2003, 2002, 999999, 2001), each = 6)
)
  data <- create_cohort_share(data = data)
  data <- create_time_share(data = data)
  data_subset <- subset_data(data, 2001, 999999)

  result <- calculate_weight_numerator(data, data_subset, 2001, 999999)

  expect_type(result, "double")
  expect_equal(result, (0.25) * (0.25) * (5/6) * (1/6) * (10))
})

test_that("calculate_weight_numerator works for Exposed vs Not Yet Exposed", {
  data <- data.frame(
  id = rep(1:4, each = 6),
  time = rep(2000:2005, times = 4),
  instrument = c(
    0, 0, 0, 1, 1, 1,   
    0, 0, 1, 1, 1, 1,   
    0, 0, 0, 0, 0, 0,   
    0, 1, 1, 1, 1, 1    
  ),
  treatment = c(
    10, 10, 10, 20, 20, 20,   
    10, 10, 20, 20, 20, 20,   
    10, 10, 10, 10, 10, 10,   
    10, 20, 20, 20, 20, 20    
  ),
  cohort = rep(c(2003, 2002, 999999, 2001), each = 6)
)
  data <- create_cohort_share(data = data)
  data <- create_time_share(data = data)
  data_subset <- subset(data, cohort %in% c(2001, 2003) & time < 2003)

  result <- calculate_weight_numerator(data, data_subset, 2001, 2003)

  expect_type(result, "double")
  expect_equal(result, (1/16) * (1/4) * (2/3) * (1/3) *(10))
})

test_that("calculate_weight_numerator works for Exposed vs Exposed Shift", {
  data <- data.frame(
  id = rep(1:4, each = 6),
  time = rep(2000:2005, times = 4),
  instrument = c(
    0, 0, 0, 1, 1, 1,   
    0, 0, 1, 1, 1, 1,   
    0, 0, 0, 0, 0, 0,   
    0, 1, 1, 1, 1, 1    
  ),
  treatment = c(
    10, 10, 10, 20, 20, 20,   
    10, 10, 20, 20, 20, 20,   
    10, 10, 10, 10, 10, 10,   
    10, 20, 20, 20, 20, 20    
  ),
  cohort = rep(c(2003, 2002, 999999, 2001), each = 6)
)
  data <- create_cohort_share(data = data)
  data <- create_time_share(data = data)
  data_subset <- subset(data, cohort %in% c(2003, 2001) & time >= 2001)

  result <- calculate_weight_numerator(data, data_subset, 2003, 2001)

  expect_type(result, "double")
  expect_equal(result, (25/144) * (1/4) * (3/5) * (2/5) * (10))
})

