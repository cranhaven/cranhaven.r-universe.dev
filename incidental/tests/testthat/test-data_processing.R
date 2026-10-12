library(incidental)

reported_vec_good = 1:15
delay_dist_good = c(.3, .4, .3)

test_that("error for negative reported", {
  reported_vec_bad_negative = c(1:10, -1)
  expect_error(
    data_check(reported = reported_vec_bad_negative,
               delay_dist = delay_dist_good), 
    "Observations need to be positive.")
})

test_that("error for numeric reported", {
  reported_vec_bad_numeric = c(1:10, 1.5)
  expect_error(
    data_check(reported = reported_vec_bad_numeric,
               delay_dist = delay_dist_good), 
    "Observations need to be integers.")
})

test_that("error for short reported", {
  reported_vec_bad_short = 1:4
  expect_error(
    data_check(reported = reported_vec_bad_short,
               delay_dist = delay_dist_good), 
    "More observations needed to fit incidence.")
})

test_that("error for negative delay distribution", {
  delay_dist_bad_negative = c(.6, .6, -.2)
  expect_error(
    data_check(reported = reported_vec_good,
               delay_dist = delay_dist_bad_negative), 
    "Delay distribution needs to be positive.")
})

test_that("error for not a distrubtion delay distribution", {
  delay_dist_bad_not_a_dist = c(.6, .6)
  expect_error(
    data_check(reported = reported_vec_good,
               delay_dist = delay_dist_bad_not_a_dist), 
    "Delay distribution needs to sum to 1.")
})

test_that("front pad with positive number of zeros works", {
  expected_reported = c(0,0,0, reported_vec_good)
  expect_equal(front_zero_pad(reported_vec_good, 3), expected_reported)
})

test_that("front pad with zero zeros works", {
  expect_equal(front_zero_pad(reported_vec_good, 0), reported_vec_good)
})

test_that("ar extrapolation works for standard data", {
  reported = c(3,  0,  0,  3,  4,  5,  8,  5, 10,  6, 10, 13, 13, 16, 15, 18,
               20, 16, 15, 20, 20, 20, 22, 22, 27, 29, 30, 27, 33, 29)
  ar_row_1 = c(32, 33, 28, 38, 46, 41, 44, 56, 61, 58)
  ar_row_2 = c(45, 48, 44, 30, 43, 43, 43, 56, 63, 68)
  
  ar_mean_row_1 = c(25, 26, 22, 30, 32, 27, 30, 34, 37, 36)
  ar_mean_row_2 = c(37, 39, 35, 23, 29, 29, 29, 34, 39, 43)
  expected_ar = mat.or.vec(2, length(reported) + 10)
  expected_ar[1,1:30] = reported
  expected_ar[2,1:30] = reported
  expected_ar[1,31:40] = ar_row_1
  expected_ar[2,31:40] = ar_row_2
  
  expected_ar_mean = mat.or.vec(2, length(reported) + 10)
  expected_ar_mean[1,1:30] = reported
  expected_ar_mean[2,1:30] = reported
  expected_ar_mean[1,31:40] = ar_mean_row_1
  expected_ar_mean[2,31:40] = ar_mean_row_2
  expect_equal(
    make_ar_extrap_samps(
      reported = reported,
      num_ar_samps = 2,
      num_ar_steps = 10,
      linear_tail = 10,
      extrapolation_prior_precision = 0,
      seed = 1), 
    expected_ar)
  
  expect_equal(
    make_ar_extrap_samps(
      reported = reported,
      num_ar_samps = 2,
      num_ar_steps = 10,
      linear_tail = 0,
      extrapolation_prior_precision = 0, 
      seed = 1), 
    expected_ar_mean)
})


test_that("ar extrapolation works for low variance data", {
  reported = c(3,  0,  0,  3,  4,  5,  8,  5, 10,  6, 10, 13, 13, 16, 15, 18,
               20, 16, 15, 20, 20, 20, 21, 20, 20, 20, 20, 20, 20, 23)
  
  ar_mean_row_1 = c(18, 18, 15, 22, 23, 20, 22, 25, 28, 27)
  ar_mean_row_2 = c(28, 30, 26, 16, 21, 21, 21, 25, 30, 33)
  
  expected_ar_mean = mat.or.vec(2, length(reported) + 10)
  expected_ar_mean[1,1:30] = reported
  expected_ar_mean[2,1:30] = reported
  expected_ar_mean[1,31:40] = ar_mean_row_1
  expected_ar_mean[2,31:40] = ar_mean_row_2
  expect_equal(
    make_ar_extrap_samps(
      reported = reported,
      num_ar_samps = 2,
      num_ar_steps = 10,
      linear_tail = 10,
      extrapolation_prior_precision = 0,
      seed = 1), 
    expected_ar_mean)
})

reported_vec_good = NULL
delay_dist_good = NULL
