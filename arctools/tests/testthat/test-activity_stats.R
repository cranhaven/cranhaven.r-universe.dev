
# rm(list = ls()); library(testthat); library(arctools)

require(data.table)
require(lubridate)
require(dplyr)


## CONTEXT (without exclude or include) ----------------------------------------

context("Testing activity_stats()")

out_activity_stats <- lapply(extdata_fnames, function(extdata_fname_i) {
  dat_i <-
    fread(system.file("extdata", extdata_fname_i, package = "arctools")) %>%
    as.data.frame()
  acc     <- dat_i$vectormagnitude
  acc_ts  <- ymd_hms(dat_i$timestamp)
  out <- activity_stats(acc, acc_ts)
  return(out)
})

out_all_steps <- lapply(extdata_fnames, function(extdata_fname_i) {
  dat_i <-
    fread(system.file("extdata", extdata_fname_i, package = "arctools")) %>%
    as.data.frame()
  acc    <- dat_i$vectormagnitude
  acc_ts <- ymd_hms(dat_i$timestamp)
  ## Get acc data vector in "midnight_to_midnight" format
  acc <- midnight_to_midnight(acc, acc_ts)
  ## Get wear/non-wear flag
  wear_flag <- get_wear_flag(acc)
  ## Get valid/non-valid day flag
  valid_day_flag <- get_valid_day_flag(wear_flag)
  ## Impute missing data in acc data vector
  acc <- impute_missing_data(acc, wear_flag, valid_day_flag)
  ## Summarize PA
  out <- summarize_PA(acc, acc_ts, wear_flag, valid_day_flag)
  return(out)
})


test_that_desc <- paste0(
  "Compare the wrapper out_activity_stats() gives same results as step by step procedure")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(out_activity_stats)){ # i <- 4
    out1 <- out_activity_stats[[i]]
    out2 <- out_all_steps[[i]]
    expect_equal(unlist(out1), unlist(out2))
  }
})


## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

context("Testing summarize_PA() for computing summaries in subsets per days of the weeks")

set.seed(1)
n <- 1440 * 7
acc <- round(10000 * runif(n = n))
acc_ts <- seq(from = as.POSIXct("2020-09-21 00:00:00.00", tz = "UTC"), by = 60, length.out = n)

test_that("Error ocurrs when subset_weekdays arg is misspecified", {
  skip_on_cran()
  expect_error({
    as_out <- activity_stats(acc, acc_ts, subset_weekdays = c(0))
  })
})


test_that("The value of activity_stats remains unchanged", {
  skip_on_cran()

  out_act <- unlist(activity_stats(acc, acc_ts))
  out_exp <- c(n_days = 7, n_valid_days = 7, wear_time_on_valid_days = 1440,
               tac = 7197993.14285714, tlac = 11813.1031994869, ltac = 15.7893128149751,
               astp = 0.187064433304805, satp = 0.804313519200421, time_spent_active = 1168.42857142857,
               time_spent_nonactive = 271.571428571429, no_of_active_bouts = 218.571428571429,
               no_of_nonactive_bouts = 218.428571428571, mean_active_bout = 5.34575163398693,
               mean_nonactive_bout = 1.24329627207325)
  expect_equal(out_act, out_exp)

  out_act <- unlist(activity_stats(acc, acc_ts, subset_weekdays = c(7,1)))
  out_exp <- c(n_days = 2, n_valid_days = 2, wear_time_on_valid_days = 1440,
               tac_weekdays17only = 7107296, tlac_weekdays17only = 11791.0154371118,
               ltac_weekdays17only = 15.7766324200201, astp_weekdays17only = 0.194145501506672,
               satp_weekdays17only = 0.807899461400359, time_spent_active_weekdays17only = 1161.5,
               time_spent_nonactive_weekdays17only = 278.5, no_of_active_bouts_weekdays17only = 225.5,
               no_of_nonactive_bouts_weekdays17only = 225, mean_active_bout_weekdays17only = 5.15077605321508,
               mean_nonactive_bout_weekdays17only = 1.23777777777778)
  expect_equal(out_act, out_exp)

  out_act <- unlist(activity_stats(acc, acc_ts, subset_weekdays = c(2:6)))
  out_exp <- c(n_days = 5, n_valid_days = 5, wear_time_on_valid_days = 1440,
               tac_weekdays23456only = 7234272, tlac_weekdays23456only = 11821.9383044369,
               ltac_weekdays23456only = 15.7943402910085, astp_weekdays23456only = 0.184426229508197,
               satp_weekdays23456only = 0.802827380952381, time_spent_active_weekdays23456only = 1171.2,
               time_spent_nonactive_weekdays23456only = 268.8, no_of_active_bouts_weekdays23456only = 216,
               no_of_nonactive_bouts_weekdays23456only = 215.8, mean_active_bout_weekdays23456only = 5.42222222222222,
               mean_nonactive_bout_weekdays23456only = 1.24559777571826)
  expect_equal(out_act, out_exp)
})
