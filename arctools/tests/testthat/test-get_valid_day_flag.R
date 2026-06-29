
# rm(list = ls()); library(testthat); library(arctools)

require(data.table)
require(lubridate)
require(dplyr)


## Alternative implementation --------------------------------------------------

ValidDays_JU = function(wnw, win_max = 180){

  # @MK commented
  # ac1440 = matrix(ac, ncol = 1440, byrow = T)
  wnw1440 = matrix(wnw, ncol = 1440, byrow = T)

  valid = rowSums(wnw1440, na.rm = T) > (1440 - win_max)
  valid.long = rep(valid, each = 1440)

  N.days = sum(valid)

  valid.long = as.data.frame(valid.long)
  names(valid.long) = as.character(N.days)
  return(valid.long)
}



## Define objects common to all test contexts  ---------------------------------

## Read data from files attached to the package to be used in tests
wnw_list <- lapply(extdata_fnames, function(extdata_fname_i) {
  dat_i <-
    fread(system.file("extdata", extdata_fname_i, package = "arctools")) %>%
    as.data.frame()
  acc     <- dat_i$vectormagnitude
  acc_ts  <- ymd_hms(dat_i$timestamp)
  acc <- midnight_to_midnight(acc, acc_ts)
  wear_flag <- get_wear_flag(acc)
  return(wear_flag)
})

## Precompute output with the use of package function implementation
ValidDays_out_list <- lapply(wnw_list, function(wear_flag){
  valid_day_flag <- get_valid_day_flag(wear_flag)
  valid_days_cnt <- sum(valid_day_flag) / 1440
  out <- list(valid.days.long = valid_day_flag, valid.days.cnt = valid_days_cnt)
  return(out)
})

## Precompute output with the use of alternative (old) function implementation
ValidDays_JU_out_list <- lapply(wnw_list, function(wnw_i){
  vd_JH_i <- ValidDays_JU(wnw_i, 144)
  out <- list(valid.days.long = vd_JH_i[,1] * 1, valid.days.cnt = as.numeric(names(vd_JH_i)))
  return(out)
})



## Context ---------------------------------------------------------------------

context("Testing get_valid_day_flag()")

test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "Same number of valid days")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(wnw_list)){
    out1 <- ValidDays_out_list[[i]]
    out2 <- ValidDays_JU_out_list[[i]]
    expect_equal(out1$valid.days.cnt,
                 out2$valid.days.cnt)
  }
})

test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "Same number of values in long vector")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(wnw_list)){
    out1 <- ValidDays_out_list[[i]]
    out2 <- ValidDays_JU_out_list[[i]]
    expect_equal(out1$valid.days.long,
                 out2$valid.days.long)
  }
})


test_that("Test method correctly computes number of valid days: case 1", {
  skip_on_cran()
  vec <- rep(c(0,1), times = 1440)
  valid_day_flag <- get_valid_day_flag(vec)
  valid_days_cnt <- sum(valid_day_flag) / 1440
  out <- list(valid.days.long = valid_day_flag,
              valid.days.cnt = valid_days_cnt)
  expect_equal(out$valid.days.long, rep(0, 1440 * 2))
  expect_equal(out$valid.days.cnt, 0)
})


test_that("Test method correctly computes number of valid days: case 2", {
  skip_on_cran()
  vec <- rep(c(0,1,1,1,1,1,1,1,1,1), times = 1440)
  valid_day_flag <- get_valid_day_flag(vec)
  valid_days_cnt <- sum(valid_day_flag) / 1440
  out <- list(valid.days.long = valid_day_flag,
              valid.days.cnt = valid_days_cnt)
  expect_equal(out$valid.days.long, rep(1, 1440 * 10))
  expect_equal(out$valid.days.cnt, 10)
})

test_that("Test method correctly computes number of valid days: case 3", {
  skip_on_cran()
  vec <- rep(1, 1439)
  expect_error({
    get_valid_day_flag(vec, 144)
  })
})

test_that("Test method correctly computes number of valid days: case 4", {
  skip_on_cran()
  vec <- rep(1, 1439 + 1440)
  expect_error({
    get_valid_day_flag(vec, 144)
  })
})

test_that("Test method correctly computes number of valid days: case 4", {
  skip_on_cran()
  vec <- rep(1, 1440)
  vec[1] <- 2
  expect_error({
    get_valid_day_flag(vec, 144)
  })
})
