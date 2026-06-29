
# rm(list = ls()); library(testthat); library(arctools)

require(runstats)
require(data.table)
require(dplyr)

## Alternative implementation --------------------------------------------------

WearNonWear_JU = function(ac, win = 90){
  ### Create WNW flags
  # 0 - non-wear
  # 1 - wear

  ac.ones = rep(0, length(ac))
  ac.ones[ac > 0] = 1

  meanw = RunningMean(x = ac.ones, W = win)
  meanw[meanw < 1/win] = 0

  out = rep(1, length(ac))
  ind = which(meanw == 0)

  if(length(ind) != 0){ # only run when there in as non-wear time
    for(i in 1:length(ind)){
      out[ind[i]:(ind[i] + win - 1)] = 0
    }
  }

  out[is.na(ac)] = NA
  return(out)
}



## Define objects common to all test contexts  ---------------------------------

## Read data from files attached to the package to be used in tests
dat_list <- lapply(extdata_fnames, function(extdata_fname_i) {
  fread(system.file("extdata", extdata_fname_i, package = "arctools")) %>%
    as.data.frame()
})
## Precomoute output with the use of package function implementation
WearNonWear_out_list <- lapply(dat_list, function(dat_i){
  get_wear_flag(dat_i$vectormagnitude, 90)
})
## Precomoute output with the use of alternative (old) function implementation
WearNonWear_JU_out_list <- lapply(dat_list, function(dat_i){
  WearNonWear_JU(dat_i$vectormagnitude, 90)
})



## Context ---------------------------------------------------------------------

context("Testing get_wear_flag()")

test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "The same number of NA entries")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(dat_list)){
    out1 <- WearNonWear_out_list[[i]]
    out2 <- WearNonWear_JU_out_list[[i]]
    expect_equal(sum(!is.na(out1)),
                 sum(!is.na(out2)))
  }
})

test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "The same location of NA entries")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(dat_list)){
    out1 <- WearNonWear_out_list[[i]]
    out2 <- WearNonWear_JU_out_list[[i]]
    expect_true(all(which(!is.na(out1)) == which(!is.na(out2))))
  }
})

test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "The same values of non-NA entries")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(dat_list)){
    out1 <- WearNonWear_out_list[[i]]
    out2 <- WearNonWear_JU_out_list[[i]]
    expect_true( all(out1[!is.na(out1)] == out2[(!is.na(out2))]))
  }
})



## Sub-context -----------------------------------------------------------------

test_that("Test method correctly computes non-wear: case 1", {
  skip_on_cran()
  ac_vec <- rep(c(0,0,0,1), 10)
  out <- get_wear_flag(ac_vec, 4)
  expect_true(all(out == 1))
})

test_that("Test method correctly computes non-wear: case 2", {
  skip_on_cran()
  ac_vec <- rep(c(0,0,0,1), 10)
  out <- get_wear_flag(ac_vec, 3)
  expect_true(all(out[1:4] == c(0,0,0,1)))
  expect_true(sum(out) == 10)
})

test_that("Test method correctly computes non-wear: case 3", {
  skip_on_cran()
  ac_vec <- rep(c(0,0,0,1), 10)
  out <- get_wear_flag(ac_vec, 2)
  expect_true(all(out[1:4] == c(0,0,0,1)))
  expect_true(sum(out) == 10)
})

test_that("Test method correctly computes non-wear: case 4", {
  skip_on_cran()
  ac_vec <- rep(c(0,0,0,0.5), 10)
  out <- get_wear_flag(ac_vec, 2)
  expect_true(all(out[1:4] == c(0,0,0,1)))
  expect_true(sum(out) == 10)
})


test_that("Test method correctly computes non-wear: case 5", {
  skip_on_cran()
  ac_vec <- rep(c(0,0,0,0), 10)
  out <- get_wear_flag(ac_vec, 4)
  expect_true(all(out == 0))
})

test_that("Test method correctly computes non-wear: case 6", {
  skip_on_cran()
  set.seed(1)
  ac_vec <- rep(c(runif(2, 0.1), 0,0), 10)
  out <- get_wear_flag(ac_vec, 3)
  expect_true(all(out == 1))
})

