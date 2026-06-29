
# rm(list = ls()); library(testthat); library(arctools)

require(data.table)
require(lubridate)
require(dplyr)

## Alternative implementation --------------------------------------------------

ImputeMissingData_JU = function(ac, wnw,  win_min = 90, win_max = 180){
  wnw_shift = c(wnw[2:length(wnw)], wnw[1])
  starts = which((wnw-wnw_shift) > 0) + 1
  stops = which((wnw-wnw_shift) < 0)
  ### Markers for data imputation
  if(length(starts) == 0 & length(stops) == 0){ #is there is non non-wear - Break
    return(ac)
  }
  if(length(starts) > length(stops)){stops = c(stops, length(ac))} # if there is a non-wear with no end at the end of the data - force end
  if(length(stops) > length(starts)){starts = c(1, starts)} # if there is a non-wear with no start at the beginning of the data - force start
  #if(starts[1] > stops[1]){stops = stops[-1]}
  lengths = stops - starts
  indimp = which(lengths > win_min & lengths < win_max)
  starts = starts[indimp]
  stops = stops[indimp]
  impMark = rep(0, length(wnw))
  for (j in 1:length(indimp)){
    if (length(starts) == 0) {break}
    impMark[starts[j]:stops[j]] = 1
  }
  impMarkM = matrix(impMark, ncol = 1440, byrow = T)
  ##############################################
  ac1440 = matrix(ac, ncol = 1440, byrow = T)
  wnw1440 = matrix(wnw, ncol = 1440, byrow = T)
  ##########################
  wnwmx = wnw1440
  wnwmx[which(wnw1440 == 0, arr.ind = TRUE)] = NA
  pa.data.mask = ac1440*wnwmx
  ### rows for data imputation
  improws = unique(which(impMarkM == 1, arr.ind = TRUE)[,1])
  if(length(improws) == 0){return(ac)}
  for (i in 1:length(improws)){
    cm = colMeans(pa.data.mask[-improws[i],],na.rm = TRUE)
    ac1440[improws[i],which(impMarkM[improws[i],] == 1)] = cm[which(impMarkM[improws[i],] == 1)]
  }
  ac = as.vector(t(ac1440))
  return(ac)
}



## Define objects common to all test contexts  ---------------------------------

acc_list <- lapply(extdata_fnames, function(extdata_fname_i) {
  dat_i <-
    fread(system.file("extdata", extdata_fname_i, package = "arctools")) %>%
    as.data.frame()
  acc     <- dat_i$vectormagnitude
  acc_ts  <- ymd_hms(dat_i$timestamp)
  acc     <- midnight_to_midnight(acc, acc_ts)
  return(acc)
})

wear_flag_list <- lapply(acc_list, function(acc) {  # acc <- acc_list[[1]]
  wear_flag <- get_wear_flag(acc)
  return(wear_flag)
})

valid_day_flag_list <- lapply(wear_flag_list, function(wear_flag) { # wear_flag  <- wear_flag_list[[1]]
  valid_day_flag <- get_valid_day_flag(wear_flag)
  return(valid_day_flag)
})

ImputeMissingData_out_list <- lapply(1:length(acc_list), function(i){
  acc            <- acc_list[[i]]
  wear_flag      <- wear_flag_list[[i]]
  valid_day_flag <- valid_day_flag_list[[i]]
  acc_imputed    <- impute_missing_data(acc, wear_flag, valid_day_flag)
  return(acc_imputed)
})

ImputeMissingData_F_out_list <- lapply(1:length(acc_list), function(i){
  acc            <- acc_list[[i]]
  wear_flag      <- wear_flag_list[[i]]
  valid_day_flag <- valid_day_flag_list[[i]]
  acc_imputed    <- impute_missing_data(acc, wear_flag, valid_day_flag, imputeFromValidDaysOnly = FALSE)
  return(acc_imputed)
})

ImputeMissingData_JU_out_list <- lapply(1:length(acc_list), function(i){ # i <- 1
  acc            <- acc_list[[i]]
  wear_flag      <- wear_flag_list[[i]]
  valid_day_flag <- valid_day_flag_list[[i]]
  out            <- ImputeMissingData_JU(acc, wear_flag)
  return(out)
})



## Context ---------------------------------------------------------------------

context("Testing impute_missing_data()")

test_that_desc <- paste0(
  "Compare updated implementation (@MK; used imputeFromValidDaysOnly = FALSE) with the previous implementation (@JU): ",
  "Compare imputed observations for valid days only")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(acc_list)){ # i <- 1
    out1 <- ImputeMissingData_F_out_list[[i]]
    out2 <- ImputeMissingData_JU_out_list[[i]]
    valid_day_flag  <- valid_day_flag_list[[i]]
    wear_flag <- wear_flag_list[[i]]
    ## Compare on valid days only b/c old implementation imputes non valid days too
    idx <- which(valid_day_flag == 1)
    expect_equal(out1[idx],
                 out2[idx])
  }
})

test_that_desc <- paste0(
  "Test that NA values are not replaced in activity count vector (i.e. before/after data collection start/end)")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(acc_list)){ # i <- 1
    acc   <- acc_list[[i]]
    out1 <- ImputeMissingData_out_list[[i]]
    expect_equal(which(is.na(acc)),
                 which(is.na(out1)))
  }
})


test_that_desc <- paste0(
  "Test that values not marked as wear OR non-valid days remain unchanged")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(acc_list)){ # i <- 1
    acc  <- acc_list[[i]]
    out1 <- ImputeMissingData_out_list[[i]]
    valid_day_flag  <- valid_day_flag_list[[i]]
    wear_flag       <- wear_flag_list[[i]]
    idx  <- which((valid_day_flag == 0) | (wear_flag == 1))
    expect_equal(out1[idx],
                 acc[idx])
  }
})


test_that("Test that output value remained unchanged", {
  skip_on_cran()
    vec_act <- sapply(ImputeMissingData_out_list, function(out) sum(out, na.rm = TRUE))
    vec_exp <- c(18677002, 20765032.5, 7789617, 2896256)
    expect_equal(vec_act, vec_exp)
})


test_that("Test that method returns error if any inout data not in MidnightToMidnight form", {
  skip_on_cran()
  i <- 1
  acc             <- acc_list[[i]]
  wear_flag       <- wear_flag_list[[i]]
  valid_day_flag  <- valid_day_flag_list[[i]]
  expect_error({
    impute_missing_data(acc[2:length(acc)], wear_flag, valid_day_flag)
  })
  expect_error({
    impute_missing_data(acc, wear_flag[2:length(wear_flag)], valid_day_flag)
  })
  expect_error({
    valid_day_flag <- valid_day_flag[2:length(valid_day_flag)]
    impute_missing_data(acc, wear_flag, valid_day_flag)
  })
})


test_that("Test data is unchanged if no non-wear is detected", {
  skip_on_cran()
  i <- 1
  acc            <- acc_list[[i]]
  wear_flag      <- rep(1, length(acc))
  valid_day_flag <- valid_day_flag_list[[i]]
  out            <- impute_missing_data(acc, wear_flag, valid_day_flag)
  expect_equal(acc, out)
})


test_that("Test data is unchanged if no valid days", {
  skip_on_cran()
  i <- 1
  acc            <- acc_list[[i]]
  wear_flag      <- wear_flag_list[[i]]
  valid_day_flag <- rep(0, length(wear_flag))
  out            <- impute_missing_data(acc, wear_flag, valid_day_flag)
  expect_equal(acc, out)
})


