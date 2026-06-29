
# rm(list = ls()); library(testthat); library(arctools)

require(data.table)
require(lubridate)
require(dplyr)

## Alternative implementation --------------------------------------------------

SummarizePA_JU_forTests = function(
  ac,
  acc_ts,
  wear_flag,
  valid_day_flag,
  sed.threshold = 1853,
  exclude.minutes = NULL,
  subset.minutes = NULL,
  in.bed.time = NULL,
  out.bed.time = NULL)
{

  require(lubridate)

  names.expand1 = NULL
  names.expand2 = NULL
  names.expand3 = NULL

  valid.long = valid_day_flag
  N.valid.days = sum(valid_day_flag) / 1440
  # N.valid.days = as.double(names(valid.long)[1])

  # Remove Time-in-bed
  if(!is.null(in.bed.time)){

    # acc_ts from midnight to midnight
    # acc_ts = ymd_hms(acc_ts)
    acc_ts = midnight_to_midnight(acc_ts, acc_ts)

    # mask out in-bed-intervals
    for(j in 1:length(in.bed.time)){
      ac[acc_ts >= in.bed.time[j] & acc_ts <= out.bed.time[j]] = NA
    }

    # # remove the first and the last day
    # ac = ac[1441:(length(ac)-1440)]
    # valid.long = valid.long[1441:(nrow(valid.long)-1440),]

    # names
    names.expand1 = '_InBedRemoved'
  }

  # Exclude night time
  if(length(exclude.minutes) > 0){
    ac1 = matrix(ac, ncol = 1440, byrow = TRUE)
    ac1[, exclude.minutes] = NA
    ac = as.vector(t(ac1))

    # names
    a = round(exclude.minutes[1]*24/1440)
    b = round(exclude.minutes[length(exclude.minutes)]*24/1440)
    names.expand2 = paste0('_', a, 'to', b, 'Removed')
  }

  # Subset partial day
  if(length(subset.minutes) > 0){
    ac1 = matrix(ac, ncol = 1440, byrow = TRUE)
    ac1[, -subset.minutes] = NA
    ac = as.vector(t(ac1))

    # names
    a = round(subset.minutes[1]*24/1440)
    b = round(subset.minutes[length(subset.minutes)]*24/1440)
    names.expand3 = paste0('_', a, 'to', b, 'Only')
  }


  TAC = sum(ac[valid.long == TRUE], na.rm = TRUE)/N.valid.days
  TLAC = sum(log(1+ac[valid.long == TRUE]), na.rm = TRUE)/N.valid.days
  LTAC = log(TAC)

  ####### ASTP
  activei = rep(0, length(ac))
  activei[ac >= sed.threshold] = 1
  activei[valid.long == FALSE] = 0 # remove activity from non-valid days
  #activei.shift = c(activei[2:1440], activei[1])
  activei.diff = diff(activei)
  starts = which(activei.diff == 1)
  stops = which(activei.diff == -1)

  if(length(starts) == 0 | length(stops) == 0){
    time.spent.active = 0
    mean.active.bout = 0
    astp = NA
    no.of.active.bouts = 0
    time.spent.active = 0
  }else{

    if (stops[1] < starts[1]){
      stops = stops[2:length(stops)]
      message("[ASTP] stops = stops[2:length(stops)]")
    }
    if (starts[length(starts)] > stops[length(stops)]){
      starts = starts[1:(length(starts)-1)]
      message("[ASTP] starts = starts[1:(length(starts)-1)]")
    }

    durations = stops - starts
    time.spent.active = sum(durations)
    mean.active.bout = mean(durations)

    astp = 1/mean.active.bout
    no.of.active.bouts = length(durations)/N.valid.days
  }
  ###########################
  ############### SATP
  activei = rep(0, length(ac))
  activei[ac < sed.threshold] = 1
  activei[valid.long == FALSE] = 0
  #activei.shift = c(activei[2:1440], activei[1])
  activei.diff = diff(activei)
  starts = which(activei.diff == 1)
  stops = which(activei.diff == -1)

  if(length(starts) == 0 | length(stops) == 0){
    time.spent.nonactive = 0
    mean.nonactive.bout = 0
    satp = NA
    no.of.nonactive.bouts = 0
    time.spent.nonactive = 0
    durations = NULL
  }else{

    if (stops[1] < starts[1]){
      stops = stops[2:length(stops)]
      message("[SATP] stops = stops[2:length(stops)]")
    }
    if (starts[length(starts)] > stops[length(stops)]){
      starts = starts[1:(length(starts)-1)]
      message("[SATP] starts = starts[1:(length(starts)-1)]")
    }

    durations = stops - starts
    time.spent.nonactive = sum(durations)
    mean.nonactive.bout = mean(durations)

    satp = 1/mean.nonactive.bout
  }

  time.spent.active = time.spent.active/N.valid.days
  time.spent.nonactive = time.spent.nonactive/N.valid.days
  no.of.nonactive.bouts = length(durations)/N.valid.days
  #################
  wear.time.on.valid.days = sum(wear_flag[valid.long == 1], na.rm = T)/N.valid.days
  N.days = length(ac)/1440

  out = data.frame(
    n_days = N.days,
    n_valid_days = N.valid.days,
    wear_time_on_valid_days = wear.time.on.valid.days,
    tac = TAC,
    tlac = TLAC,
    ltac = LTAC,
    astp = astp,
    satp = satp,
    time_spent_active = time.spent.active,
    time_spent_nonactive = time.spent.nonactive,
    no_of_active_bouts = no.of.active.bouts,
    no_of_nonactive_bouts = no.of.nonactive.bouts,
    mean_active_bout = mean.active.bout,
    mean_nonactive_bout = mean.nonactive.bout)

  ## Correct if no valid days
  if (N.valid.days == 0){
    out[, 3:ncol(out)] <- NA
  }

  ####################

  ###### Replace Inf with NA
  out[out == Inf] = NA

  # Rename for subset, partial day or time-in-bed
  names(out)[-(1:3)] = paste0(names(out)[-(1:3)], names.expand1, names.expand2, names.expand3)

  # Names to lower case
  names(out) = tolower(names(out))
  return(out)

  # In the names - replace dots with underscores
  names(out) = gsub('\\.', '_', names(out))
}



## CONTEXT (without exclude or include) ----------------------------------------

context("Testing summarize_PA() (without exclude or include)")

## Read data from files attached to the package to be used in tests
acc_list <- lapply(extdata_fnames, function(extdata_fname_i) {
  dat_i <-
    fread(system.file("extdata", extdata_fname_i, package = "arctools")) %>%
    as.data.frame()
  acc     <- dat_i$vectormagnitude
  acc_ts  <- ymd_hms(dat_i$timestamp)
  acc     <- midnight_to_midnight(acc, acc_ts)
  return(acc)
})

acc_ts_list <- lapply(extdata_fnames, function(extdata_fname_i) {
  dat_i <-
    fread(system.file("extdata", extdata_fname_i, package = "arctools")) %>%
    as.data.frame()
  acc_ts  <- ymd_hms(dat_i$timestamp)
  return(acc_ts)
})

wear_flag_list <- lapply(acc_list, function(acc) {  # acc <- acc_list[[1]]
  wear_flag <- get_wear_flag(acc)
  return(wear_flag)
})

valid_day_flag_list <- lapply(wear_flag_list, function(wear_flag) { # wear_flag  <- wear_flag_list[[1]]
  valid_day_flag <- get_valid_day_flag(wear_flag)
  return(valid_day_flag)
})


## Precomoute output with the use of package function implementation
SummarizePA_JU_out_list <- lapply(1:length(acc_list), function(i){ # i <- 1
  acc            <- acc_list[[i]]
  acc_ts         <- acc_ts_list[[i]]
  wear_flag      <- wear_flag_list[[i]]
  valid_day_flag <- valid_day_flag_list[[i]]
  acc            <- impute_missing_data(acc, wear_flag, valid_day_flag)
  out            <- SummarizePA_JU_forTests(acc, acc_ts, wear_flag, valid_day_flag)
  return(out)
})

SummarizePA_out_list <- lapply(1:length(acc_list), function(i){ # i <- 1
  acc            <- acc_list[[i]]
  acc_ts         <- acc_ts_list[[i]]
  wear_flag      <- wear_flag_list[[i]]
  valid_day_flag <- valid_day_flag_list[[i]]
  acc            <- impute_missing_data(acc, wear_flag, valid_day_flag)
  out            <- summarize_PA(acc, acc_ts, wear_flag, valid_day_flag)
  return(out)
})


test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "The same output values")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(acc_list)){ # i <- 4
    print(i)
    out1 <- SummarizePA_JU_out_list[[i]]
    out2 <- SummarizePA_out_list[[i]]
    expect_equal(unlist(out1), unlist(out2))
  }
})


test_that_desc <- paste0(
  "Test that the value of running the function remained unchanged.")
test_that(test_that_desc, {
  skip_on_cran()
  ## Pull all values from all the output lists
  out <- unlist(lapply(SummarizePA_out_list, function(val) unlist(val)))
  out <- out[!is.na(out)]
  ## Get statisticsl of the quantile values
  out_act <- quantile(out, probs = seq(0.1, 0.9, 0.1))
  out_exp <- c(`10%` = 0.21451770919856, `20%` = 3.20461095100865, `30%` = 8,
               `40%` = 13.8473432917687, `50%` = 25.5, `60%` = 89.4, `70%` = 397.75,
               `80%` = 1333.77222222222, `90%` = 5937.6679924896)
  expect_equal(out_act, out_exp, tolerance=1e-6)
  ## Get statistics of the mean value
  out_act <- mean(out)
  out_exp <- 132683.792848205
  expect_equal(out_act, out_exp, tolerance=1e-6)
})


rm(SummarizePA_out_list)
rm(SummarizePA_JU_out_list)




## CONTEXT (use fixed range of minutes only) -----------------------------------

context("Testing summarize_PA() (use fixed range of minutes only)")

subset_12am_6am = 1 : (6 * 1440/24)
subset_6am_12pm = (6 * 1440/24 + 1) : (12 * 1440/24)
subset_12pm_6pm = (12 * 1440/24 + 1) : (18 * 1440/24)
subset_6pm_12am = (18 * 1440/24 + 1) : (24 * 1440/24)
subset.minutes.list <- list(subset_12am_6am, subset_6am_12pm, subset_12pm_6pm, subset_6pm_12am)

## Precompute output with the use of package function implementation
SummarizePA_out_list    <- list()
SummarizePA_JU_out_list <- list()

for (i in 1:length(acc_list)){ # i <- 1

  acc            <- acc_list[[i]]
  acc_ts         <- acc_ts_list[[i]]
  wear_flag      <- wear_flag_list[[i]]
  valid_day_flag <- valid_day_flag_list[[i]]
  ## Skip missing data imputation

  for (subset.minutes in subset.minutes.list){  # subset.minutes <- subset.minutes.list[[1]]

    ## MK
    out <- summarize_PA(
      acc, acc_ts, wear_flag, valid_day_flag, subset_minutes = subset.minutes)
    SummarizePA_out_list <- c(SummarizePA_out_list, list(out))

    ## JU
    out <- SummarizePA_JU_forTests(
      acc, acc_ts, wear_flag, valid_day_flag, subset.minutes = subset.minutes)
    SummarizePA_JU_out_list <- c(SummarizePA_JU_out_list, list(out))
  }
}


test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "The same output values")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(acc_list)){ # i <- 1
    out1 <- SummarizePA_JU_out_list[[i]]
    out2 <- SummarizePA_out_list[[i]]
    expect_equal(unlist(out1), unlist(out2))
  }
})


test_that_desc <- paste0(
  "Test that the value of running the function remained unchanged.")
test_that(test_that_desc, {
  skip_on_cran()
  ## Pull all values from all the output lists
  out <- unlist(lapply(SummarizePA_out_list, function(val) unlist(val)))
  out <- out[!is.na(out)]
  ## Get statisticsl of the quantile values
  out_act <- quantile(out, probs = seq(0.1, 0.9, 0.1))
  out_exp <- c(`10%` = 0.195079194356651, `20%` = 3, `30%` = 6.8302752293578,
               `40%` = 10.0040650406504, `50%` = 17.2920634920635, `60%` = 30,
               `70%` = 92.7222222222224, `80%` = 351.5, `90%` = 1497.61022110541
  )
  expect_equal(out_act, out_exp, tolerance=1e-6)
  ## Get statistics of the mean value
  out_act <- mean(out)
  out_exp <- 33022.4419449636
  expect_equal(out_act, out_exp, tolerance=1e-6)
})

rm(SummarizePA_out_list,
   SummarizePA_JU_out_list)



## CONTEXT (exclude fixed range of minutes) ------------------------------------

context("Testing summarize_PA() (exclude fixed range of minutes)")

subset_12am_6am = 1 : (6 * 1440/24)
subset_6am_12pm = (6 * 1440/24 + 1) : (12 * 1440/24)
subset_12pm_6pm = (12 * 1440/24 + 1) : (18 * 1440/24)
subset_6pm_12am = (18 * 1440/24 + 1) : (24 * 1440/24)
subset.minutes.list <- list(subset_12am_6am, subset_6am_12pm, subset_12pm_6pm, subset_6pm_12am)

## Precompute output with the use of package function implementation
SummarizePA_out_list    <- list()
SummarizePA_JU_out_list <- list()

for (i in 1:length(acc_list)){ # i <- 1

  acc            <- acc_list[[i]]
  acc_ts         <- acc_ts_list[[i]]
  wear_flag      <- wear_flag_list[[i]]
  valid_day_flag <- valid_day_flag_list[[i]]

  for (subset.minutes in subset.minutes.list){  # subset.minutes <- subset.minutes.list[[1]]

    ## MK
    out <- summarize_PA(
      acc, acc_ts, wear_flag, valid_day_flag, exclude_minutes = subset.minutes)
    SummarizePA_out_list <- c(SummarizePA_out_list, list(out))

    ## JU
    out <- SummarizePA_JU_forTests(
      acc, acc_ts, wear_flag, valid_day_flag, exclude.minutes = subset.minutes)
    SummarizePA_JU_out_list <- c(SummarizePA_JU_out_list, list(out))
  }
}


test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "The same output values")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(SummarizePA_out_list)){ # i <- 1
    out1 <- SummarizePA_JU_out_list[[i]]
    out2 <- SummarizePA_out_list[[i]]
    expect_equal(unlist(out1), unlist(out2))
  }
})


test_that_desc <- paste0(
  "Test that the value of running the function remained unchanged.")
test_that(test_that_desc, {
  skip_on_cran()
  ## Pull all values from all the output lists
  out <- unlist(lapply(SummarizePA_out_list, function(val) unlist(val)))
  out <- out[!is.na(out)]
  ## Get statisticsl of the quantile values
  out_act <- quantile(out, probs = seq(0.1, 0.9, 0.1))
  out_exp <- c(`10%` = 0.178425049439684, `20%` = 3, `30%` = 8, `40%` = 13.3181904066867,
               `50%` = 25.5, `60%` = 79.25, `70%` = 300.194444444445, `80%` = 1424.55555555556,
               `90%` = 4473.42193163677)
  expect_equal(out_act, out_exp, tolerance=1e-6)
  ## Get statistics of the mean value
  out_act <- mean(out)
  out_exp <- 98863.1343057966
  expect_equal(out_act, out_exp, tolerance=1e-6)
})

rm(SummarizePA_out_list,
   SummarizePA_JU_out_list)



## CONTEXT (exclude sleep time)------------ ------------------------------------

context("Testing summarize_PA() (exclude sleep time)")

## Summarize PA without (i.e., excluding) minutes range corresponding to
## ActiLife-estimated sleep time.
SleepDetails_fname <- "BatchSleepExportDetails_2020-05-01_14-00-46.csv"
SleepDetails_fpath <- system.file("extdata", SleepDetails_fname, package = "arctools")
SleepDetails       <- fread(SleepDetails_fpath)

## Precomoute output with the use of package function implementation
SummarizePA_out_list    <- list()
SummarizePA_JU_out_list <- list()

for (i in 1:length(acc_list)){ # i <- 1

  acc            <- acc_list[[i]]
  acc_ts         <- acc_ts_list[[i]]
  wear_flag      <- wear_flag_list[[i]]
  valid_day_flag <- valid_day_flag_list[[i]]

  ## Filter to keep data correcponding to current counts data file,
  ## transform in/out bed data time dates to POSIXct objects,
  ## arrange data according to in bed time ascending
  SleepDetails_i <-
    SleepDetails %>%
    filter(`Subject Name` == paste0("ID_", i)) %>%
    mutate(`In Bed Time`  = mdy_hms(`In Bed Time`),
           `Out Bed Time` = mdy_hms(`Out Bed Time`)) %>%
    arrange(`In Bed Time`)
  ## Pull in/out bed data time
  in_bed_time  <- SleepDetails_i[, "In Bed Time"]
  out_bed_time <- SleepDetails_i[, "Out Bed Time"]

  ## MK
  out <- summarize_PA(
    acc, acc_ts, wear_flag, valid_day_flag,
    in_bed_time = in_bed_time, out_bed_time = out_bed_time)
  SummarizePA_out_list <- c(SummarizePA_out_list, list(out))

  ## JU
  out <- SummarizePA_JU_forTests(
    acc, acc_ts, wear_flag, valid_day_flag,
    in.bed.time = in_bed_time, out.bed.time = out_bed_time)
  SummarizePA_JU_out_list <- c(SummarizePA_JU_out_list, list(out))
}



test_that_desc <- paste0(
  "Compare updated implementation (@MK) with the previous implementation (@JU): ",
  "The same output values")
test_that(test_that_desc, {
  skip_on_cran()
  for (i in 1:length(SummarizePA_out_list)){ # i <- 1
    out1 <- SummarizePA_JU_out_list[[i]]
    out2 <- SummarizePA_out_list[[i]]
    expect_equal(unlist(out1), unlist(out2))
  }
})


test_that_desc <- paste0(
  "Test that the value of running the function remained unchanged.")
test_that(test_that_desc, {
  skip_on_cran()
  ## Pull all values from all the output lists
  out <- unlist(lapply(SummarizePA_out_list, function(val) unlist(val)))
  out <- out[!is.na(out)]
  ## Get statisticsl of the quantile values
  out_act <- quantile(out, probs = seq(0.1, 0.9, 0.1))
  out_exp <- c(`10%` = 0.208752344061389, `20%` = 3.21108870967742, `30%` = 6.61207592928038,
               `40%` = 11.9039273190626, `50%` = 25.5, `60%` = 83.55, `70%` = 381.075,
               `80%` = 1120.99722222222, `90%` = 5531.49928558971)
  expect_equal(out_act, out_exp, tolerance=1e-6)
  ## Get statistics of the mean value
  out_act <- mean(out)
  out_exp <- 127658.141829767
  expect_equal(out_act, out_exp, tolerance=1e-6)
})

rm(SummarizePA_out_list,
   SummarizePA_JU_out_list)



## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

context("Testing summarize_PA() for catching misspecifications of some of the arguments")

test_that_desc <- paste0(
  "Error ocurrs when include/exclude minutes arg is misspecified")
test_that(test_that_desc, {
  skip_on_cran()
  i <- 1
  acc            <- acc_list[[i]]
  acc_ts         <- acc_ts_list[[i]]
  wear_flag      <- wear_flag_list[[i]]
  valid_day_flag <- valid_day_flag_list[[i]]

  expect_error({
    summarize_PA(acc, acc_ts, valid_day_flag = valid_day_flag, wear_flag = wear_flag, exclude_minutes = c("1"))
  })
  expect_error({
    summarize_PA(acc, acc_ts, valid_day_flag = valid_day_flag, wear_flag = wear_flag, exclude_minutes = c(1.5,2))
  })
  expect_error({
    summarize_PA(acc, acc_ts, valid_day_flag = valid_day_flag, wear_flag = wear_flag, exclude_minutes = c(1441))
  })
})


test_that_desc <- paste0(
  "Error ocurrs when include/exclude minutes arg is misspecified")
test_that(test_that_desc, {
  skip_on_cran()
  i <- 1
  acc            <- acc_list[[i]]
  acc_ts         <- acc_ts_list[[i]]
  wear_flag      <- wear_flag_list[[i]]
  valid_day_flag <- valid_day_flag_list[[i]]

  SleepDetails_i <-
    SleepDetails %>%
    filter(`Subject Name` == paste0("ID_", i)) %>%
    mutate(`In Bed Time`  = mdy_hms(`In Bed Time`),
           `Out Bed Time` = mdy_hms(`Out Bed Time`)) %>%
    arrange(`In Bed Time`)
  ## Pull in/out bed data time
  in_bed_time  <- SleepDetails_i[, "In Bed Time"]
  out_bed_time <- SleepDetails_i[, "Out Bed Time"]

  expect_error({
    summarize_PA(acc, acc_ts, valid_day_flag = valid_day_flag, wear_flag = wear_flag, in_bed_time = in_bed_time)
  })
  expect_error({
    summarize_PA(acc, acc_ts, valid_day_flag = valid_day_flag, wear_flag = wear_flag, out_bed_time = out_bed_time)
  })
  expect_error({
    summarize_PA(ac, acc_ts, valid_day_flag = valid_day_flag, wear_flag = wear_flag,
                 in_bed_time = in_bed_time, out_bed_time = out_bed_time[1:(length(out_bed_time)-1)])
  })
})



