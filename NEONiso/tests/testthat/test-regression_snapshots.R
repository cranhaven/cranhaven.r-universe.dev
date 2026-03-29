# test-regression_snapshots.R
# Regression tests that compare calibrated output against known-good
# reference values. These catch any change that shifts calibrated values.
#
# Reference values were generated from the ONAQ 2019-05 test file using
# NEONiso at the time of the hdf5r migration (commit on hdf5r-migration branch).
# If calibration logic changes intentionally, update these values.

fin <- system.file("extdata",
  "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
  package = "NEONiso", mustWork = TRUE)

# ---- Carbon: gain-offset method ----

test_that("calibrate_carbon gainoffset produces stable calibration parameters", {

  suppressWarnings({
    co2_go <- calibrate_carbon(fin, tempfile(), "ONAQ",
      method = "gainoffset", calibration_half_width = 0.5,
      write_to_file = FALSE)
  })

  cal_df <- co2_go$cal_df

  # Structure checks
  expect_equal(nrow(cal_df), 30)
  expect_equal(ncol(cal_df), 14)
  expect_equal(names(cal_df),
    c("timeBgn", "timeEnd", "gain12C", "offset12C", "r2_12C",
      "cvloo_12C", "cv5mae_12C", "cv5rmse_12C",
      "gain13C", "offset13C", "r2_13C",
      "cvloo_13C", "cv5mae_13C", "cv5rmse_13C"))

  # Calibration parameter value checks (non-NA values)
  valid <- !is.na(cal_df$gain12C)
  expect_equal(sum(valid), 21) # 21 valid calibration periods
  expect_equal(mean(cal_df$gain12C, na.rm = TRUE), 1.004, tolerance = 0.001)
  expect_equal(mean(cal_df$offset12C, na.rm = TRUE), -1.6419, tolerance = 0.01)
  expect_equal(mean(cal_df$gain13C, na.rm = TRUE), 1.0017, tolerance = 0.001)
  expect_equal(mean(cal_df$offset13C, na.rm = TRUE), -0.007078, tolerance = 0.001)
})

test_that("calibrate_carbon gainoffset produces stable ambient d13C values", {

  suppressWarnings({
    co2_go <- calibrate_carbon(fin, tempfile(), "ONAQ",
      method = "gainoffset", calibration_half_width = 0.5,
      write_to_file = FALSE)
  })

  d13c <- co2_go$ciso_subset_cal$`000_010_09m`$dlta13CCo2
  expect_equal(sum(!is.na(d13c$mean_cal)), 730)
  expect_equal(mean(d13c$mean_cal, na.rm = TRUE), -9.2746, tolerance = 0.01)
  expect_equal(sd(d13c$mean_cal, na.rm = TRUE), 0.5023, tolerance = 0.01)

  co2 <- co2_go$ciso_subset_cal$`000_010_09m`$rtioMoleDryCo2
  expect_equal(sum(!is.na(co2$mean_cal)), 730)
  expect_equal(mean(co2$mean_cal, na.rm = TRUE), 418.23, tolerance = 0.1)
  expect_equal(sd(co2$mean_cal, na.rm = TRUE), 8.030, tolerance = 0.1)
})

# ---- Carbon: linear regression method ----

test_that("calibrate_carbon linreg produces stable calibration parameters", {

  co2_lr <- calibrate_carbon(fin, tempfile(), "ONAQ",
    method = "linreg", calibration_half_width = 0.5,
    write_to_file = FALSE)

  cal_df <- co2_lr$cal_df

  # Structure
  expect_equal(nrow(cal_df), 30)
  expect_equal(ncol(cal_df), 14)
  expect_equal(names(cal_df),
    c("timeBgn", "timeEnd", "d13C_slope", "d13C_intercept", "d13C_r2",
      "d13C_cvloo", "d13C_cv5mae", "d13C_cv5rmse",
      "co2_slope", "co2_intercept", "co2_r2",
      "co2_cvloo", "co2_cv5mae", "co2_cv5rmse"))

  # Parameter values
  valid <- !is.na(cal_df$d13C_slope)
  expect_equal(sum(valid), 21)
  expect_equal(mean(cal_df$d13C_slope, na.rm = TRUE), 1.0842, tolerance = 0.001)
  expect_equal(mean(cal_df$d13C_intercept, na.rm = TRUE), 0.8199, tolerance = 0.01)
  expect_equal(mean(cal_df$co2_slope, na.rm = TRUE), 1.004, tolerance = 0.001)
  expect_equal(mean(cal_df$co2_intercept, na.rm = TRUE), -1.6571, tolerance = 0.01)
})

test_that("calibrate_carbon linreg produces stable ambient d13C values", {

  co2_lr <- calibrate_carbon(fin, tempfile(), "ONAQ",
    method = "linreg", calibration_half_width = 0.5,
    write_to_file = FALSE)

  d13c <- co2_lr$ciso_subset_cal$`000_010_09m`$dlta13CCo2
  expect_equal(sum(!is.na(d13c$mean_cal)), 731)
  expect_equal(mean(d13c$mean_cal, na.rm = TRUE), -9.3093, tolerance = 0.01)
  expect_equal(sd(d13c$mean_cal, na.rm = TRUE), 0.4585, tolerance = 0.01)

  co2 <- co2_lr$ciso_subset_cal$`000_010_09m`$rtioMoleDryCo2
  expect_equal(sum(!is.na(co2$mean_cal)), 669)
  expect_equal(mean(co2$mean_cal, na.rm = TRUE), 417.07, tolerance = 0.1)
  expect_equal(sd(co2$mean_cal, na.rm = TRUE), 6.878, tolerance = 0.1)
})

# ---- Water ----

test_that("calibrate_water produces stable calibration parameters", {

  h2o <- calibrate_water(fin, tempfile(), "ONAQ", write_to_file = FALSE)

  cal_df <- h2o$cal_df

  # Structure
  expect_equal(nrow(cal_df), 22)
  expect_equal(ncol(cal_df), 14)
  expect_equal(names(cal_df),
    c("timeBgn", "timeEnd", "slope18O", "intercept18O", "r2_18O",
      "cvloo_18O", "cv5mae_18O", "cv5rmse_18O",
      "slope2H", "intercept2H", "r2_2H",
      "cvloo_2H", "cv5mae_2H", "cv5rmse_2H"))

  # Parameter values
  expect_equal(mean(cal_df$slope18O), 1.021, tolerance = 0.001)
  expect_equal(mean(cal_df$intercept18O), 0.03455, tolerance = 0.001)
  expect_equal(mean(cal_df$slope2H), 1.031, tolerance = 0.001)
  expect_equal(mean(cal_df$intercept2H), 3.582, tolerance = 0.01)
})

test_that("calibrate_water produces stable ambient isotope values", {

  h2o <- calibrate_water(fin, tempfile(), "ONAQ", write_to_file = FALSE)

  d18o <- h2o$wiso_subset_cal$`000_010_09m`$dlta18OH2o
  expect_equal(sum(!is.na(d18o$mean_cal)), 915)
  expect_equal(mean(d18o$mean_cal, na.rm = TRUE), -22.988, tolerance = 0.05)
  expect_equal(sd(d18o$mean_cal, na.rm = TRUE), 4.431, tolerance = 0.05)

  d2h <- h2o$wiso_subset_cal$`000_010_09m`$dlta2HH2o
  expect_equal(sum(!is.na(d2h$mean_cal)), 848)
  expect_equal(mean(d2h$mean_cal, na.rm = TRUE), -176.16, tolerance = 0.5)
  expect_equal(sd(d2h$mean_cal, na.rm = TRUE), 32.72, tolerance = 0.5)
})
