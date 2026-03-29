# test-data_regression

#----------------------
# CARBON FUNCTIONS
#----------------------

# test to make sure that the I/O structures
# across regression functions are correct.
fin <- system.file("extdata",
                   "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
                   package = "NEONiso", mustWork = TRUE)
co2test <- ingest_data(fin, analyte = "Co2", amb_avg = 9, ref_avg = 9)
co2data <- NEONiso:::extract_carbon_cal_data(co2test$refe_stacked,
                                             standards = c("co2Low",
                                                           "co2Med",
                                                           "co2High"))
caldf_b03 <- NEONiso:::fit_carbon_regression(co2data,
                                             method = "Bowling_2003",
                                             calibration_half_width = 2)
caldf_lr <- NEONiso:::fit_carbon_regression(co2data,
                                            method = "linreg",
                                            calibration_half_width = 2)

test_that("fit_carbon_regression returns data.frame", {

  expect_s3_class(fit_carbon_regression(co2data,
                                        method = "Bowling_2003",
                                        calibration_half_width = 2),
                  "data.frame")
  expect_s3_class(fit_carbon_regression(co2data,
                                        method = "linreg",
                                        calibration_half_width = 2),
                  "data.frame")
})

test_that("calibration data frames have 14 columns", {

  expect_equal(ncol(caldf_b03), 14)
  expect_equal(ncol(caldf_lr), 14)
})

test_that("carbon calibration data frames correct even when no input data", {

  # empty ref data frame:
  empty_df <- data.frame(matrix(nrow = 0, ncol = ncol(co2data)))
  names(empty_df) <- names(co2data)
  expect_no_error(fit_carbon_regression(empty_df,
                                        method = "Bowling_2003"))
  expect_no_error(fit_carbon_regression(empty_df,
                                        method = "linreg"))
  expect_equal(ncol(fit_carbon_regression(empty_df,
                                          method = "Bowling_2003")), 14)
  expect_equal(ncol(fit_carbon_regression(empty_df,
                                          method = "linreg",
                                          calibration_half_width = 2)), 14)
})


# work through ambient calibrations

# test carbon - bowling ambient calibration
temp <- calibrate_ambient_carbon_gainoffset(co2test$ambient$`000_010_09m`,
                                           caldf_b03,
                                           site = "ONAQ")
temp_gf <- calibrate_ambient_carbon_gainoffset(co2test$ambient$`000_010_09m`,
                                               caldf_b03,
                                               site = "ONAQ",
                                               gap_fill_parameters = TRUE)

test_that("calibrate_ambient_carbon_Bowling2003 returns a list",{

  expect_equal(class(temp), "list")
  expect_equal(class(temp_gf), "list")
})

test_that("calibrate_ambient_carbon_Bowling2003 returns correct variables", {

  vars <- c("dlta13CCo2", "pres", "presEnvHut", "rhEnvHut",
            "rtioMoleDry12CCo2", "rtioMoleDry13CCo2", "rtioMoleDryCo2",
            "rtioMoleDryH2o", "rtioMoleWet12CCo2", "rtioMoleWet13CCo2",
            "rtioMoleWetCo2", "rtioMoleWetH2o",
            "rtioMoleWetH2oEnvHut", "temp", "tempEnvHut")
  expect_equal(names(temp), vars)
  expect_equal(names(temp_gf), vars)
})

test_that("calibrated d13C values have been added to calibrate_ambient_cabron_Bowling2003 output", {

  expect_gt(ncol(temp$dlta13CCo2),
            ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
  expect_gt(ncol(temp_gf$dlta13CCo2),
            ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
})

# test carbon - linreg ambient calibration
temp <- calibrate_ambient_carbon_linreg(co2test$ambient$`000_010_09m`,
                                        caldf_lr,
                                        site = "ONAQ")
temp_gf <- calibrate_ambient_carbon_linreg(co2test$ambient$`000_010_09m`,
                                           caldf_lr,
                                           site = "ONAQ",
                                           gap_fill_parameters = TRUE)

test_that("calibrate_ambient_carbon_linreg returns a list", {

  expect_equal(class(temp), "list")
  expect_equal(class(temp_gf), "list")
})

test_that("calibrate_ambient_carbon_linreg returns correct variables", {

  vars <- c("dlta13CCo2", "pres", "presEnvHut", "rhEnvHut",
            "rtioMoleDry12CCo2", "rtioMoleDry13CCo2", "rtioMoleDryCo2",
            "rtioMoleDryH2o", "rtioMoleWet12CCo2", "rtioMoleWet13CCo2",
            "rtioMoleWetCo2", "rtioMoleWetH2o",
            "rtioMoleWetH2oEnvHut", "temp", "tempEnvHut")
  expect_equal(names(temp), vars)
  expect_equal(names(temp_gf), vars)
})

test_that("calibrated d13C values have been added to calibrate_ambient_cabron_linreg output", {

  expect_gt(ncol(temp$dlta13CCo2),
            ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
  expect_gt(ncol(temp_gf$dlta13CCo2),
            ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
})


#----------------------
# WATER FUNCTIONS
#----------------------

# test to make sure that the I/O structures across regression functions are correct.
fin <- system.file("extdata",
                   "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
                   package = "NEONiso", mustWork = TRUE)
h2otest <- ingest_data(fin, analyte = "H2o", amb_avg = 9, ref_avg = 3)
h2odata <- NEONiso:::extract_water_calibration_data(h2otest$refe_stacked)

caldf <- NEONiso:::fit_water_regression(h2odata,
                                        calibration_half_width = 2)

test_that("fit_water_regression returns data.frame", {

  expect_s3_class(fit_water_regression(h2odata,
                                       calibration_half_width = 2),
                  "data.frame")
})

test_that("calibration data frames have 14 columns", {

  expect_equal(ncol(caldf), 14)
})


# work through ambient calibrations

# test carbon - bowling ambient calibration
temp <- calibrate_ambient_water_linreg(h2otest$ambient$`000_010_09m`,
                                       caldf,
                                       site = "ONAQ")
# no gapfilling in the water functions yet!
#temp_gf <- calibrate_ambient_water_linreg(h2otest$ambient$`000_010_09m`,
#                                               calDf,
#                                               site = "ONAQ",
#                                               gap_fill_parameters = TRUE)

test_that("calibrate_ambient_water_linreg returns a list", {

  str(temp)
  expect_equal(class(temp), "list")
  #expect_equal(class(temp_gf), "list")
})

test_that("calibrate_ambient_water_linreg returns correct variables", {

  vars <- c("dlta18OH2o", "dlta2HH2o", "pres", "presEnvHut", "rhEnvHut",
            "rtioMoleDryH2o", "rtioMoleWetH2o",
            "rtioMoleWetH2oEnvHut", "temp", "tempEnvHut")
  expect_equal(names(temp), vars)
 #expect_equal(names(temp_gf), vars)
})

test_that("calibrated water isotope values have been added to calibrate_ambient_water_linreg output", {

  expect_gt(ncol(temp$dlta18OH2o),
            ncol(h2otest$ambient$`000_010_09m`$dlta18OH2o))
  expect_gt(ncol(temp$dlta2HH2o),
            ncol(h2otest$ambient$`000_010_09m`$dlta2HH2o))
})


#-----------------------------------------
# GAP-FILL PARAMETER TESTS
#-----------------------------------------

# Test gap_fill_parameters = TRUE with bad r2 values for gainoffset
test_that("gap-fill carries forward good calibration parameters (gainoffset)", {

  # Make a copy and set some r2 values below threshold
  caldf_bad <- caldf_b03
  if (nrow(caldf_bad) >= 3) {
    # Set period 2 r2 below threshold to trigger carry-forward
    caldf_bad$r2_12C[2] <- 0.1
    caldf_bad$r2_13C[2] <- 0.1
  }

  result <- calibrate_ambient_carbon_gainoffset(co2test$ambient$`000_010_09m`,
                                                 caldf_bad,
                                                 site = "ONAQ",
                                                 gap_fill_parameters = TRUE)
  expect_equal(class(result), "list")
  expect_true("dlta13CCo2" %in% names(result))
})

test_that("gap-fill handles bad first calibration period (gainoffset)", {

  # Set the first period's r2 below threshold
  caldf_bad <- caldf_b03
  if (nrow(caldf_bad) >= 2 && any(!is.na(caldf_bad$r2_12C) &
                                    caldf_bad$r2_12C > 0.9)) {
    caldf_bad$r2_12C[1] <- 0.1
    caldf_bad$r2_13C[1] <- 0.1

    result <- calibrate_ambient_carbon_gainoffset(co2test$ambient$`000_010_09m`,
                                                   caldf_bad,
                                                   site = "ONAQ",
                                                   gap_fill_parameters = TRUE)
    expect_equal(class(result), "list")
    expect_true("dlta13CCo2" %in% names(result))
  }
})

# Test gap_fill_parameters = TRUE with bad r2 values for linreg
test_that("gap-fill carries forward good calibration parameters (linreg)", {

  caldf_bad <- caldf_lr
  if (nrow(caldf_bad) >= 3) {
    caldf_bad$d13C_r2[2] <- 0.1
    caldf_bad$co2_r2[2] <- 0.1
  }

  result <- calibrate_ambient_carbon_linreg(co2test$ambient$`000_010_09m`,
                                             caldf_bad,
                                             site = "ONAQ",
                                             gap_fill_parameters = TRUE)
  expect_equal(class(result), "list")
  expect_true("dlta13CCo2" %in% names(result))
})

test_that("gap-fill handles bad first calibration period (linreg)", {

  caldf_bad <- caldf_lr
  if (nrow(caldf_bad) >= 2 && any(!is.na(caldf_bad$d13C_r2) &
                                    caldf_bad$d13C_r2 > 0.9)) {
    caldf_bad$d13C_r2[1] <- 0.1
    caldf_bad$co2_r2[1] <- 0.1

    result <- calibrate_ambient_carbon_linreg(co2test$ambient$`000_010_09m`,
                                               caldf_bad,
                                               site = "ONAQ",
                                               gap_fill_parameters = TRUE)
    expect_equal(class(result), "list")
    expect_true("dlta13CCo2" %in% names(result))
  }
})

#-----------------------------------------
# SETEQUAL BRANCH TESTS (mismatched timestamps)
#-----------------------------------------

test_that("gainoffset handles mismatched ambient timestamps", {

  # Create amb_data_list with an extra row in dlta13CCo2
  amb_mismatch <- co2test$ambient$`000_010_09m`
  extra_row <- amb_mismatch$dlta13CCo2[1, , drop = FALSE]
  extra_row$timeBgn <- "2099-01-01T00:00:00.000Z"
  extra_row$timeEnd <- "2099-01-01T00:09:00.000Z"
  amb_mismatch$dlta13CCo2 <- rbind(amb_mismatch$dlta13CCo2, extra_row)

  result <- calibrate_ambient_carbon_gainoffset(amb_mismatch,
                                                 caldf_b03,
                                                 site = "ONAQ")
  expect_equal(class(result), "list")
  # After alignment, row counts should be consistent
  expect_equal(nrow(result$dlta13CCo2), nrow(result$rtioMoleDryCo2))
})

test_that("linreg handles mismatched ambient timestamps", {

  amb_mismatch <- co2test$ambient$`000_010_09m`
  extra_row <- amb_mismatch$dlta13CCo2[1, , drop = FALSE]
  extra_row$timeBgn <- "2099-01-01T00:00:00.000Z"
  extra_row$timeEnd <- "2099-01-01T00:09:00.000Z"
  amb_mismatch$dlta13CCo2 <- rbind(amb_mismatch$dlta13CCo2, extra_row)

  result <- calibrate_ambient_carbon_linreg(amb_mismatch,
                                             caldf_lr,
                                             site = "ONAQ")
  expect_equal(class(result), "list")
  expect_equal(nrow(result$dlta13CCo2), nrow(result$rtioMoleDryCo2))
})

#-----------------------------------------
# WATER HYDROGEN R2 ELSE BRANCH
#-----------------------------------------

test_that("water calibration sets NA when r2_2H is below threshold", {

  # Set all r2_2H values below threshold
  caldf_bad_h <- caldf
  caldf_bad_h$r2_2H <- 0.1

  result <- calibrate_ambient_water_linreg(h2otest$ambient$`000_010_09m`,
                                            caldf_bad_h,
                                            site = "ONAQ")
  # Calibrated hydrogen values in calibration periods should be NA
  expect_true(any(is.na(result$dlta2HH2o$mean_cal)))
  expect_true(any(is.na(result$dlta2HH2o$min_cal)))
  expect_true(any(is.na(result$dlta2HH2o$max_cal)))
})

test_that("water calibration sets NA when r2_18O is below threshold", {

  caldf_bad_o <- caldf
  caldf_bad_o$r2_18O <- 0.1

  result <- calibrate_ambient_water_linreg(h2otest$ambient$`000_010_09m`,
                                            caldf_bad_o,
                                            site = "ONAQ")
  expect_true(any(is.na(result$dlta18OH2o$mean_cal)))
  expect_true(any(is.na(result$dlta18OH2o$min_cal)))
  expect_true(any(is.na(result$dlta18OH2o$max_cal)))
})
