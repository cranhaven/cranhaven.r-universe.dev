# test-output_values.R

# need to add some tests to validate output data,
# as there are clearly some issues
# remaining with calibration - trying to figure out why

fin <- system.file("extdata",
                   "NEON.D12.YELL.DP4.00200.001.nsae.2020-11.basic.packed.h5",
                   package = "NEONiso", mustWork = TRUE)

co2test <- NEONiso:::ingest_data(fin, analyte = "Co2", ref_avg = 9, amb_avg = 9)
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

ciso_subset <- co2test$ambient

ciso_subset_cal <- lapply(names(ciso_subset),
                          function(x) {
                            calibrate_ambient_carbon_gainoffset(amb_data_list = ciso_subset[[x]],
                                                                caldf = caldf_b03,
                                                                site = "YELL",
                                                                filter_data = TRUE,
                                                                force_to_end = TRUE,
                                                                force_to_beginning = TRUE,
                                                                r2_thres = 0.9)
                          })

names(ciso_subset_cal) <- names(ciso_subset)

test_that("calibrated ambient co2 delta values in B03 are within a plausible range", {
  expect_lt(max(ciso_subset_cal$`000_010_09m`$dlta13CCo2$mean_cal,
                na.rm = TRUE),
            10)
  expect_gt(min(ciso_subset_cal$`000_010_09m`$dlta13CCo2$mean_cal,
                na.rm = TRUE),
            -50)
})

test_that("calibrated ambient co2 mixing ratios in B03 are within a plausible range", {
  expect_lt(max(ciso_subset_cal$`000_010_09m`$rtioMoleDryCo2$mean_cal,
                na.rm = TRUE),
            1500)
  expect_gt(min(ciso_subset_cal$`000_010_09m`$rtioMoleDryCo2$mean_cal,
                na.rm = TRUE),
            300)
})

ciso_subset_cal <- lapply(names(ciso_subset),
                          function(x) {
                            calibrate_ambient_carbon_linreg(amb_data_list = ciso_subset[[x]],
                                                            caldf = caldf_lr,
                                                            site = "YELL",
                                                            filter_data = TRUE,
                                                            force_to_end = TRUE,
                                                            force_to_beginning = TRUE,
                                                            r2_thres = 0.9)
                          })

names(ciso_subset_cal) <- names(ciso_subset)

test_that("calibrated carbon values linreg are within a plausible range", {
  expect_lt(max(ciso_subset_cal$`000_010_09m`$dlta13CCo2$mean_cal,
                na.rm = TRUE),
            10)
  expect_gt(min(ciso_subset_cal$`000_010_09m`$dlta13CCo2$mean_cal,
                na.rm = TRUE),
            -50)
})

test_that("calibrated ambient co2 mixing ratios linreg are within a plausible range", {
  expect_lt(max(ciso_subset_cal$`000_010_09m`$rtioMoleDryCo2$mean_cal,
                na.rm = TRUE),
            1500)
  expect_gt(min(ciso_subset_cal$`000_010_09m`$rtioMoleDryCo2$mean_cal,
                na.rm = TRUE),
            300)
})


#---------------------------------------
# test some of the output data functions

fout <- tempfile()

test_that("setup_output_file returns no errors", {
  expect_no_error(setup_output_file(fin,
                                    fout,
                                    site = "YELL",
                                    analyte = "H2o"))
  expect_no_error(setup_output_file(fin,
                                    fout,
                                    site = "YELL",
                                    analyte = "Co2"))
})
