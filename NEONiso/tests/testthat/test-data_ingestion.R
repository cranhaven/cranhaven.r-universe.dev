# test-data_ingestion

#-----------------------------
# test validate analyte function
test_that("validate_analyte corrects case of analytes", {
  expect_equal(validate_analyte("h2o"), "H2o")
  expect_equal(validate_analyte("co2"), "Co2")
})

test_that("validate_analyte errors if invalid species is given", {
  expect_error(validate_analyte("CH4"))
})


fin <- system.file("extdata",
                   "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
                   package = "NEONiso", mustWork = TRUE)

#------------------------------------------
# various tests about output structure of ingest_data.

# a) get data
co2test <- ingest_data(fin, analyte = "Co2", amb_avg = 9, ref_avg = 9)
h2otest <- ingest_data(fin, analyte = "H2o", amb_avg = 9, ref_avg = 3)

# b) that a list is returned
test_that("ingest_data returns a list for analyte='Co2'", {
  expect_type(co2test, "list")
})
test_that("ingest_data returns a list for analyte='H2o'", {
  expect_type(h2otest, "list")
})

# c) that list contains appropriate list of lists
test_that("ingest_data returns list with correct sublist names - co2", {
  expect_equal(names(co2test), c("ambient", "reference", "refe_stacked", "attrs"))
})
test_that("ingest_data returns list with correct sublist names - h2o", {
  expect_equal(names(h2otest), c("ambient", "reference", "refe_stacked", "attrs"))
})

#-----------------------------------------------------------------------
# test restructure_carbon_variables function
rest_raw <- neonUtilities::stackEddy(fin, avg = 9, level = "dp01")[[1]] %>%
  dplyr::select("verticalPosition", "timeBgn",
                "timeEnd", tidyselect::contains("isoCo2"))

# Needed to remove merge error where times are
# slightly different between h2o and co2.
rest_raw <- rest_raw[rowSums(is.na(rest_raw)) < 145, ]

test_that("restructure_carbon_variables errors when invalid mode provided", {
  expect_error(restructure_carbon_variables(rest_raw,
                                            "dlta13CCo2",
                                            mode = "cheese",
                                            group = "data"))
  expect_silent(restructure_carbon_variables(rest_raw,
                                             "dlta13CCo2",
                                             mode = "reference",
                                             group = "data"))
  expect_silent(restructure_carbon_variables(rest_raw,
                                             "dlta13CCo2",
                                             mode = "ambient",
                                             group = "data"))
})

test_that("output structure from restructure_carbon_variables is correct", {
  expect_true(is.data.frame(restructure_carbon_variables(rest_raw,
                                                         "dlta13CCo2",
                                                         mode = "reference",
                                                         group = "data")))
  expect_equal(ncol(restructure_carbon_variables(rest_raw,
                                                 "dlta13CCo2",
                                                 mode = "reference",
                                                 group = "data")), 7)
  expect_equal(ncol(restructure_carbon_variables(rest_raw,
                                                 "dlta13CCo2",
                                                 mode = "ambient",
                                                 group = "data")), 7)
})

test_that("output datetimes from restructure_carbon_variables are characters, not POSIX", {
  tmp <- restructure_carbon_variables(rest_raw,
                                      "dlta13CCo2",
                                      mode = "ambient",
                                      group = "data")
  expect_type(tmp$timeBgn, "character")
  expect_type(tmp$timeEnd, "character")
  tmp <- restructure_carbon_variables(rest_raw,
                                      "dlta13CCo2",
                                      mode = "reference",
                                      group = "data")
  expect_type(tmp$timeBgn, "character")
  expect_type(tmp$timeEnd, "character")
})

#--------------------------------------------------
fin1 <- system.file("extdata",
                    "NEON.D15.ONAQ.DP4.00200.001.nsae.2020-06-01.basic.packed.h5",
                    package = "NEONiso", mustWork = TRUE)
fin2 <- system.file("extdata",
                    "NEON.D15.ONAQ.DP4.00200.001.nsae.2020-06-02.basic.packed.h5",
                    package = "NEONiso", mustWork = TRUE)
fin3 <- system.file("extdata",
                    "NEON.D15.ONAQ.DP4.00200.001.nsae.2020-06-03.basic.packed.h5",
                    package = "NEONiso", mustWork = TRUE)

# create list of files
all_files <- c(fin1, fin2, fin3)

test1 <- ingest_data(fin1, analyte = "Co2", amb_avg = 9, ref_avg = 9)
test2 <- ingest_data(all_files, analyte = "Co2", amb_avg = 9, ref_avg = 9)

test_that("stackEddy/ingest data works on multiple daily files", {
  # would expect number of rows to be larger in test2 than test 1:
  expect_gt(nrow(test2$ambient$`000_010_09m`$dlta13CCo2),
            nrow(test1$ambient$`000_010_09m`$dlta13CCo2))
})

#--------------------------------------------------
# test restructure_carbon_variables with qfqm and ucrt groups

#--------------------------------------------------
# test restructure_water_variables with data group

rest_h2o <- neonUtilities::stackEddy(fin, avg = 3, level = "dp01")[[1]] %>%
  dplyr::select("verticalPosition", "timeBgn",
                "timeEnd", tidyselect::contains("isoH2o"))
rest_h2o <- rest_h2o[rowSums(is.na(rest_h2o)) < 145, ]

test_that("restructure_water_variables works for reference data group", {
  result <- restructure_water_variables(rest_h2o,
                                         "dlta18OH2o",
                                         mode = "reference",
                                         group = "data")
  expect_true(is.data.frame(result))
  expect_true("mean" %in% names(result))
  expect_true("varname" %in% names(result))
  expect_type(result$timeBgn, "character")
})

test_that("restructure_water_variables works for d2H reference data group", {
  result <- restructure_water_variables(rest_h2o,
                                         "dlta2HH2o",
                                         mode = "reference",
                                         group = "data")
  expect_true(is.data.frame(result))
  expect_true("mean" %in% names(result))
  expect_equal(unique(result$varname), "dlta2HH2o")
})

test_that("restructure_water_variables works for ambient mode", {
  result <- restructure_water_variables(rest_h2o,
                                         "dlta18OH2o",
                                         mode = "ambient",
                                         group = "data")
  expect_true(is.data.frame(result))
  expect_true("mean" %in% names(result))
  expect_true("varname" %in% names(result))
  expect_type(result$timeBgn, "character")
})

test_that("restructure_water_variables errors with invalid mode", {
  expect_error(restructure_water_variables(rest_h2o,
                                            "dlta18OH2o",
                                            mode = "cheese",
                                            group = "data"))
})

test_that("restructure_water_variables errors with wrong input type", {
  expect_error(restructure_water_variables(list(),
                                            "dlta18OH2o",
                                            mode = "reference",
                                            group = "data"))
  expect_error(restructure_water_variables(data.frame(),
                                            "dlta18OH2o",
                                            mode = "ambient",
                                            group = "data"))
})
