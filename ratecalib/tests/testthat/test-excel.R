# Tests for the Excel input/output helpers. These exercise real round-trips
# through .xlsx files, so they require openxlsx.

skip_if_not_installed("openxlsx")

tmp_xlsx <- function() tempfile(fileext = ".xlsx")

test_that("read_calibration_data reads a sample-data sheet back as a data frame", {
  d <- data.frame(g = c("a", "b", "a"), y = c(1L, 0L, 1L), w = c(1, 2, 1.5),
                  stringsAsFactors = FALSE)
  path <- tmp_xlsx()
  openxlsx::write.xlsx(d, path)
  got <- read_calibration_data(path)
  expect_s3_class(got, "data.frame")
  expect_equal(nrow(got), 3)
  expect_true(all(c("g", "y", "w") %in% names(got)))
})

test_that("read_targets_xlsx reads a canonical target sheet", {
  tg <- data.frame(variable = c(".overall", "sex", "sex"),
                   level = c(".all", "M", "F"),
                   target_rate = c(0.7, 0.71, 0.69),
                   stringsAsFactors = FALSE)
  path <- tmp_xlsx()
  openxlsx::write.xlsx(tg, path)
  got <- read_targets_xlsx(path)
  expect_true(all(c("variable", "level", "target_rate") %in% names(got)))
  expect_equal(got$target_rate, c(0.7, 0.71, 0.69))
})

test_that("read_targets_xlsx tolerates alternative English headers", {
  tg <- data.frame(var = c("sex", "sex"), category = c("M", "F"),
                   target = c(0.71, 0.69), stringsAsFactors = FALSE)
  path <- tmp_xlsx()
  openxlsx::write.xlsx(tg, path)
  got <- read_targets_xlsx(path)
  expect_equal(sort(names(got)[1:3]), c("level", "target_rate", "variable"))
  expect_equal(got$variable, c("sex", "sex"))
})

test_that("read_targets_xlsx maps Chinese headers", {
  # headers variable / level / target_rate in Chinese, built from code points
  # so this test source stays ASCII.
  tg <- data.frame(a = c("sex"), b = c("M"), c = c(0.71), stringsAsFactors = FALSE)
  names(tg) <- c(intToUtf8(c(0x53d8, 0x91cf)),          # variable
                 intToUtf8(c(0x7c7b, 0x522b)),          # level
                 intToUtf8(c(0x76ee, 0x6807, 0x7387)))  # target_rate
  path <- tmp_xlsx()
  openxlsx::write.xlsx(tg, path)
  got <- read_targets_xlsx(path)
  expect_true(all(c("variable", "level", "target_rate") %in% names(got)))
  expect_equal(got$variable, "sex")
  expect_equal(got$target_rate, 0.71)
})

test_that("read_targets_xlsx errors when a required column is missing", {
  tg <- data.frame(variable = "sex", level = "M", stringsAsFactors = FALSE)
  path <- tmp_xlsx()
  openxlsx::write.xlsx(tg, path)
  expect_error(read_targets_xlsx(path), "target_rate")
})

test_that("export_calibration_xlsx writes a multi-sheet workbook that reads back", {
  d <- example_rate_data(n = 600, seed = 3)
  fit <- calibrate_rates(d, "qualified", "initial_weight",
                         overall = 0.7, groups = list(sex = c(M = 0.71, F = 0.69)),
                         mode = "soft")
  path <- tmp_xlsx()
  export_calibration_xlsx(fit, path)
  expect_true(file.exists(path))
  sheets <- openxlsx::getSheetNames(path)
  expect_true(all(c("data", "target_check", "margin_check", "diagnostics", "settings") %in% sheets))
  back <- openxlsx::read.xlsx(path, sheet = "data")
  expect_true("weight_calibrated" %in% names(back))
  expect_equal(nrow(back), nrow(d))
})

test_that("calibrate_from_excel reads data + targets and solves end to end", {
  d <- example_rate_data(n = 600, seed = 4)
  tg <- make_rate_targets(overall = 0.7, groups = list(sex = c(M = 0.71, F = 0.69)))
  path <- tmp_xlsx()
  openxlsx::write.xlsx(list(data = d, targets = tg), path)
  fit <- calibrate_from_excel(path, outcome = "qualified", weight = "initial_weight",
                              data_sheet = "data", targets_sheet = "targets",
                              mode = "soft")
  expect_s3_class(fit, "pass_rate_calibration")
  expect_true("weight_calibrated" %in% names(fit$data))
  # group_vars were inferred from the targets table
  expect_true("sex" %in% fit$settings$group_vars)
})
