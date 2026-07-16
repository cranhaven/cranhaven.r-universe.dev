fit_example <- function() {
  skip_if_not_installed("osqp")
  set.seed(2026)
  d <- example_rate_data(n = 1500)
  calibrate_rates(
    d, outcome = "qualified", weight = "initial_weight",
    overall = 0.7,
    groups = list(sex = c(M = 0.71, F = 0.69)),
    mode = "soft"
  )
}

test_that("example_rate_data has the documented structure and ASCII levels", {
  d <- example_rate_data(n = 200)
  expect_named(
    d,
    c("sex", "residence", "education5", "age5", "qualified", "initial_weight")
  )
  expect_setequal(unique(d$sex), c("M", "F"))
  expect_setequal(unique(d$residence), c("Urban", "Rural"))
  expect_true(all(d$qualified %in% c(0, 1)))
  expect_true(all(d$initial_weight > 0))
})

test_that("weights() returns the calibrated weight vector", {
  fit <- fit_example()
  w <- weights(fit)
  expect_type(w, "double")
  expect_length(w, nrow(fit$data))
  expect_identical(w, fit$data[[fit$settings$new_weight]])
  expect_true(all(w > 0))
})

test_that("weights() honours a custom new_weight column name", {
  d <- example_rate_data(n = 600, seed = 2)
  fit <- calibrate_rates(d, "qualified", "initial_weight",
                         groups = list(sex = c(M = 0.7, F = 0.66)),
                         mode = "soft", new_weight = "cal_w")
  expect_identical(weights(fit), fit$data$cal_w)
})

test_that("as.data.frame() returns the data with the calibrated weight column", {
  fit <- fit_example()
  df <- as.data.frame(fit)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), nrow(fit$data))
  expect_true(fit$settings$new_weight %in% names(df))
})

test_that("print returns the object invisibly and emits output", {
  fit <- fit_example()
  out <- utils::capture.output(res <- print(fit))
  expect_identical(res, fit)
  expect_true(any(nzchar(out)))
  expect_true(any(grepl("calibration result", out)))
})

test_that("summary produces a summary object that prints", {
  fit <- fit_example()
  s <- summary(fit, top = 3)
  expect_s3_class(s, "summary_pass_rate_calibration")
  out <- utils::capture.output(print(s))
  expect_true(any(grepl("summary", out, ignore.case = TRUE)))
  expect_lte(nrow(s$largest_target_errors), 3)
})

test_that("calibration_diagnostics returns sorted target diagnostics", {
  fit <- fit_example()
  diag <- calibration_diagnostics(fit)
  expect_named(diag, c("targets", "margins", "weights"))
  errs <- diag$targets$abs_error
  expect_equal(errs, sort(errs, decreasing = TRUE))
  expect_error(calibration_diagnostics(list()), "pass_rate_calibration object")
})

test_that("plot runs for both diagnostic types without error", {
  fit <- fit_example()
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  expect_silent(plot(fit, type = "target_error"))
  expect_silent(plot(fit, type = "multipliers"))
})

test_that("print.ratecalib_check emits status output", {
  d <- example_rate_data(n = 300)
  report <- check_calibration_data(
    d, "qualified", "initial_weight", c("sex", "residence")
  )
  out <- utils::capture.output(print(report))
  expect_true(any(grepl("pre-calibration check", out)))
})
