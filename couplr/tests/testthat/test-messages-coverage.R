# ==============================================================================
# Coverage tests for matching_messages.R and internal functions
# ==============================================================================

# ------------------------------------------------------------------------------
# Message helpers
# ------------------------------------------------------------------------------

test_that("use_emoji respects option", {
  skip_on_cran()
  old_opt <- getOption("couplr.emoji")
  on.exit(options(couplr.emoji = old_opt), add = TRUE)

  options(couplr.emoji = FALSE)
  expect_false(couplr:::use_emoji())
})

test_that("couplr_emoji returns emoji or empty string", {
  skip_on_cran()
  old_opt <- getOption("couplr.emoji")
  on.exit(options(couplr.emoji = old_opt), add = TRUE)

  options(couplr.emoji = TRUE)
  # Emoji output depends on interactive(), just verify no error
  expect_no_error(couplr:::couplr_emoji("error"))
  expect_no_error(couplr:::couplr_emoji("warning"))
  expect_no_error(couplr:::couplr_emoji("info"))
  expect_no_error(couplr:::couplr_emoji("success"))
  expect_no_error(couplr:::couplr_emoji("heart"))
  expect_no_error(couplr:::couplr_emoji("broken"))
  expect_no_error(couplr:::couplr_emoji("sparkles"))
  expect_no_error(couplr:::couplr_emoji("search"))
  expect_no_error(couplr:::couplr_emoji("chart"))
  expect_no_error(couplr:::couplr_emoji("warning_sign"))
  expect_no_error(couplr:::couplr_emoji("stop"))
  expect_no_error(couplr:::couplr_emoji("check"))
})

test_that("couplr_emoji returns empty when disabled", {
  skip_on_cran()
  old_opt <- getOption("couplr.emoji")
  on.exit(options(couplr.emoji = old_opt), add = TRUE)

  options(couplr.emoji = FALSE)
  expect_equal(couplr:::couplr_emoji("error"), "")
})

# ------------------------------------------------------------------------------
# Error message functions
# ------------------------------------------------------------------------------

test_that("err_missing_data throws error", {
  skip_on_cran()
  expect_error(couplr:::err_missing_data("left"), "left dataset is empty")
  expect_error(couplr:::err_missing_data("right"), "right dataset is empty")
})

test_that("err_missing_vars throws error", {
  skip_on_cran()
  expect_error(
    couplr:::err_missing_vars(c("x", "y"), "left"),
    "Missing variables in left"
  )
})

test_that("err_invalid_param throws error", {
  skip_on_cran()
  expect_error(
    couplr:::err_invalid_param("method", "invalid", "one of: jv, hungarian"),
    "invalid value"
  )
})

test_that("err_no_valid_pairs throws error", {
  skip_on_cran()
  expect_error(couplr:::err_no_valid_pairs(), "No valid pairs")
  expect_error(couplr:::err_no_valid_pairs("test reason"), "Reason")
})

# ------------------------------------------------------------------------------
# Warning message functions
# ------------------------------------------------------------------------------

test_that("warn_constant_var produces warning", {
  skip_on_cran()
  expect_warning(couplr:::warn_constant_var("x"), "constant")
})

test_that("warn_many_zeros produces warning", {
  skip_on_cran()
  expect_warning(couplr:::warn_many_zeros(50.0, 100), "50.0%.*zero")
})

test_that("warn_extreme_costs produces warning", {
  skip_on_cran()
  expect_warning(
    couplr:::warn_extreme_costs(10, 100, 10),
    "highly skewed"
  )
  expect_warning(
    couplr:::warn_extreme_costs(10, 100, 10, problem_vars = c("x", "y")),
    "extreme values"
  )
})

test_that("warn_many_forbidden produces warning at different severity levels", {
  skip_on_cran()
  expect_warning(couplr:::warn_many_forbidden(95, 10, 100), "critical")
  expect_warning(couplr:::warn_many_forbidden(80, 10, 100), "concerning")
  expect_warning(couplr:::warn_many_forbidden(60, 10, 100), "moderate")
})

test_that("warn_constant_distance produces warning", {
  skip_on_cran()
  expect_warning(couplr:::warn_constant_distance(5.0), "identical")
})

# ------------------------------------------------------------------------------
# Info message functions
# ------------------------------------------------------------------------------

test_that("info_low_match_rate produces messages at different thresholds", {
  skip_on_cran()
  # < 25% produces warning
  expect_warning(couplr:::info_low_match_rate(10, 100, 10), "single")

  # 25-50% produces message - just verify no error
  expect_no_error(
    suppressMessages(couplr:::info_low_match_rate(40, 100, 40))
  )

  # > 50% produces info - just verify no error
  expect_no_error(
    suppressMessages(couplr:::info_low_match_rate(60, 100, 60))
  )
})

test_that("couplr_inform produces message", {
  skip_on_cran()
  expect_message(couplr:::couplr_inform("test message"), "test message")
})

test_that("couplr_success produces message", {
  skip_on_cran()
  expect_message(couplr:::couplr_success("success message"), "success")
})

# ------------------------------------------------------------------------------
# Additional warning/info tests
# ------------------------------------------------------------------------------

test_that("couplr_warn produces warning", {
  skip_on_cran()
  expect_warning(couplr:::couplr_warn("test warning"), "test warning")
})

test_that("couplr_stop produces error", {
  skip_on_cran()
  expect_error(couplr:::couplr_stop("test error"), "test error")
})

