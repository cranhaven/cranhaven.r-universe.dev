test_that("API deprecation warning is shown once per session", {
  original_value <- .dawaR_env$deprecation_warning_shown
  on.exit({
    .dawaR_env$deprecation_warning_shown <- original_value
  })

  .dawaR_env$deprecation_warning_shown <- FALSE

  expect_warning(
    warn_api_deprecation(),
    "DAWA is deprecated.*October 1, 2026"
  )
  expect_no_warning(warn_api_deprecation())
})
