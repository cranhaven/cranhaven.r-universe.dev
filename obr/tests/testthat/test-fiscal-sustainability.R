# get_pension_projections() was deprecated in 0.5.1 after the OBR restructured
# the FSR workbooks (July 2026). It no longer accesses the network; it warns
# and returns NULL. See R/fiscal_sustainability.R.

test_that("get_pension_projections() is deprecated and returns NULL", {
  expect_warning(
    result <- get_pension_projections(),
    "deprecated",
    class = "deprecatedWarning"
  )
  expect_null(result)
})

test_that("get_pension_projections() ignores its refresh argument", {
  # The stub must not error on the old signature and must not hit the network.
  expect_warning(result <- get_pension_projections(refresh = TRUE), "deprecated")
  expect_null(result)
})
