test_that("Valid table ID returns a vector", {
 expect_no_error(get_ons_table("nm_1_1", geography = "TYPE480", time = "latest"))
})

test_that("Valid query returning over 25,000 rows sends a warning", {
  expect_warning(get_ons_table("nm_105_1", time = "latest"))
})

test_that("Valid table ID returns a vector", {
  expect_no_error(get_ons_table("nm_1002_1", geography = "TYPE265", time = "latest"))
})

test_that("Valid table ID returns a vector", {
  expect_no_error(get_ons_table("nm_1002_1", geography = "TYPE265", time = "latest", select = c("GEOGRAPHY_NAME", "C_AGE_NAME", "OBS_VALUE")))
})

test_that("Valid table ID returns a list", {
  expect_type(get_ons_table("nm_1002_1", geography = "TYPE265", time = "latest", select = c("GEOGRAPHY_NAME", "C_AGE_NAME", "OBS_VALUE")), "list")
})

test_that("Invalid table ID returns an error message", {
  expect_error(get_ons_table("A_1_1", geography = "TYPE480", time = "latest"))
})
