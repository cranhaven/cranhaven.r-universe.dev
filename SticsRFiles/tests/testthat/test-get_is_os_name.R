library(SticsRFiles)


context("Is OS name")

test_that("Is OS name", {
  expect_equal(length(is_os_name()), 4)
  expect_type(is_os_name("windows"), "logical")
  expect_type(is_os_name("linux"), "logical")
  expect_type(is_os_name("mac"), "logical")

})
