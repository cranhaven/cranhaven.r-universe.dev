


# Config: validate --------------------------------------------------------

test_that("thaipdf_config_validate() works", {

  # Thai font
  expect_error(thaipdf_config_validate(thai_font = 1))
  expect_error(thaipdf_config_validate(thai_font = c("")))
  expect_error(thaipdf_config_validate(thai_font = c("a", "b")))

  expect_error(thaipdf_config_validate(line_spacing = "a"))
  expect_error(thaipdf_config_validate(line_spacing = 1:2))

})
