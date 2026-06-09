#' Test lens projection function
#'
#' @description
#' Verify that a lens projection maps zenith 0 deg to 0 and 90 to 1.
#'
#' @details
#' The package tolerate a number very close to 1 at 90 deg but not exactly 1 as
#' long as it is greater than 1. See [testthat::expect_equal()] for tolerance
#' details.
#'
#' When the test fails at *"Test that lens projection
#' function does not predict values barely below one"*, the best practice is to
#' manually edit the last coefficient (e.g., change -0.0296 to -0.0295).
#'
#' If the check *"works within the 0–1 range"* fails, new calibration data may
#' be required.
#'
#' @inheritParams zenith_image
#'
#' @return Invisibly returns `TRUE` if all checks pass; otherwise an error is
#'   thrown.
#'
#' @export
#'
#' @seealso [calc_relative_radius()]
#'
#' @examples
#' test_lens_coef(lens("Nikon_FCE9"))
#' test_lens_coef(2/pi)
test_lens_coef <- function(lens_coef) {
  .check_vector(lens_coef, "numeric", sign = "any")

  testthat::test_that(
    "Test that lens projection function works between the 0-1 range",
    {
      testthat::expect_equal(calc_relative_radius(0, lens_coef) %>%
                               round(., 2), 0)
      testthat::expect_equal(calc_relative_radius(90, lens_coef) %>%
                               round(., 2), 1)
    }
  )
  testthat::test_that(
    "Test that lens projection function does not predict values barely below one",
    {
      testthat::expect_true(calc_relative_radius(90, lens_coef) >= 1)
    }
  )
}
