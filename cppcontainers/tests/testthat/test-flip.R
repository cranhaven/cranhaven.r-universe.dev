skip_on_os("mac")
# vector
testthat::test_that("vector flip boolean", {
  v <- cpp_vector(c(TRUE, FALSE))
  testthat::expect_invisible(flip(v))
  testthat::expect_equal(to_r(v), c(FALSE, TRUE))
})
