skip_on_os("mac")
tests <- function(v) {
  r <- testthat::expect_type(capacity(v), "double")
  testthat::expect_length(r, 1L)
  testthat::expect_gt(r, 0)
  testthat::expect_true(is.finite(r))
}

# vector
testthat::test_that("vector capacity integer", {
  v <- cpp_vector(4:9)
  tests(v)
})
testthat::test_that("vector capacity double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  tests(v)
})
testthat::test_that("vector capacity string", {
  v <- cpp_vector(c("hello", "there"))
  tests(v)
})
testthat::test_that("vector capacity boolean", {
  v <- cpp_vector(c(TRUE, FALSE))
  tests(v)
})
