skip_on_os("mac")
# vector
testthat::test_that("vector shrink_to_fit integer", {
  v <- cpp_vector(4:6)
  testthat::expect_invisible(shrink_to_fit(v))
})
testthat::test_that("vector shrink_to_fit double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_invisible(shrink_to_fit(v))
})
testthat::test_that("vector shrink_to_fit string", {
  v <- cpp_vector(c("hello", "there", "world"))
  testthat::expect_invisible(shrink_to_fit(v))
})
testthat::test_that("vector shrink_to_fit boolean", {
  v <- cpp_vector(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(shrink_to_fit(v))
})

# deque
testthat::test_that("deque shrink_to_fit integer", {
  v <- cpp_deque(4:6)
  testthat::expect_invisible(shrink_to_fit(v))
})
testthat::test_that("deque shrink_to_fit double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_invisible(shrink_to_fit(v))
})
testthat::test_that("deque shrink_to_fit string", {
  v <- cpp_deque(c("hello", "there", "world"))
  testthat::expect_invisible(shrink_to_fit(v))
})
testthat::test_that("deque shrink_to_fit boolean", {
  v <- cpp_deque(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(shrink_to_fit(v))
})
