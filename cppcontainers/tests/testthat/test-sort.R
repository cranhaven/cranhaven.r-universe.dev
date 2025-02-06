skip_on_os("mac")
# forward_list
testthat::test_that("forward_list sort integer", {
  v <- cpp_forward_list(c(4L, 9L, 6L))
  testthat::expect_invisible(sort(v))
  testthat::expect_equal(to_r(v), c(4L, 6L, 9L))
})
testthat::test_that("forward_list sort double", {
  v <- cpp_forward_list(c(1, 3.5, 2))
  testthat::expect_invisible(sort(v))
  testthat::expect_equal(to_r(v), c(1, 2, 3.5))
})
testthat::test_that("forward_list sort string", {
  v <- cpp_forward_list(c("hello", "world", "there"))
  testthat::expect_invisible(sort(v))
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
})
testthat::test_that("forward_list sort boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(sort(v))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, TRUE))
})

# list
testthat::test_that("list sort integer", {
  v <- cpp_list(c(4L, 9L, 6L))
  testthat::expect_invisible(sort(v))
  testthat::expect_equal(to_r(v), c(4L, 6L, 9L))
})
testthat::test_that("list sort double", {
  v <- cpp_list(c(1, 3.5, 2))
  testthat::expect_invisible(sort(v))
  testthat::expect_equal(to_r(v), c(1, 2, 3.5))
})
testthat::test_that("list sort string", {
  v <- cpp_list(c("hello", "world", "there"))
  testthat::expect_invisible(sort(v))
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
})
testthat::test_that("list sort boolean", {
  v <- cpp_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(sort(v))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, TRUE))
})
