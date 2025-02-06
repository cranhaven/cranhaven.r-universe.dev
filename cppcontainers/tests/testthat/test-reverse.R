skip_on_os("mac")
# forward_list
testthat::test_that("forward_list reverse integer", {
  v <- cpp_forward_list(4:6)
  testthat::expect_invisible(reverse(v))
  testthat::expect_equal(to_r(v), 6:4)
})
testthat::test_that("forward_list reverse double", {
  v <- cpp_forward_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(reverse(v))
  testthat::expect_equal(to_r(v), seq.int(2, 1, -0.5))
})
testthat::test_that("forward_list reverse string", {
  v <- cpp_forward_list(c("hello", "there", "world"))
  testthat::expect_invisible(reverse(v))
  testthat::expect_equal(to_r(v), c("world", "there", "hello"))
})
testthat::test_that("forward_list reverse boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(reverse(v))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, TRUE))
})

# list
testthat::test_that("list reverse integer", {
  v <- cpp_list(4:6)
  testthat::expect_invisible(reverse(v))
  testthat::expect_equal(to_r(v), 6:4)
})
testthat::test_that("list reverse double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(reverse(v))
  testthat::expect_equal(to_r(v), seq.int(2, 1, -0.5))
})
testthat::test_that("list reverse string", {
  v <- cpp_list(c("hello", "there", "world"))
  testthat::expect_invisible(reverse(v))
  testthat::expect_equal(to_r(v), c("world", "there", "hello"))
})
testthat::test_that("list reverse boolean", {
  v <- cpp_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(reverse(v))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, TRUE))
})
