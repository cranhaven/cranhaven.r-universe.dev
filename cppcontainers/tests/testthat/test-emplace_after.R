skip_on_os("mac")
# forward_list
testthat::test_that("forward_list emplace_after integer", {
  v <- cpp_forward_list(4:9)
  testthat::expect_invisible(emplace_after(v, 12L, position = 2L))
  testthat::expect_equal(to_r(v), c(4:5, 12L, 6:9))
  testthat::expect_error(emplace_after(v, 13:14))
})
testthat::test_that("forward_list emplace_after double", {
  v <- cpp_forward_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace_after(v, 12, position = 2L))
  testthat::expect_equal(to_r(v), c(1, 1.5, 12, 2))
  testthat::expect_error(emplace_after(v, c(13, 14)))
})
testthat::test_that("forward_list emplace_after string", {
  v <- cpp_forward_list(c("hello", "there"))
  testthat::expect_invisible(emplace_after(v, "world", position = 2L))
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
  testthat::expect_error(emplace_after(v, c("test", "vector")))
})
testthat::test_that("forward_list emplace_after boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE))
  testthat::expect_invisible(emplace_after(v, TRUE, position = 2L))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, TRUE))
  testthat::expect_error(emplace_after(v, c(TRUE, FALSE)))
})
