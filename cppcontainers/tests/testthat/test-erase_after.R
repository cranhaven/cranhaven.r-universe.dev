skip_on_os("mac")
# forward_list
testthat::test_that("forward_list erase_after integer", {
  v <- cpp_forward_list(4:9)
  testthat::expect_invisible(erase_after(v, from = 2, to = 4))
  testthat::expect_equal(to_r(v), c(4:5, 8:9))
})
testthat::test_that("forward_list erase_after double", {
  v <- cpp_forward_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(erase_after(v, from = 1, to = 3))
  testthat::expect_equal(to_r(v), 1)
})
testthat::test_that("forward_list erase_after string", {
  v <- cpp_forward_list(c("hello", "there", "world"))
  testthat::expect_invisible(erase_after(v, from = 1, to = 3))
  testthat::expect_equal(to_r(v), "hello")
})
testthat::test_that("forward_list erase_after boolean", {
  v <- cpp_forward_list(c(FALSE, TRUE))
  testthat::expect_invisible(erase_after(v, from = 1, to = 2))
  testthat::expect_equal(to_r(v), FALSE)
})
