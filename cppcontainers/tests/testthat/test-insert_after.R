skip_on_os("mac")
# forward_list
testthat::test_that("forward_list insert_after integer", {
  v <- cpp_forward_list(4:9)
  testthat::expect_invisible(insert_after(v, 12:13, position = 2))
  testthat::expect_equal(to_r(v), c(4:5, 12:13, 6:9))
})
testthat::test_that("forward_list insert_after double", {
  v <- cpp_forward_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(insert_after(v, c(12, 13), position = 2))
  testthat::expect_equal(to_r(v), c(1, 1.5, 12, 13, 2))
})
testthat::test_that("forward_list insert_after string", {
  v <- cpp_forward_list(c("hello", "there"))
  testthat::expect_invisible(insert_after(v, c("world", "a"), position = 1))
  testthat::expect_equal(to_r(v), c("hello", "world", "a", "there"))
})
testthat::test_that("forward_list insert_after boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert_after(v, c(FALSE, TRUE), position = 1))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, TRUE, FALSE, FALSE))
})
