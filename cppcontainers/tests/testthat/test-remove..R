skip_on_os("mac")
# forward_list
testthat::test_that("forward_list remove. integer", {
  v <- cpp_forward_list(4:6)
  testthat::expect_invisible(remove.(v, 6L))
  testthat::expect_equal(to_r(v), 4:5)
})
testthat::test_that("forward_list remove. double", {
  v <- cpp_forward_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(remove.(v, 1.5))
  testthat::expect_equal(to_r(v), c(1, 2))
})
testthat::test_that("forward_list remove. string", {
  v <- cpp_forward_list(c("hello", "there", "world"))
  testthat::expect_invisible(remove.(v, "there"))
  testthat::expect_equal(to_r(v), c("hello", "world"))
})
testthat::test_that("forward_list remove. boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(remove.(v, TRUE))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE))
})

# list
testthat::test_that("list remove. integer", {
  v <- cpp_list(4:6)
  testthat::expect_invisible(remove.(v, 6L))
  testthat::expect_equal(to_r(v), 4:5)
})
testthat::test_that("list remove. double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(remove.(v, 1.5))
  testthat::expect_equal(to_r(v), c(1, 2))
})
testthat::test_that("list remove. string", {
  v <- cpp_list(c("hello", "there", "world"))
  testthat::expect_invisible(remove.(v, "there"))
  testthat::expect_equal(to_r(v), c("hello", "world"))
})
testthat::test_that("list remove. boolean", {
  v <- cpp_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(remove.(v, TRUE))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE))
})
