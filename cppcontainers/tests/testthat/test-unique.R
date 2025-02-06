skip_on_os("mac")
# forward_list
testthat::test_that("forward_list type integer", {
  v <- cpp_forward_list(c(4L, 4:6))
  testthat::expect_equal(unique(v), 1)
  testthat::expect_equal(to_r(v), 4:6)
})
testthat::test_that("forward_list type double", {
  v <- cpp_forward_list(c(seq.int(1, 2, 0.5), 2, 2))
  testthat::expect_equal(unique(v), 2)
  testthat::expect_equal(to_r(v), seq.int(1, 2, 0.5))
})
testthat::test_that("forward_list type string", {
  v <- cpp_forward_list(c("hello", "there", "world", "world"))
  testthat::expect_equal(unique(v), 1)
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
})
testthat::test_that("forward_list type boolean", {
  v <- cpp_forward_list(c(TRUE, TRUE, FALSE))
  testthat::expect_equal(unique(v), 1)
  testthat::expect_equal(to_r(v), c(TRUE, FALSE))
})

# list
testthat::test_that("list type integer", {
  v <- cpp_list(c(4L, 4:6))
  testthat::expect_equal(unique(v), 1)
  testthat::expect_equal(to_r(v), 4:6)
})
testthat::test_that("list type double", {
  v <- cpp_list(c(seq.int(1, 2, 0.5), 2, 2))
  testthat::expect_equal(unique(v), 2)
  testthat::expect_equal(to_r(v), seq.int(1, 2, 0.5))
})
testthat::test_that("list type string", {
  v <- cpp_list(c("hello", "there", "world", "world"))
  testthat::expect_equal(unique(v), 1)
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
})
testthat::test_that("list type boolean", {
  v <- cpp_list(c(TRUE, TRUE, FALSE))
  testthat::expect_equal(unique(v), 1)
  testthat::expect_equal(to_r(v), c(TRUE, FALSE))
})
