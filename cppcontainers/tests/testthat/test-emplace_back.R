skip_on_os("mac")
# vector
testthat::test_that("vector emplace_back integer", {
  v <- cpp_vector(4:9)
  testthat::expect_invisible(emplace_back(v, 12L))
  testthat::expect_equal(to_r(v), c(4:9, 12L))
  testthat::expect_error(emplace_back(v, 13:14))
})
testthat::test_that("vector emplace_back double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace_back(v, 12))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 12))
  testthat::expect_error(emplace_back(v, c(13, 14)))
})
testthat::test_that("vector emplace_back string", {
  v <- cpp_vector(c("hello", "there"))
  testthat::expect_invisible(emplace_back(v, "world"))
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
  testthat::expect_error(emplace_back(v, c("test", "vector")))
})
testthat::test_that("vector emplace_back boolean", {
  v <- cpp_vector(c(TRUE, FALSE))
  testthat::expect_invisible(emplace_back(v, TRUE))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, TRUE))
  testthat::expect_error(emplace_back(v, c(TRUE, FALSE)))
})

# deque
testthat::test_that("deque emplace_back integer", {
  v <- cpp_deque(4:9)
  testthat::expect_invisible(emplace_back(v, 12L))
  testthat::expect_equal(to_r(v), c(4:9, 12L))
  testthat::expect_error(emplace_back(v, 13:14))
})
testthat::test_that("deque emplace_back double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace_back(v, 12))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 12))
  testthat::expect_error(emplace_back(v, c(13, 14)))
})
testthat::test_that("deque emplace_back string", {
  v <- cpp_deque(c("hello", "there"))
  testthat::expect_invisible(emplace_back(v, "world"))
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
  testthat::expect_error(emplace_back(v, c("test", "vector")))
})
testthat::test_that("deque emplace_back boolean", {
  v <- cpp_deque(c(TRUE, FALSE))
  testthat::expect_invisible(emplace_back(v, TRUE))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, TRUE))
  testthat::expect_error(emplace_back(v, c(TRUE, FALSE)))
})

# list
testthat::test_that("list emplace_back integer", {
  v <- cpp_list(4:9)
  testthat::expect_invisible(emplace_back(v, 12L))
  testthat::expect_equal(to_r(v), c(4:9, 12L))
  testthat::expect_error(emplace_back(v, 13:14))
})
testthat::test_that("list emplace_back double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace_back(v, 12))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 12))
  testthat::expect_error(emplace_back(v, c(13, 14)))
})
testthat::test_that("list emplace_back string", {
  v <- cpp_list(c("hello", "there"))
  testthat::expect_invisible(emplace_back(v, "world"))
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
  testthat::expect_error(emplace_back(v, c("test", "vector")))
})
testthat::test_that("list emplace_back boolean", {
  v <- cpp_list(c(TRUE, FALSE))
  testthat::expect_invisible(emplace_back(v, TRUE))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, TRUE))
  testthat::expect_error(emplace_back(v, c(TRUE, FALSE)))
})
