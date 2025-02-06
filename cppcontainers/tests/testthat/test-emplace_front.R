skip_on_os("mac")
# deque
testthat::test_that("deque emplace_front integer", {
  v <- cpp_deque(4:9)
  testthat::expect_invisible(emplace_front(v, 12L))
  testthat::expect_equal(to_r(v), c(12L, 4:9))
  testthat::expect_error(emplace_front(v, 13:14))
})
testthat::test_that("deque emplace_front double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace_front(v, 12))
  testthat::expect_equal(to_r(v), c(12, seq.int(1, 2, 0.5)))
  testthat::expect_error(emplace_front(v, c(13, 14)))
})
testthat::test_that("deque emplace_front string", {
  v <- cpp_deque(c("hello", "there"))
  testthat::expect_invisible(emplace_front(v, "world"))
  testthat::expect_equal(to_r(v), c("world", "hello", "there"))
  testthat::expect_error(emplace_front(v, c("test", "vector")))
})
testthat::test_that("deque emplace_front boolean", {
  v <- cpp_deque(c(TRUE, FALSE))
  testthat::expect_invisible(emplace_front(v, TRUE))
  testthat::expect_equal(to_r(v), c(TRUE, TRUE, FALSE))
  testthat::expect_error(emplace_front(v, c(TRUE, FALSE)))
})

# forward_list
testthat::test_that("forward_list emplace_front integer", {
  v <- cpp_forward_list(4:9)
  testthat::expect_invisible(emplace_front(v, 12L))
  testthat::expect_equal(to_r(v), c(12L, 4:9))
  testthat::expect_error(emplace_front(v, 13:14))
})
testthat::test_that("forward_list emplace_front double", {
  v <- cpp_forward_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace_front(v, 12))
  testthat::expect_equal(to_r(v), c(12, seq.int(1, 2, 0.5)))
  testthat::expect_error(emplace_front(v, c(13, 14)))
})
testthat::test_that("forward_list emplace_front string", {
  v <- cpp_forward_list(c("hello", "there"))
  testthat::expect_invisible(emplace_front(v, "world"))
  testthat::expect_equal(to_r(v), c("world", "hello", "there"))
  testthat::expect_error(emplace_front(v, c("test", "vector")))
})
testthat::test_that("forward_list emplace_front boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE))
  testthat::expect_invisible(emplace_front(v, TRUE))
  testthat::expect_equal(to_r(v), c(TRUE, TRUE, FALSE))
  testthat::expect_error(emplace_front(v, c(TRUE, FALSE)))
})

# list
testthat::test_that("list emplace_front integer", {
  v <- cpp_list(4:9)
  testthat::expect_invisible(emplace_front(v, 12L))
  testthat::expect_equal(to_r(v), c(12L, 4:9))
  testthat::expect_error(emplace_front(v, 13:14))
})
testthat::test_that("list emplace_front double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace_front(v, 12))
  testthat::expect_equal(to_r(v), c(12, seq.int(1, 2, 0.5)))
  testthat::expect_error(emplace_front(v, c(13, 14)))
})
testthat::test_that("list emplace_front string", {
  v <- cpp_list(c("hello", "there"))
  testthat::expect_invisible(emplace_front(v, "world"))
  testthat::expect_equal(to_r(v), c("world", "hello", "there"))
  testthat::expect_error(emplace_front(v, c("test", "vector")))
})
testthat::test_that("list emplace_front boolean", {
  v <- cpp_list(c(TRUE, FALSE))
  testthat::expect_invisible(emplace_front(v, TRUE))
  testthat::expect_equal(to_r(v), c(TRUE, TRUE, FALSE))
  testthat::expect_error(emplace_front(v, c(TRUE, FALSE)))
})
