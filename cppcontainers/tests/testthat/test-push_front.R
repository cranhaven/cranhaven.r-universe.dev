skip_on_os("mac")
# deque
testthat::test_that("deque push_front integer", {
  v <- cpp_deque(4:6)
  testthat::expect_invisible(push_front(v, 9L))
  testthat::expect_equal(to_r(v), c(9L, 4:6))
  testthat::expect_error(push_front(v, 10:11))
})
testthat::test_that("deque push_front double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_invisible(push_front(v, 3))
  testthat::expect_equal(to_r(v), c(3, seq.int(1, 2, 0.5)))
  testthat::expect_error(push_front(v, c(3.1, 3.2)))
})
testthat::test_that("deque push_front string", {
  v <- cpp_deque(c("hello", "there", "world"))
  testthat::expect_invisible(push_front(v, "R"))
  testthat::expect_equal(to_r(v), c("R", "hello", "there", "world"))
  testthat::expect_error(push_front(v, c("a", "test")))
})
testthat::test_that("deque push_front boolean", {
  v <- cpp_deque(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(push_front(v, FALSE))
  testthat::expect_equal(to_r(v), c(FALSE, TRUE, FALSE, FALSE))
  testthat::expect_error(push_front(v, c(TRUE, TRUE)))
})

# list
testthat::test_that("list push_front integer", {
  v <- cpp_list(4:6)
  testthat::expect_invisible(push_front(v, 9L))
  testthat::expect_equal(to_r(v), c(9L, 4:6))
  testthat::expect_error(push_front(v, 10:11))
})
testthat::test_that("list push_front double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(push_front(v, 3))
  testthat::expect_equal(to_r(v), c(3, seq.int(1, 2, 0.5)))
  testthat::expect_error(push_front(v, c(3.1, 3.2)))
})
testthat::test_that("list push_front string", {
  v <- cpp_list(c("hello", "there", "world"))
  testthat::expect_invisible(push_front(v, "R"))
  testthat::expect_equal(to_r(v), c("R", "hello", "there", "world"))
  testthat::expect_error(push_front(v, c("a", "test")))
})
testthat::test_that("list push_front boolean", {
  v <- cpp_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(push_front(v, FALSE))
  testthat::expect_equal(to_r(v), c(FALSE, TRUE, FALSE, FALSE))
  testthat::expect_error(push_front(v, c(TRUE, TRUE)))
})
