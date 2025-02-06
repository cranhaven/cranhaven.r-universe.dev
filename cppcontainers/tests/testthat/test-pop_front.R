skip_on_os("mac")
# deque
testthat::test_that("deque pop_front integer", {
  v <- cpp_deque(4:9)
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), 5:9)
})
testthat::test_that("deque pop_front double", {
  v <- cpp_deque(seq.int(1, 2.5, 0.5))
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), seq.int(1.5, 2.5, 0.5))
})
testthat::test_that("deque pop_front string", {
  v <- cpp_deque(c("hello", "there", "world"))
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), c("there", "world"))
})
testthat::test_that("deque pop_front boolean", {
  v <- cpp_deque(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE))
})

# forward_list
testthat::test_that("forward_list pop_front integer", {
  v <- cpp_forward_list(4:9)
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), 5:9)
})
testthat::test_that("forward_list pop_front double", {
  v <- cpp_forward_list(seq.int(1, 2.5, 0.5))
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), seq.int(1.5, 2.5, 0.5))
})
testthat::test_that("forward_list pop_front string", {
  v <- cpp_forward_list(c("hello", "there", "world"))
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), c("there", "world"))
})
testthat::test_that("forward_list pop_front boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE))
})

# list
testthat::test_that("list pop_front integer", {
  v <- cpp_list(4:9)
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), 5:9)
})
testthat::test_that("list pop_front double", {
  v <- cpp_list(seq.int(1, 2.5, 0.5))
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), seq.int(1.5, 2.5, 0.5))
})
testthat::test_that("list pop_front string", {
  v <- cpp_list(c("hello", "there", "world"))
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), c("there", "world"))
})
testthat::test_that("list pop_front boolean", {
  v <- cpp_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(pop_front(v))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE))
})
