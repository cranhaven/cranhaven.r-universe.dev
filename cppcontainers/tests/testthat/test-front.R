skip_on_os("mac")
# queue
testthat::test_that("queue front integer", {
  v <- cpp_queue(4:9)
  testthat::expect_equal(front(v), 4L)
})
testthat::test_that("queue front double", {
  v <- cpp_queue(seq.int(1, 2, 0.5))
  testthat::expect_equal(front(v), 1)
})
testthat::test_that("queue front string", {
  v <- cpp_queue(c("hello", "there", "world"))
  testthat::expect_equal(front(v), "hello")
})
testthat::test_that("queue front boolean", {
  v <- cpp_queue(c(TRUE, FALSE))
  testthat::expect_equal(front(v), TRUE)
})

# vector
testthat::test_that("vector front integer", {
  v <- cpp_vector(4:9)
  testthat::expect_equal(front(v), 4L)
})
testthat::test_that("vector front double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_equal(front(v), 1)
})
testthat::test_that("vector front string", {
  v <- cpp_vector(c("hello", "there", "world"))
  testthat::expect_equal(front(v), "hello")
})
testthat::test_that("vector front boolean", {
  v <- cpp_vector(c(TRUE, FALSE))
  testthat::expect_equal(front(v), TRUE)
})

# deque
testthat::test_that("deque front integer", {
  v <- cpp_deque(4:9)
  testthat::expect_equal(front(v), 4L)
})
testthat::test_that("deque front double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_equal(front(v), 1)
})
testthat::test_that("deque front string", {
  v <- cpp_deque(c("hello", "there", "world"))
  testthat::expect_equal(front(v), "hello")
})
testthat::test_that("deque front boolean", {
  v <- cpp_deque(c(TRUE, FALSE))
  testthat::expect_equal(front(v), TRUE)
})

# forward_list
testthat::test_that("forward_list front integer", {
  v <- cpp_forward_list(4:9)
  testthat::expect_equal(front(v), 4L)
})
testthat::test_that("forward_list front double", {
  v <- cpp_forward_list(seq.int(1, 2, 0.5))
  testthat::expect_equal(front(v), 1)
})
testthat::test_that("forward_list front string", {
  v <- cpp_forward_list(c("hello", "there", "world"))
  testthat::expect_equal(front(v), "hello")
})
testthat::test_that("forward_list front boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE))
  testthat::expect_equal(front(v), TRUE)
})

# list
testthat::test_that("list front integer", {
  v <- cpp_list(4:9)
  testthat::expect_equal(front(v), 4L)
})
testthat::test_that("list front double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_equal(front(v), 1)
})
testthat::test_that("list front string", {
  v <- cpp_list(c("hello", "there", "world"))
  testthat::expect_equal(front(v), "hello")
})
testthat::test_that("list front boolean", {
  v <- cpp_list(c(TRUE, FALSE))
  testthat::expect_equal(front(v), TRUE)
})
