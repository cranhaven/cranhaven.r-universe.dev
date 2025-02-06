skip_on_os("mac")
# queue
testthat::test_that("queue back integer", {
  v <- cpp_queue(4:9)
  testthat::expect_equal(back(v), 9L)
})
testthat::test_that("queue back double", {
  v <- cpp_queue(seq.int(1, 2, 0.5))
  testthat::expect_equal(back(v), 2)
})
testthat::test_that("queue back string", {
  v <- cpp_queue(c("hello", "there"))
  testthat::expect_equal(back(v), "there")
})
testthat::test_that("queue back boolean", {
  v <- cpp_queue(c(TRUE, FALSE))
  testthat::expect_equal(back(v), FALSE)
})

# vector
testthat::test_that("vector back integer", {
  v <- cpp_vector(4:9)
  testthat::expect_equal(back(v), 9L)
})
testthat::test_that("vector back double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_equal(back(v), 2)
})
testthat::test_that("vector back string", {
  v <- cpp_vector(c("hello", "there"))
  testthat::expect_equal(back(v), "there")
})
testthat::test_that("vector back boolean", {
  v <- cpp_vector(c(TRUE, FALSE))
  testthat::expect_equal(back(v), FALSE)
})

# deque
testthat::test_that("deque back integer", {
  v <- cpp_deque(4:9)
  testthat::expect_equal(back(v), 9L)
})
testthat::test_that("deque back double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_equal(back(v), 2)
})
testthat::test_that("deque back string", {
  v <- cpp_deque(c("hello", "there"))
  testthat::expect_equal(back(v), "there")
})
testthat::test_that("deque back boolean", {
  v <- cpp_deque(c(TRUE, FALSE))
  testthat::expect_equal(back(v), FALSE)
})

# list
testthat::test_that("list back integer", {
  v <- cpp_list(4:9)
  testthat::expect_equal(back(v), 9L)
})
testthat::test_that("list back double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_equal(back(v), 2)
})
testthat::test_that("list back string", {
  v <- cpp_list(c("hello", "there"))
  testthat::expect_equal(back(v), "there")
})
testthat::test_that("list back boolean", {
  v <- cpp_list(c(TRUE, FALSE))
  testthat::expect_equal(back(v), FALSE)
})
