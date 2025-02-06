skip_on_os("mac")
# vector
testthat::test_that("vector push_back integer", {
  v <- cpp_vector(4:6)
  testthat::expect_invisible(push_back(v, 9L))
  testthat::expect_equal(to_r(v), c(4:6, 9L))
  testthat::expect_error(push_back(v, 10:11))
})
testthat::test_that("vector push_back double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_invisible(push_back(v, 3))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 3))
  testthat::expect_error(push_back(v, c(3.1, 3.2)))
})
testthat::test_that("vector push_back string", {
  v <- cpp_vector(c("hello", "there", "world"))
  testthat::expect_invisible(push_back(v, "R"))
  testthat::expect_equal(to_r(v), c("hello", "there", "world", "R"))
  testthat::expect_error(push_back(v, c("a", "test")))
})
testthat::test_that("vector push_back boolean", {
  v <- cpp_vector(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(push_back(v, FALSE))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, FALSE, FALSE))
  testthat::expect_error(push_back(v, c(TRUE, TRUE)))
})

# deque
testthat::test_that("deque push_back integer", {
  v <- cpp_deque(4:6)
  testthat::expect_invisible(push_back(v, 9L))
  testthat::expect_equal(to_r(v), c(4:6, 9L))
  testthat::expect_error(push_back(v, 10:11))
})
testthat::test_that("deque push_back double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_invisible(push_back(v, 3))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 3))
  testthat::expect_error(push_back(v, c(3.1, 3.2)))
})
testthat::test_that("deque push_back string", {
  v <- cpp_deque(c("hello", "there", "world"))
  testthat::expect_invisible(push_back(v, "R"))
  testthat::expect_equal(to_r(v), c("hello", "there", "world", "R"))
  testthat::expect_error(push_back(v, c("a", "test")))
})
testthat::test_that("deque push_back boolean", {
  v <- cpp_deque(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(push_back(v, FALSE))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, FALSE, FALSE))
  testthat::expect_error(push_back(v, c(TRUE, TRUE)))
})

# list
testthat::test_that("list push_back integer", {
  v <- cpp_list(4:6)
  testthat::expect_invisible(push_back(v, 9L))
  testthat::expect_equal(to_r(v), c(4:6, 9L))
  testthat::expect_error(push_back(v, 10:11))
})
testthat::test_that("list push_back double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(push_back(v, 3))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 3))
  testthat::expect_error(push_back(v, c(3.1, 3.2)))
})
testthat::test_that("list push_back string", {
  v <- cpp_list(c("hello", "there", "world"))
  testthat::expect_invisible(push_back(v, "R"))
  testthat::expect_equal(to_r(v), c("hello", "there", "world", "R"))
  testthat::expect_error(push_back(v, c("a", "test")))
})
testthat::test_that("list push_back boolean", {
  v <- cpp_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(push_back(v, FALSE))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, FALSE, FALSE))
  testthat::expect_error(push_back(v, c(TRUE, TRUE)))
})
