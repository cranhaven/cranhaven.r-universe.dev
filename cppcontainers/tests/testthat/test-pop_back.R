skip_on_os("mac")
# vector
testthat::test_that("vector pop_back integer", {
  v <- cpp_vector(4:9)
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), 4:8)
})
testthat::test_that("vector pop_back double", {
  v <- cpp_vector(seq.int(1, 2.5, 0.5))
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), seq.int(1, 2, 0.5))
})
testthat::test_that("vector pop_back string", {
  v <- cpp_vector(c("hello", "there", "world"))
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), c("hello", "there"))
})
testthat::test_that("vector pop_back boolean", {
  v <- cpp_vector(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE))
})

# deque
testthat::test_that("deque pop_back integer", {
  v <- cpp_deque(4:9)
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), 4:8)
})
testthat::test_that("deque pop_back double", {
  v <- cpp_deque(seq.int(1, 2.5, 0.5))
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), seq.int(1, 2, 0.5))
})
testthat::test_that("deque pop_back string", {
  v <- cpp_deque(c("hello", "there", "world"))
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), c("hello", "there"))
})
testthat::test_that("deque pop_back boolean", {
  v <- cpp_deque(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE))
})

# list
testthat::test_that("list pop_back integer", {
  v <- cpp_list(4:9)
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), 4:8)
})
testthat::test_that("list pop_back double", {
  v <- cpp_list(seq.int(1, 2.5, 0.5))
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), seq.int(1, 2, 0.5))
})
testthat::test_that("list pop_back string", {
  v <- cpp_list(c("hello", "there", "world"))
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), c("hello", "there"))
})
testthat::test_that("list pop_back boolean", {
  v <- cpp_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(pop_back(v))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE))
})
