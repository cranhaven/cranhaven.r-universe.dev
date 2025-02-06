skip_on_os("mac")
# stack
testthat::test_that("stack top integer", {
  v <- cpp_stack(4:6)
  testthat::expect_equal(top(v), 6L)
})
testthat::test_that("stack top double", {
  v <- cpp_stack(seq.int(1, 2, 0.5))
  testthat::expect_equal(top(v), 2)
})
testthat::test_that("stack top string", {
  v <- cpp_stack(c("hello", "there", "world"))
  testthat::expect_equal(top(v), "world")
})
testthat::test_that("stack top boolean", {
  v <- cpp_stack(c(TRUE, FALSE))
  testthat::expect_equal(top(v), FALSE)
})

# priority_queue
testthat::test_that("priority_queue top integer descending", {
  v <- cpp_priority_queue(4:6)
  testthat::expect_equal(top(v), 6L)
})
testthat::test_that("priority_queue top double descending", {
  v <- cpp_priority_queue(seq.int(1, 2, 0.5))
  testthat::expect_equal(top(v), 2)
})
testthat::test_that("priority_queue top string descending", {
  v <- cpp_priority_queue(c("hello", "there", "world"))
  testthat::expect_equal(top(v), "world")
})
testthat::test_that("priority_queue top boolean descending", {
  v <- cpp_priority_queue(c(TRUE, FALSE))
  testthat::expect_equal(top(v), TRUE)
})
testthat::test_that("priority_queue top integer ascending", {
  v <- cpp_priority_queue(4:6, "ascending")
  testthat::expect_equal(top(v), 4L)
})
testthat::test_that("priority_queue top double ascending", {
  v <- cpp_priority_queue(seq.int(1, 2, 0.5), "ascending")
  testthat::expect_equal(top(v), 1)
})
testthat::test_that("priority_queue top string ascending", {
  v <- cpp_priority_queue(c("hello", "there", "world"), "ascending")
  testthat::expect_equal(top(v), "hello")
})
testthat::test_that("priority_queue top boolean ascending", {
  v <- cpp_priority_queue(c(TRUE, FALSE), "ascending")
  testthat::expect_equal(top(v), FALSE)
})
