skip_on_os("mac")
# stack
testthat::test_that("stack push integer", {
  v <- cpp_stack(4:6)
  testthat::expect_invisible(push(v, 9L))
  testthat::expect_equal(to_r(v), c(9L, 6:4))
})
testthat::test_that("stack push double", {
  v <- cpp_stack(seq.int(1, 2, 0.5))
  testthat::expect_invisible(push(v, 3))
  testthat::expect_equal(to_r(v), c(3, seq.int(2, 1, -0.5)))
})
testthat::test_that("stack push string", {
  v <- cpp_stack(c("hello", "there", "world"))
  testthat::expect_invisible(push(v, "R"))
  testthat::expect_equal(to_r(v), c("R", "world", "there", "hello"))
})
testthat::test_that("stack push boolean", {
  v <- cpp_stack(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(push(v, FALSE))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, FALSE, TRUE))
})

# queue
testthat::test_that("queue push integer", {
  v <- cpp_queue(4:6)
  testthat::expect_invisible(push(v, 9L))
  testthat::expect_equal(to_r(v), c(4:6, 9L))
})
testthat::test_that("queue push double", {
  v <- cpp_queue(seq.int(1, 2, 0.5))
  testthat::expect_invisible(push(v, 3))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 3))
})
testthat::test_that("queue push string", {
  v <- cpp_queue(c("hello", "there", "world"))
  testthat::expect_invisible(push(v, "R"))
  testthat::expect_equal(to_r(v), c("hello", "there", "world", "R"))
})
testthat::test_that("queue push boolean", {
  v <- cpp_queue(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(push(v, FALSE))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, FALSE, FALSE))
})

# priority_queue
testthat::test_that("priority_queue push integer desceding", {
  v <- cpp_priority_queue(4:6)
  testthat::expect_invisible(push(v, 9L))
  testthat::expect_equal(to_r(v), c(9L, 6:4))
})
testthat::test_that("priority_queue push double desceding", {
  v <- cpp_priority_queue(seq.int(1, 2, 0.5))
  testthat::expect_invisible(push(v, 3))
  testthat::expect_equal(to_r(v), c(3, seq.int(2, 1, -0.5)))
})
testthat::test_that("priority_queue push string desceding", {
  v <- cpp_priority_queue(c("hello", "there", "world"))
  testthat::expect_invisible(push(v, "R"))
  testthat::expect_equal(to_r(v), c("world", "there", "hello", "R"))
})
testthat::test_that("priority_queue push boolean desceding", {
  v <- cpp_priority_queue(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(push(v, FALSE))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, FALSE, FALSE))
})
testthat::test_that("priority_queue push integer ascending", {
  v <- cpp_priority_queue(4:6, "ascending")
  testthat::expect_invisible(push(v, 9L))
  testthat::expect_equal(to_r(v), c(4:6, 9L))
})
testthat::test_that("priority_queue push double ascending", {
  v <- cpp_priority_queue(seq.int(1, 2, 0.5), "ascending")
  testthat::expect_invisible(push(v, 3))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 3))
})
testthat::test_that("priority_queue push string ascending", {
  v <- cpp_priority_queue(c("hello", "there", "world"), "ascending")
  testthat::expect_invisible(push(v, "R"))
  testthat::expect_equal(to_r(v), c("R", "hello", "there", "world"))
})
testthat::test_that("priority_queue push boolean ascending", {
  v <- cpp_priority_queue(c(TRUE, FALSE, FALSE), "ascending")
  testthat::expect_invisible(push(v, FALSE))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, FALSE, TRUE))
})
