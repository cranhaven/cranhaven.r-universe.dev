skip_on_os("mac")
# stack
testthat::test_that("stack pop integer", {
  v <- cpp_stack(4:9)
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), 8:4)
})
testthat::test_that("stack pop double", {
  v <- cpp_stack(seq.int(1, 2.5, 0.5))
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), seq.int(2, 1, -0.5))
})
testthat::test_that("stack pop string", {
  v <- cpp_stack(c("hello", "there", "world"))
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), c("there", "hello"))
})
testthat::test_that("stack pop boolean", {
  v <- cpp_stack(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), c(FALSE, TRUE))
})

# queue
testthat::test_that("queue pop integer", {
  v <- cpp_queue(4:9)
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), 5:9)
})
testthat::test_that("queue pop double", {
  v <- cpp_queue(seq.int(1, 2.5, 0.5))
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), seq.int(1.5, 2.5, 0.5))
})
testthat::test_that("queue pop string", {
  v <- cpp_queue(c("hello", "there", "world"))
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), c("there", "world"))
})
testthat::test_that("queue pop boolean", {
  v <- cpp_queue(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE))
})

# priority_queue
testthat::test_that("priority_queue pop integer descending", {
  v <- cpp_priority_queue(4:9)
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), 8:4)
})
testthat::test_that("priority_queue pop double descending", {
  v <- cpp_priority_queue(seq.int(1, 2.5, 0.5))
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), seq.int(2, 1, -0.5))
})
testthat::test_that("priority_queue pop string descending", {
  v <- cpp_priority_queue(c("hello", "there", "world"))
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), c("there", "hello"))
})
testthat::test_that("priority_queue pop boolean descending", {
  v <- cpp_priority_queue(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE))
})
testthat::test_that("priority_queue pop integer ascending", {
  v <- cpp_priority_queue(4:9, "ascending")
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), 5:9)
})
testthat::test_that("priority_queue pop double ascending", {
  v <- cpp_priority_queue(seq.int(1, 2.5, 0.5), "ascending")
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), seq.int(1.5, 2.5, 0.5))
})
testthat::test_that("priority_queue pop string ascending", {
  v <- cpp_priority_queue(c("hello", "there", "world"), "ascending")
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), c("there", "world"))
})
testthat::test_that("priority_queue pop boolean ascending", {
  v <- cpp_priority_queue(c(TRUE, FALSE, FALSE), "ascending")
  testthat::expect_invisible(pop(v))
  testthat::expect_equal(to_r(v), c(FALSE, TRUE))
})
