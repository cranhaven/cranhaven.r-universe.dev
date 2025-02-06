skip_on_os("mac")
# priority_queue
testthat::test_that("priority_queue sorting integer descending", {
  v <- cpp_priority_queue(4:9)
  testthat::expect_equal(sorting(v), "descending")
})
testthat::test_that("priority_queue sorting double descending", {
  v <- cpp_priority_queue(seq.int(1, 3.5, 0.5))
  testthat::expect_equal(sorting(v), "descending")
})
testthat::test_that("priority_queue sorting string descending", {
  v <- cpp_priority_queue(c("hello", "there"))
  testthat::expect_equal(sorting(v), "descending")
})
testthat::test_that("priority_queue sorting boolean descending", {
  v <- cpp_priority_queue(c(TRUE, FALSE))
 testthat::expect_equal(sorting(v), "descending")
})
testthat::test_that("priority_queue sorting integer ascending", {
  v <- cpp_priority_queue(4:9, "ascending")
  testthat::expect_equal(sorting(v), "ascending")
})
testthat::test_that("priority_queue sorting double ascending", {
  v <- cpp_priority_queue(seq.int(1, 3.5, 0.5), "ascending")
  testthat::expect_equal(sorting(v), "ascending")
})
testthat::test_that("priority_queue sorting string ascending", {
  v <- cpp_priority_queue(c("hello", "there"), "ascending")
  testthat::expect_equal(sorting(v), "ascending")
})
testthat::test_that("priority_queue sorting boolean ascending", {
  v <- cpp_priority_queue(c(TRUE, FALSE), "ascending")
 testthat::expect_equal(sorting(v), "ascending")
})
