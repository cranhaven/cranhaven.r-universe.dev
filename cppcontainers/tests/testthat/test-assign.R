skip_on_os("mac")
# vector
testthat::test_that("vector assign integer", {
  v <- cpp_vector(4:9)
  testthat::expect_no_error(assign(v, 12:14))
  testthat::expect_equal(to_r(v), 12:14)
})
testthat::test_that("vector assign double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_no_error(assign(v, seq.int(3, 6, 0.5)))
  testthat::expect_equal(to_r(v), seq.int(3, 6, 0.5))
})
testthat::test_that("vector assign string", {
  v <- cpp_vector(c("hello", "there"))
  testthat::expect_no_error(assign(v, c("Once", "upon", "a", "time")))
  testthat::expect_equal(to_r(v), c("Once", "upon", "a", "time"))
})
testthat::test_that("vector assign boolean", {
  v <- cpp_vector(c(TRUE, FALSE, TRUE))
  testthat::expect_no_error(assign(v, c(FALSE, FALSE, FALSE, TRUE)))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, FALSE, TRUE))
})

# deque
testthat::test_that("deque assign integer", {
  v <- cpp_deque(4:9)
  testthat::expect_no_error(assign(v, 12:14))
  testthat::expect_equal(to_r(v), 12:14)
})
testthat::test_that("deque assign double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_no_error(assign(v, seq.int(3, 6, 0.5)))
  testthat::expect_equal(to_r(v), seq.int(3, 6, 0.5))
})
testthat::test_that("deque assign string", {
  v <- cpp_deque(c("hello", "there"))
  testthat::expect_no_error(assign(v, c("Once", "upon", "a", "time")))
  testthat::expect_equal(to_r(v), c("Once", "upon", "a", "time"))
})
testthat::test_that("deque assign boolean", {
  v <- cpp_deque(c(TRUE, FALSE, TRUE))
  testthat::expect_no_error(assign(v, c(FALSE, FALSE, FALSE, TRUE)))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, FALSE, TRUE))
})

# forward_list
testthat::test_that("forward_list assign integer", {
  v <- cpp_forward_list(4:9)
  testthat::expect_no_error(assign(v, 12:14))
  testthat::expect_equal(to_r(v), 12:14)
})
testthat::test_that("forward_list assign double", {
  v <- cpp_forward_list(seq.int(1, 2, 0.5))
  testthat::expect_no_error(assign(v, seq.int(3, 6, 0.5)))
  testthat::expect_equal(to_r(v), seq.int(3, 6, 0.5))
})
testthat::test_that("forward_list assign string", {
  v <- cpp_forward_list(c("hello", "there"))
  testthat::expect_no_error(assign(v, c("Once", "upon", "a", "time")))
  testthat::expect_equal(to_r(v), c("Once", "upon", "a", "time"))
})
testthat::test_that("forward_list assign boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE, TRUE))
  testthat::expect_no_error(assign(v, c(FALSE, FALSE, FALSE, TRUE)))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, FALSE, TRUE))
})

# list
testthat::test_that("list assign integer", {
  v <- cpp_list(4:9)
  testthat::expect_no_error(assign(v, 12:14))
  testthat::expect_equal(to_r(v), 12:14)
})
testthat::test_that("list assign double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_no_error(assign(v, seq.int(3, 6, 0.5)))
  testthat::expect_equal(to_r(v), seq.int(3, 6, 0.5))
})
testthat::test_that("list assign string", {
  v <- cpp_list(c("hello", "there"))
  testthat::expect_no_error(assign(v, c("Once", "upon", "a", "time")))
  testthat::expect_equal(to_r(v), c("Once", "upon", "a", "time"))
})
testthat::test_that("list assign boolean", {
  v <- cpp_list(c(TRUE, FALSE, TRUE))
  testthat::expect_no_error(assign(v, c(FALSE, FALSE, FALSE, TRUE)))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, FALSE, TRUE))
})
