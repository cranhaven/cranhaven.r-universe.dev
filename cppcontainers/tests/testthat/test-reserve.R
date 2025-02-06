skip_on_os("mac")
# unordered_set
testthat::test_that("unordered_set reserve integer", {
  v <- cpp_unordered_set(4:6)
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_invisible(reserve(v, 0L))
})
testthat::test_that("unordered_set reserve double", {
  v <- cpp_unordered_set(seq.int(1, 2, 0.5))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_invisible(reserve(v, 0L))
})
testthat::test_that("unordered_set reserve string", {
  v <- cpp_unordered_set(c("hello", "there", "world"))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_invisible(reserve(v, 0L))
})
testthat::test_that("unordered_set reserve boolean", {
  v <- cpp_unordered_set(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_invisible(reserve(v, 0L))
})

# unordered_multiset
testthat::test_that("unordered_multiset reserve integer", {
  v <- cpp_unordered_multiset(4:6)
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_invisible(reserve(v, 0L))
})
testthat::test_that("unordered_multiset reserve double", {
  v <- cpp_unordered_multiset(seq.int(1, 2, 0.5))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_invisible(reserve(v, 0L))
})
testthat::test_that("unordered_multiset reserve string", {
  v <- cpp_unordered_multiset(c("hello", "there", "world"))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_invisible(reserve(v, 0L))
})
testthat::test_that("unordered_multiset reserve boolean", {
  v <- cpp_unordered_multiset(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_invisible(reserve(v, 0L))
})

# unordered_map
testthat::test_that("unordered_map reserve integer integer", {
  v <- cpp_unordered_map(4:6, 8:10)
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve integer double", {
  v <- cpp_unordered_map(4:6, seq.int(1, 2, 0.5))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve integer string", {
  v <- cpp_unordered_map(4:6, c("hello", "there", "world"))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve integer boolean", {
  v <- cpp_unordered_map(4:6, c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve double integer", {
  v <- cpp_unordered_map(seq.int(4, 4.2, 0.1), 8:10)
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve double double", {
  v <- cpp_unordered_map(seq.int(4, 4.2, 0.1), seq.int(1, 2, 0.5))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve double string", {
  v <- cpp_unordered_map(seq.int(4, 4.2, 0.1), c("hello", "there", "world"))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve double boolean", {
  v <- cpp_unordered_map(seq.int(4, 4.2, 0.1), c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve string integer", {
  v <- cpp_unordered_map(c("a", "quick", "test"), 8:10)
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve string double", {
  v <- cpp_unordered_map(c("a", "quick", "test"), seq.int(1, 2, 0.5))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve string string", {
  v <- cpp_unordered_map(c("a", "quick", "test"), c("hello", "there", "world"))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve string boolean", {
  v <- cpp_unordered_map(c("a", "quick", "test"), c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve boolean integer", {
  v <- cpp_unordered_map(c(TRUE, FALSE), 8:9)
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve boolean double", {
  v <- cpp_unordered_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve boolean string", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c("hello", "there"))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_map reserve boolean boolean", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c(TRUE, FALSE))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})

# unordered_multimap
testthat::test_that("unordered_multimap reserve integer integer", {
  v <- cpp_unordered_multimap(4:6, 8:10)
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve integer double", {
  v <- cpp_unordered_multimap(4:6, seq.int(1, 2, 0.5))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve integer string", {
  v <- cpp_unordered_multimap(4:6, c("hello", "there", "world"))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve integer boolean", {
  v <- cpp_unordered_multimap(4:6, c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve double integer", {
  v <- cpp_unordered_multimap(seq.int(4, 4.2, 0.1), 8:10)
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve double double", {
  v <- cpp_unordered_multimap(seq.int(4, 4.2, 0.1), seq.int(1, 2, 0.5))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve double string", {
  v <- cpp_unordered_multimap(seq.int(4, 4.2, 0.1), c("hello", "there", "world"))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve double boolean", {
  v <- cpp_unordered_multimap(seq.int(4, 4.2, 0.1), c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve string integer", {
  v <- cpp_unordered_multimap(c("a", "quick", "test"), 8:10)
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve string double", {
  v <- cpp_unordered_multimap(c("a", "quick", "test"), seq.int(1, 2, 0.5))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve string string", {
  v <- cpp_unordered_multimap(c("a", "quick", "test"), c("hello", "there", "world"))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve string boolean", {
  v <- cpp_unordered_multimap(c("a", "quick", "test"), c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve boolean integer", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), 8:9)
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve boolean double", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve boolean string", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c("hello", "there"))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("unordered_multimap reserve boolean boolean", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c(TRUE, FALSE))
  testthat::expect_invisible(reserve(v, 9L))
  testthat::expect_error(reserve(v, 0L))
})

# vector
testthat::test_that("vector reserve integer", {
  v <- cpp_vector(4:6)
  testthat::expect_invisible(reserve(v, 100L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("vector reserve double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_invisible(reserve(v, 100L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("vector reserve string", {
  v <- cpp_vector(c("hello", "there", "world"))
  testthat::expect_invisible(reserve(v, 100L))
  testthat::expect_error(reserve(v, 0L))
})
testthat::test_that("vector reserve boolean", {
  v <- cpp_vector(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(reserve(v, 100L))
  testthat::expect_error(reserve(v, 0L))
})
