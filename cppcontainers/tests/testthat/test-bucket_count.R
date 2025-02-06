skip_on_os("mac")
tests <- function(v) {
  r <- testthat::expect_type(bucket_count(v), "double")
  testthat::expect_length(r, 1L)
  testthat::expect_gt(r, 0)
  testthat::expect_true(is.finite(r))
}

# unordered_set
testthat::test_that("unordered_set bucket_count integer", {
  v <- cpp_unordered_set(4:9)
  tests(v)
})
testthat::test_that("unordered_set bucket_count double", {
  v <- cpp_unordered_set(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_set bucket_count string", {
  v <- cpp_unordered_set(c("hello", "there"))
  tests(v)
})
testthat::test_that("unordered_set bucket_count boolean", {
  v <- cpp_unordered_set(c(TRUE, FALSE))
  tests(v)
})

# unordered_multiset
testthat::test_that("unordered_multiset bucket_count integer", {
  v <- cpp_unordered_multiset(4:9)
  tests(v)
})
testthat::test_that("unordered_multiset bucket_count double", {
  v <- cpp_unordered_multiset(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multiset bucket_count string", {
  v <- cpp_unordered_multiset(c("hello", "there"))
  tests(v)
})
testthat::test_that("unordered_multiset bucket_count boolean", {
  v <- cpp_unordered_multiset(c(TRUE, FALSE))
  tests(v)
})

# unordered_map
testthat::test_that("unordered_map bucket_count integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  tests(v)
})
testthat::test_that("unordered_map bucket_count integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map bucket_count integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map bucket_count integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map bucket_count double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("unordered_map bucket_count double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map bucket_count double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map bucket_count double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map bucket_count string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("unordered_map bucket_count string double", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map bucket_count string string", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map bucket_count string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map bucket_count boolean integer", {
  v <- cpp_unordered_map(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("unordered_map bucket_count boolean double", {
  v <- cpp_unordered_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map bucket_count boolean string", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("unordered_map bucket_count boolean boolean", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})

# unordered_multimap
testthat::test_that("unordered_multimap bucket_count integer integer", {
  v <- cpp_unordered_multimap(4:9, 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count integer double", {
  v <- cpp_unordered_multimap(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count integer string", {
  v <- cpp_unordered_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count integer boolean", {
  v <- cpp_unordered_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count double integer", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count double double", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count double string", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count double boolean", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count string integer", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count string double", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count string string", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count string boolean", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count boolean integer", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count boolean double", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count boolean string", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("unordered_multimap bucket_count boolean boolean", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})
