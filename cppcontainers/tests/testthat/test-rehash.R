skip_on_os("mac")
# unordered_set
testthat::test_that("unordered_set rehash integer", {
  v <- cpp_unordered_set(4:6)
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_set rehash double", {
  v <- cpp_unordered_set(seq.int(1, 2, 0.5))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_set rehash string", {
  v <- cpp_unordered_set(c("hello", "there", "world"))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_set rehash boolean", {
  v <- cpp_unordered_set(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})

# unordered_multiset
testthat::test_that("unordered_multiset rehash integer", {
  v <- cpp_unordered_multiset(4:6)
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multiset rehash double", {
  v <- cpp_unordered_multiset(seq.int(1, 2, 0.5))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multiset rehash string", {
  v <- cpp_unordered_multiset(c("hello", "there", "world"))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multiset rehash boolean", {
  v <- cpp_unordered_multiset(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})

# unordered_map
testthat::test_that("unordered_map rehash integer integer", {
  v <- cpp_unordered_map(4:6, 8:10)
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash integer double", {
  v <- cpp_unordered_map(4:6, seq.int(1, 2, 0.5))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash integer string", {
  v <- cpp_unordered_map(4:6, c("hello", "there", "world"))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash integer boolean", {
  v <- cpp_unordered_map(4:6, c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash double integer", {
  v <- cpp_unordered_map(seq.int(4, 4.2, 0.1), 8:10)
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash double double", {
  v <- cpp_unordered_map(seq.int(4, 4.2, 0.1), seq.int(1, 2, 0.5))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash double string", {
  v <- cpp_unordered_map(seq.int(4, 4.2, 0.1), c("hello", "there", "world"))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash double boolean", {
  v <- cpp_unordered_map(seq.int(4, 4.2, 0.1), c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash string integer", {
  v <- cpp_unordered_map(c("a", "quick", "test"), 8:10)
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash string double", {
  v <- cpp_unordered_map(c("a", "quick", "test"), seq.int(1, 2, 0.5))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash string string", {
  v <- cpp_unordered_map(c("a", "quick", "test"), c("hello", "there", "world"))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash string boolean", {
  v <- cpp_unordered_map(c("a", "quick", "test"), c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash boolean integer", {
  v <- cpp_unordered_map(c(TRUE, FALSE), 8:9)
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash boolean double", {
  v <- cpp_unordered_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash boolean string", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c("hello", "there"))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_map rehash boolean boolean", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c(TRUE, FALSE))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})

# unordered_multimap
testthat::test_that("unordered_multimap rehash integer integer", {
  v <- cpp_unordered_multimap(4:6, 8:10)
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash integer double", {
  v <- cpp_unordered_multimap(4:6, seq.int(1, 2, 0.5))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash integer string", {
  v <- cpp_unordered_multimap(4:6, c("hello", "there", "world"))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash integer boolean", {
  v <- cpp_unordered_multimap(4:6, c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash double integer", {
  v <- cpp_unordered_multimap(seq.int(4, 4.2, 0.1), 8:10)
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash double double", {
  v <- cpp_unordered_multimap(seq.int(4, 4.2, 0.1), seq.int(1, 2, 0.5))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash double string", {
  v <- cpp_unordered_multimap(seq.int(4, 4.2, 0.1), c("hello", "there", "world"))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash double boolean", {
  v <- cpp_unordered_multimap(seq.int(4, 4.2, 0.1), c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash string integer", {
  v <- cpp_unordered_multimap(c("a", "quick", "test"), 8:10)
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash string double", {
  v <- cpp_unordered_multimap(c("a", "quick", "test"), seq.int(1, 2, 0.5))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash string string", {
  v <- cpp_unordered_multimap(c("a", "quick", "test"), c("hello", "there", "world"))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash string boolean", {
  v <- cpp_unordered_multimap(c("a", "quick", "test"), c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash boolean integer", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), 8:9)
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash boolean double", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash boolean string", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c("hello", "there"))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
testthat::test_that("unordered_multimap rehash boolean boolean", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c(TRUE, FALSE))
  testthat::expect_invisible(rehash(v, 9L))
  testthat::expect_invisible(rehash(v, 0L))
})
