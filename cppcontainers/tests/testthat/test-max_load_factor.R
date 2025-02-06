skip_on_os("mac")
tests <- function(v) {
  r <- testthat::expect_type(max_load_factor(v), "double")
  testthat::expect_length(r, 1L)
  testthat::expect_gt(r, 0)
  testthat::expect_true(is.finite(r))
  testthat::expect_invisible(max_load_factor(v, 3))
}

# unordered_set
testthat::test_that("unordered_set max_load_factor integer", {
  v <- cpp_unordered_set(4:9)
  tests(v)
})
testthat::test_that("unordered_set max_load_factor double", {
  v <- cpp_unordered_set(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_set max_load_factor string", {
  v <- cpp_unordered_set(c("hello", "there"))
  tests(v)
})
testthat::test_that("unordered_set max_load_factor boolean", {
  v <- cpp_unordered_set(c(TRUE, FALSE))
  tests(v)
})

# unordered_multiset
testthat::test_that("unordered_multiset max_load_factor integer", {
  v <- cpp_unordered_multiset(4:9)
  tests(v)
})
testthat::test_that("unordered_multiset max_load_factor double", {
  v <- cpp_unordered_multiset(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multiset max_load_factor string", {
  v <- cpp_unordered_multiset(c("hello", "there"))
  tests(v)
})
testthat::test_that("unordered_multiset max_load_factor boolean", {
  v <- cpp_unordered_multiset(c(TRUE, FALSE))
  tests(v)
})

# unordered_map
testthat::test_that("unordered_map max_load_factor integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  tests(v)
})
testthat::test_that("unordered_map max_load_factor integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map max_load_factor integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map max_load_factor integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map max_load_factor double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("unordered_map max_load_factor double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map max_load_factor double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map max_load_factor double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map max_load_factor string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("unordered_map max_load_factor string double", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map max_load_factor string string", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map max_load_factor string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map max_load_factor boolean integer", {
  v <- cpp_unordered_map(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("unordered_map max_load_factor boolean double", {
  v <- cpp_unordered_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map max_load_factor boolean string", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("unordered_map max_load_factor boolean boolean", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})

# unordered_multimap
testthat::test_that("unordered_multimap max_load_factor integer integer", {
  v <- cpp_unordered_multimap(4:9, 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor integer double", {
  v <- cpp_unordered_multimap(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor integer string", {
  v <- cpp_unordered_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor integer boolean", {
  v <- cpp_unordered_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor double integer", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor double double", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor double string", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor double boolean", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor string integer", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor string double", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor string string", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor string boolean", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor boolean integer", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor boolean double", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor boolean string", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("unordered_multimap max_load_factor boolean boolean", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})
