skip_on_os("mac")
tests <- function(v) {
  r <- testthat::expect_type(max_size(v), "double")
  testthat::expect_length(r, 1L)
  testthat::expect_gt(r, 0)
  testthat::expect_true(is.finite(r))
}

# set
testthat::test_that("set max_size integer", {
  v <- cpp_set(4:9)
  tests(v)
})
testthat::test_that("set max_size double", {
  v <- cpp_set(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("set max_size string", {
  v <- cpp_set(c("hello", "there"))
  tests(v)
})
testthat::test_that("set max_size boolean", {
  v <- cpp_set(c(TRUE, FALSE))
  tests(v)
})

# unordered_set
testthat::test_that("unordered_set max_size integer", {
  v <- cpp_unordered_set(4:9)
  tests(v)
})
testthat::test_that("unordered_set max_size double", {
  v <- cpp_unordered_set(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_set max_size string", {
  v <- cpp_unordered_set(c("hello", "there"))
  tests(v)
})
testthat::test_that("unordered_set max_size boolean", {
  v <- cpp_unordered_set(c(TRUE, FALSE))
  tests(v)
})

# multiset
testthat::test_that("multiset max_size integer", {
  v <- cpp_multiset(4:9)
  tests(v)
})
testthat::test_that("multiset max_size double", {
  v <- cpp_multiset(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("multiset max_size string", {
  v <- cpp_multiset(c("hello", "there"))
  tests(v)
})
testthat::test_that("multiset max_size boolean", {
  v <- cpp_multiset(c(TRUE, FALSE))
  tests(v)
})

# unordered_multiset
testthat::test_that("unordered_multiset max_size integer", {
  v <- cpp_unordered_multiset(4:9)
  tests(v)
})
testthat::test_that("unordered_multiset max_size double", {
  v <- cpp_unordered_multiset(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multiset max_size string", {
  v <- cpp_unordered_multiset(c("hello", "there"))
  tests(v)
})
testthat::test_that("unordered_multiset max_size boolean", {
  v <- cpp_unordered_multiset(c(TRUE, FALSE))
  tests(v)
})

# map
testthat::test_that("map max_size integer integer", {
  v <- cpp_map(4:9, 12:17)
  tests(v)
})
testthat::test_that("map max_size integer double", {
  v <- cpp_map(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("map max_size integer string", {
  v <- cpp_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("map max_size integer boolean", {
  v <- cpp_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("map max_size double integer", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("map max_size double double", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("map max_size double string", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("map max_size double boolean", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("map max_size string integer", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("map max_size string double", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("map max_size string string", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("map max_size string boolean", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("map max_size boolean integer", {
  v <- cpp_map(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("map max_size boolean double", {
  v <- cpp_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("map max_size boolean string", {
  v <- cpp_map(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("map max_size boolean boolean", {
  v <- cpp_map(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})

# unordered_map
testthat::test_that("unordered_map max_size integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  tests(v)
})
testthat::test_that("unordered_map max_size integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map max_size integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map max_size integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map max_size double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("unordered_map max_size double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map max_size double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map max_size double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map max_size string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("unordered_map max_size string double", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map max_size string string", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map max_size string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map max_size boolean integer", {
  v <- cpp_unordered_map(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("unordered_map max_size boolean double", {
  v <- cpp_unordered_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map max_size boolean string", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("unordered_map max_size boolean boolean", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})

# multimap
testthat::test_that("multimap max_size integer integer", {
  v <- cpp_multimap(4:9, 12:17)
  tests(v)
})
testthat::test_that("multimap max_size integer double", {
  v <- cpp_multimap(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("multimap max_size integer string", {
  v <- cpp_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("multimap max_size integer boolean", {
  v <- cpp_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("multimap max_size double integer", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("multimap max_size double double", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("multimap max_size double string", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("multimap max_size double boolean", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("multimap max_size string integer", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("multimap max_size string double", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("multimap max_size string string", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("multimap max_size string boolean", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("multimap max_size boolean integer", {
  v <- cpp_multimap(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("multimap max_size boolean double", {
  v <- cpp_multimap(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("multimap max_size boolean string", {
  v <- cpp_multimap(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("multimap max_size boolean boolean", {
  v <- cpp_multimap(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})

# unordered_multimap
testthat::test_that("unordered_multimap max_size integer integer", {
  v <- cpp_unordered_multimap(4:9, 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap max_size integer double", {
  v <- cpp_unordered_multimap(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap max_size integer string", {
  v <- cpp_unordered_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap max_size integer boolean", {
  v <- cpp_unordered_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap max_size double integer", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap max_size double double", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap max_size double string", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap max_size double boolean", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap max_size string integer", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap max_size string double", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap max_size string string", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap max_size string boolean", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap max_size boolean integer", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("unordered_multimap max_size boolean double", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap max_size boolean string", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("unordered_multimap max_size boolean boolean", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})

# vector
testthat::test_that("vector max_size integer", {
  v <- cpp_vector(4:9)
  tests(v)
})
testthat::test_that("vector max_size double", {
  v <- cpp_vector(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("vector max_size string", {
  v <- cpp_vector(c("hello", "there"))
  tests(v)
})
testthat::test_that("vector max_size boolean", {
  v <- cpp_vector(c(TRUE, FALSE))
  tests(v)
})

# deque
testthat::test_that("deque max_size integer", {
  v <- cpp_deque(4:9)
  tests(v)
})
testthat::test_that("deque max_size double", {
  v <- cpp_deque(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("deque max_size string", {
  v <- cpp_deque(c("hello", "there"))
  tests(v)
})
testthat::test_that("deque max_size boolean", {
  v <- cpp_deque(c(TRUE, FALSE))
  tests(v)
})

# forward_list
testthat::test_that("forward_list max_size integer", {
  v <- cpp_forward_list(4:9)
  tests(v)
})
testthat::test_that("forward_list max_size double", {
  v <- cpp_forward_list(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("forward_list max_size string", {
  v <- cpp_forward_list(c("hello", "there"))
  tests(v)
})
testthat::test_that("forward_list max_size boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE))
  tests(v)
})

# list
testthat::test_that("list max_size integer", {
  v <- cpp_list(4:9)
  tests(v)
})
testthat::test_that("list max_size double", {
  v <- cpp_list(seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("list max_size string", {
  v <- cpp_list(c("hello", "there"))
  tests(v)
})
testthat::test_that("list max_size boolean", {
  v <- cpp_list(c(TRUE, FALSE))
  tests(v)
})
