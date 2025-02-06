skip_on_os("mac")
tests <- function(v) {
  r <- testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
}

# set
testthat::test_that("set clear integer", {
  v <- cpp_set(4:9)
  tests(v)
})
testthat::test_that("set clear double", {
  v <- cpp_set(seq.int(1, 2, 0.5))
  tests(v)
})
testthat::test_that("set clear string", {
  v <- cpp_set(c("hello", "there"))
  tests(v)
})
testthat::test_that("set clear boolean", {
  v <- cpp_set(c(TRUE, FALSE))
  tests(v)
})

# unordered_set
testthat::test_that("unordered_set clear integer", {
  v <- cpp_unordered_set(4:9)
  tests(v)
})
testthat::test_that("unordered_set clear double", {
  v <- cpp_unordered_set(seq.int(1, 2, 0.5))
  tests(v)
})
testthat::test_that("unordered_set clear string", {
  v <- cpp_unordered_set(c("hello", "there"))
  tests(v)
})
testthat::test_that("unordered_set clear boolean", {
  v <- cpp_unordered_set(c(TRUE, FALSE))
  tests(v)
})

# multiset
testthat::test_that("multiset clear integer", {
  v <- cpp_multiset(4:9)
  tests(v)
})
testthat::test_that("multiset clear double", {
  v <- cpp_multiset(seq.int(1, 2, 0.5))
  tests(v)
})
testthat::test_that("multiset clear string", {
  v <- cpp_multiset(c("hello", "there"))
  tests(v)
})
testthat::test_that("multiset clear boolean", {
  v <- cpp_multiset(c(TRUE, FALSE))
  tests(v)
})

# unordered_multiset
testthat::test_that("unordered_multiset clear integer", {
  v <- cpp_unordered_multiset(4:9)
  tests(v)
})
testthat::test_that("unordered_multiset clear double", {
  v <- cpp_unordered_multiset(seq.int(1, 2, 0.5))
  tests(v)
})
testthat::test_that("unordered_multiset clear string", {
  v <- cpp_unordered_multiset(c("hello", "there"))
  tests(v)
})
testthat::test_that("unordered_multiset clear boolean", {
  v <- cpp_unordered_multiset(c(TRUE, FALSE))
  tests(v)
})

# map
testthat::test_that("map clear integer integer", {
  v <- cpp_map(4:9, 12:17)
  tests(v)
})
testthat::test_that("map clear integer double", {
  v <- cpp_map(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("map clear integer string", {
  v <- cpp_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("map clear integer boolean", {
  v <- cpp_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("map clear double integer", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("map clear double double", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("map clear double string", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("map clear double boolean", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("map clear string integer", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("map clear string double", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("map clear string string", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("map clear string boolean", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("map clear boolean integer", {
  v <- cpp_map(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("map clear boolean double", {
  v <- cpp_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("map clear boolean string", {
  v <- cpp_map(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("map clear boolean boolean", {
  v <- cpp_map(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})

# unordered_map
testthat::test_that("unordered_map clear integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  tests(v)
})
testthat::test_that("unordered_map clear integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map clear integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map clear integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map clear double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("unordered_map clear double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map clear double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map clear double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map clear string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("unordered_map clear string double", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map clear string string", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_map clear string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_map clear boolean integer", {
  v <- cpp_unordered_map(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("unordered_map clear boolean double", {
  v <- cpp_unordered_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_map clear boolean string", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("unordered_map clear boolean boolean", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})

# multimap
testthat::test_that("multimap clear integer integer", {
  v <- cpp_multimap(4:9, 12:17)
  tests(v)
})
testthat::test_that("multimap clear integer double", {
  v <- cpp_multimap(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("multimap clear integer string", {
  v <- cpp_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("multimap clear integer boolean", {
  v <- cpp_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("multimap clear double integer", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("multimap clear double double", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("multimap clear double string", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("multimap clear double boolean", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("multimap clear string integer", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("multimap clear string double", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("multimap clear string string", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("multimap clear string boolean", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("multimap clear boolean integer", {
  v <- cpp_multimap(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("multimap clear boolean double", {
  v <- cpp_multimap(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("multimap clear boolean string", {
  v <- cpp_multimap(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("multimap clear boolean boolean", {
  v <- cpp_multimap(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})

# unordered_multimap
testthat::test_that("unordered_multimap clear integer integer", {
  v <- cpp_unordered_multimap(4:9, 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap clear integer double", {
  v <- cpp_unordered_multimap(4:9, seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap clear integer string", {
  v <- cpp_unordered_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap clear integer boolean", {
  v <- cpp_unordered_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap clear double integer", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap clear double double", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap clear double string", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap clear double boolean", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap clear string integer", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  tests(v)
})
testthat::test_that("unordered_multimap clear string double", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap clear string string", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  tests(v)
})
testthat::test_that("unordered_multimap clear string boolean", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  tests(v)
})
testthat::test_that("unordered_multimap clear boolean integer", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), 12:13)
  tests(v)
})
testthat::test_that("unordered_multimap clear boolean double", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  tests(v)
})
testthat::test_that("unordered_multimap clear boolean string", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c("Once", "upon"))
  tests(v)
})
testthat::test_that("unordered_multimap clear boolean boolean", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c(FALSE, TRUE))
  tests(v)
})

# vector
testthat::test_that("vector clear integer", {
  v <- cpp_vector(4:9)
  tests(v)
})
testthat::test_that("vector clear double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  tests(v)
})
testthat::test_that("vector clear string", {
  v <- cpp_vector(c("hello", "there"))
  tests(v)
})
testthat::test_that("vector clear boolean", {
  v <- cpp_vector(c(TRUE, FALSE))
  tests(v)
})

# deque
testthat::test_that("deque clear integer", {
  v <- cpp_deque(4:9)
  tests(v)
})
testthat::test_that("deque clear double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  tests(v)
})
testthat::test_that("deque clear string", {
  v <- cpp_deque(c("hello", "there"))
  tests(v)
})
testthat::test_that("deque clear boolean", {
  v <- cpp_deque(c(TRUE, FALSE))
  tests(v)
})

# forward_list
testthat::test_that("forward_list clear integer", {
  v <- cpp_forward_list(4:9)
  tests(v)
})
testthat::test_that("forward_list clear double", {
  v <- cpp_forward_list(seq.int(1, 2, 0.5))
  tests(v)
})
testthat::test_that("forward_list clear string", {
  v <- cpp_forward_list(c("hello", "there"))
  tests(v)
})
testthat::test_that("forward_list clear boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE))
  tests(v)
})

# list
testthat::test_that("list clear integer", {
  v <- cpp_list(4:9)
  tests(v)
})
testthat::test_that("list clear double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  tests(v)
})
testthat::test_that("list clear string", {
  v <- cpp_list(c("hello", "there"))
  tests(v)
})
testthat::test_that("list clear boolean", {
  v <- cpp_list(c(TRUE, FALSE))
  tests(v)
})
