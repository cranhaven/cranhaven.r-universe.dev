skip_on_os("mac")
# set
testthat::test_that("set contains integer", {
  v <- cpp_set(4:9)
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("set contains double", {
  v <- cpp_set(seq.int(1, 2, 0.5))
  testthat::expect_true(contains(v, 1))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("set contains string", {
  v <- cpp_set(c("hello", "there"))
  testthat::expect_true(contains(v, "there"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("set contains boolean", {
  v <- cpp_set(TRUE)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})

# unordered_set
testthat::test_that("unordered_set contains integer", {
  v <- cpp_unordered_set(4:9)
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("unordered_set contains double", {
  v <- cpp_unordered_set(seq.int(1, 2, 0.5))
  testthat::expect_true(contains(v, 1))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("unordered_set contains string", {
  v <- cpp_unordered_set(c("hello", "there"))
  testthat::expect_true(contains(v, "there"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("unordered_set contains boolean", {
  v <- cpp_unordered_set(TRUE)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})

# multiset
testthat::test_that("multiset contains integer", {
  v <- cpp_multiset(4:9)
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("multiset contains double", {
  v <- cpp_multiset(seq.int(1, 2, 0.5))
  testthat::expect_true(contains(v, 1))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("multiset contains string", {
  v <- cpp_multiset(c("hello", "there"))
  testthat::expect_true(contains(v, "there"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("multiset contains boolean", {
  v <- cpp_multiset(TRUE)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})

# unordered_multiset
testthat::test_that("unordered_multiset contains integer", {
  v <- cpp_unordered_multiset(4:9)
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("unordered_multiset contains double", {
  v <- cpp_unordered_multiset(seq.int(1, 2, 0.5))
  testthat::expect_true(contains(v, 1))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("unordered_multiset contains string", {
  v <- cpp_unordered_multiset(c("hello", "there"))
  testthat::expect_true(contains(v, "there"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("unordered_multiset contains boolean", {
  v <- cpp_unordered_multiset(TRUE)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})

# map
testthat::test_that("map contains integer integer", {
  v <- cpp_map(4:9, 12:17)
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("map contains integer double", {
  v <- cpp_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("map contains integer string", {
  v <- cpp_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("map contains integer boolean", {
  v <- cpp_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("map contains double integer", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("map contains double double", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("map contains double string", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("map contains double boolean", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("map contains string integer", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("map contains string double", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("map contains string string", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("map contains string boolean", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("map contains boolean integer", {
  v <- cpp_map(TRUE, 12L)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("map contains boolean double", {
  v <- cpp_map(TRUE, 1.5)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("map contains boolean string", {
  v <- cpp_map(TRUE, "upon")
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("map contains boolean boolean", {
  v <- cpp_map(TRUE, FALSE)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})

# unordered_map
testthat::test_that("unordered_map contains integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("unordered_map contains integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("unordered_map contains integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("unordered_map contains integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("unordered_map contains double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("unordered_map contains double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("unordered_map contains double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("unordered_map contains double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("unordered_map contains string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("unordered_map contains string double", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("unordered_map contains string string", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("unordered_map contains string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("unordered_map contains boolean integer", {
  v <- cpp_unordered_map(TRUE, 12L)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("unordered_map contains boolean double", {
  v <- cpp_unordered_map(TRUE, 1.5)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("unordered_map contains boolean string", {
  v <- cpp_unordered_map(TRUE, "upon")
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("unordered_map contains boolean boolean", {
  v <- cpp_unordered_map(TRUE, FALSE)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})

# multimap
testthat::test_that("multimap contains integer integer", {
  v <- cpp_multimap(4:9, 12:17)
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("multimap contains integer double", {
  v <- cpp_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("multimap contains integer string", {
  v <- cpp_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("multimap contains integer boolean", {
  v <- cpp_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("multimap contains double integer", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("multimap contains double double", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("multimap contains double string", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("multimap contains double boolean", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("multimap contains string integer", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("multimap contains string double", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("multimap contains string string", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("multimap contains string boolean", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("multimap contains boolean integer", {
  v <- cpp_multimap(TRUE, 12L)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("multimap contains boolean double", {
  v <- cpp_multimap(TRUE, 1.5)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("multimap contains boolean string", {
  v <- cpp_multimap(TRUE, "upon")
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("multimap contains boolean boolean", {
  v <- cpp_multimap(TRUE, FALSE)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})

# unordered_multimap
testthat::test_that("unordered_multimap contains integer integer", {
  v <- cpp_unordered_multimap(4:9, 12:17)
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("unordered_multimap contains integer double", {
  v <- cpp_unordered_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("unordered_multimap contains integer string", {
  v <- cpp_unordered_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("unordered_multimap contains integer boolean", {
  v <- cpp_unordered_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, 7L))
  testthat::expect_false(contains(v, 10L))
})
testthat::test_that("unordered_multimap contains double integer", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("unordered_multimap contains double double", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("unordered_multimap contains double string", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("unordered_multimap contains double boolean", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, 3))
  testthat::expect_false(contains(v, 10))
})
testthat::test_that("unordered_multimap contains string integer", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("unordered_multimap contains string double", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("unordered_multimap contains string string", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("unordered_multimap contains string boolean", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_true(contains(v, "the"))
  testthat::expect_false(contains(v, "world"))
})
testthat::test_that("unordered_multimap contains boolean integer", {
  v <- cpp_unordered_multimap(TRUE, 12L)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("unordered_multimap contains boolean double", {
  v <- cpp_unordered_multimap(TRUE, 1.5)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("unordered_multimap contains boolean string", {
  v <- cpp_unordered_multimap(TRUE, "upon")
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
testthat::test_that("unordered_multimap contains boolean boolean", {
  v <- cpp_unordered_multimap(TRUE, FALSE)
  testthat::expect_true(contains(v, TRUE))
  testthat::expect_false(contains(v, FALSE))
})
