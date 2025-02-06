skip_on_os("mac")
# set
testthat::test_that("set size integer", {
  v <- cpp_set(4:9)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("set size double", {
  v <- cpp_set(seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("set size string", {
  v <- cpp_set(c("hello", "there"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("set size boolean", {
  v <- cpp_set(c(TRUE, FALSE))
 testthat::expect_equal(size(v), 2)
})

# unordered_set
testthat::test_that("unordered_set size integer", {
  v <- cpp_unordered_set(4:9)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_set size double", {
  v <- cpp_unordered_set(seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_set size string", {
  v <- cpp_unordered_set(c("hello", "there"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("unordered_set size boolean", {
  v <- cpp_unordered_set(c(TRUE, FALSE))
 testthat::expect_equal(size(v), 2)
})

# multiset
testthat::test_that("multiset size integer", {
  v <- cpp_multiset(4:9)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multiset size double", {
  v <- cpp_multiset(seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multiset size string", {
  v <- cpp_multiset(c("hello", "there"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("multiset size boolean", {
  v <- cpp_multiset(c(TRUE, FALSE))
 testthat::expect_equal(size(v), 2)
})

# unordered_multiset
testthat::test_that("unordered_multiset size integer", {
  v <- cpp_unordered_multiset(4:9)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multiset size double", {
  v <- cpp_unordered_multiset(seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multiset size string", {
  v <- cpp_unordered_multiset(c("hello", "there"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("unordered_multiset size boolean", {
  v <- cpp_unordered_multiset(c(TRUE, FALSE))
 testthat::expect_equal(size(v), 2)
})

# map
testthat::test_that("map size integer integer", {
  v <- cpp_map(4:9, 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size integer double", {
  v <- cpp_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size integer string", {
  v <- cpp_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size integer boolean", {
  v <- cpp_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size double integer", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size double double", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size double string", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size double boolean", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size string integer", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size string double", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size string string", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size string boolean", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("map size boolean integer", {
  v <- cpp_map(c(TRUE, FALSE), 12:13)
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("map size boolean double", {
  v <- cpp_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("map size boolean string", {
  v <- cpp_map(c(TRUE, FALSE), c("Once", "upon"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("map size boolean boolean", {
  v <- cpp_map(c(TRUE, FALSE), c(FALSE, TRUE))
  testthat::expect_equal(size(v), 2)
})

# unordered_map
testthat::test_that("unordered_map size integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size string double", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size string string", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_map size boolean integer", {
  v <- cpp_unordered_map(c(TRUE, FALSE), 12:13)
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("unordered_map size boolean double", {
  v <- cpp_unordered_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("unordered_map size boolean string", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c("Once", "upon"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("unordered_map size boolean boolean", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c(FALSE, TRUE))
  testthat::expect_equal(size(v), 2)
})

# multimap
testthat::test_that("multimap size integer integer", {
  v <- cpp_multimap(4:9, 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size integer double", {
  v <- cpp_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size integer string", {
  v <- cpp_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size integer boolean", {
  v <- cpp_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size double integer", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size double double", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size double string", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size double boolean", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size string integer", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size string double", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size string string", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size string boolean", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("multimap size boolean integer", {
  v <- cpp_multimap(c(TRUE, FALSE), 12:13)
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("multimap size boolean double", {
  v <- cpp_multimap(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("multimap size boolean string", {
  v <- cpp_multimap(c(TRUE, FALSE), c("Once", "upon"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("multimap size boolean boolean", {
  v <- cpp_multimap(c(TRUE, FALSE), c(FALSE, TRUE))
  testthat::expect_equal(size(v), 2)
})

# unordered_multimap
testthat::test_that("unordered_multimap size integer integer", {
  v <- cpp_unordered_multimap(4:9, 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size integer double", {
  v <- cpp_unordered_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size integer string", {
  v <- cpp_unordered_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size integer boolean", {
  v <- cpp_unordered_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size double integer", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size double double", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size double string", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size double boolean", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size string integer", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size string double", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size string string", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size string boolean", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("unordered_multimap size boolean integer", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), 12:13)
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("unordered_multimap size boolean double", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("unordered_multimap size boolean string", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c("Once", "upon"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("unordered_multimap size boolean boolean", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c(FALSE, TRUE))
  testthat::expect_equal(size(v), 2)
})

# stack
testthat::test_that("stack size integer", {
  v <- cpp_stack(4:9)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("stack size double", {
  v <- cpp_stack(seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("stack size string", {
  v <- cpp_stack(c("hello", "there"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("stack size boolean", {
  v <- cpp_stack(c(TRUE, FALSE))
 testthat::expect_equal(size(v), 2)
})

# queue
testthat::test_that("queue size integer", {
  v <- cpp_queue(4:9)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("queue size double", {
  v <- cpp_queue(seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("queue size string", {
  v <- cpp_queue(c("hello", "there"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("queue size boolean", {
  v <- cpp_queue(c(TRUE, FALSE))
 testthat::expect_equal(size(v), 2)
})

# priority_queue
testthat::test_that("priority_queue size integer descending", {
  v <- cpp_priority_queue(4:9)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("priority_queue size double descending", {
  v <- cpp_priority_queue(seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("priority_queue size string descending", {
  v <- cpp_priority_queue(c("hello", "there"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("priority_queue size boolean descending", {
  v <- cpp_priority_queue(c(TRUE, FALSE))
 testthat::expect_equal(size(v), 2)
})
testthat::test_that("priority_queue size integer ascending", {
  v <- cpp_priority_queue(4:9, "ascending")
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("priority_queue size double ascending", {
  v <- cpp_priority_queue(seq.int(1, 3.5, 0.5), "ascending")
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("priority_queue size string ascending", {
  v <- cpp_priority_queue(c("hello", "there"), "ascending")
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("priority_queue size boolean ascending", {
  v <- cpp_priority_queue(c(TRUE, FALSE), "ascending")
 testthat::expect_equal(size(v), 2)
})

# vector
testthat::test_that("vector size integer", {
  v <- cpp_vector(4:9)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("vector size double", {
  v <- cpp_vector(seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("vector size string", {
  v <- cpp_vector(c("hello", "there"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("vector size boolean", {
  v <- cpp_vector(c(TRUE, FALSE))
 testthat::expect_equal(size(v), 2)
})

# deque
testthat::test_that("deque size integer", {
  v <- cpp_deque(4:9)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("deque size double", {
  v <- cpp_deque(seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("deque size string", {
  v <- cpp_deque(c("hello", "there"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("deque size boolean", {
  v <- cpp_deque(c(TRUE, FALSE))
 testthat::expect_equal(size(v), 2)
})

# list
testthat::test_that("list size integer", {
  v <- cpp_list(4:9)
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("list size double", {
  v <- cpp_list(seq.int(1, 3.5, 0.5))
  testthat::expect_equal(size(v), 6)
})
testthat::test_that("list size string", {
  v <- cpp_list(c("hello", "there"))
  testthat::expect_equal(size(v), 2)
})
testthat::test_that("list size boolean", {
  v <- cpp_list(c(TRUE, FALSE))
 testthat::expect_equal(size(v), 2)
})
