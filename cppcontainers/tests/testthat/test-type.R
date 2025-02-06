skip_on_os("mac")
# set
testthat::test_that("set type integer", {
  v <- cpp_set(4:6)
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("set type double", {
  v <- cpp_set(seq.int(1, 2, 0.5))
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("set type string", {
  v <- cpp_set(c("hello", "there", "world"))
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("set type boolean", {
  v <- cpp_set(c(TRUE, FALSE))
  testthat::expect_equal(type(v), "boolean")
})

# unordered_set
testthat::test_that("unordered_set type integer", {
  v <- cpp_unordered_set(4:6)
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("unordered_set type double", {
  v <- cpp_unordered_set(seq.int(1, 2, 0.5))
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("unordered_set type string", {
  v <- cpp_unordered_set(c("hello", "there", "world"))
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("unordered_set type boolean", {
  v <- cpp_unordered_set(c(TRUE, FALSE))
  testthat::expect_equal(type(v), "boolean")
})

# multiset
testthat::test_that("multiset type integer", {
  v <- cpp_multiset(4:6)
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("multiset type double", {
  v <- cpp_multiset(seq.int(1, 2, 0.5))
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("multiset type string", {
  v <- cpp_multiset(c("hello", "there", "world"))
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("multiset type boolean", {
  v <- cpp_multiset(c(TRUE, FALSE))
  testthat::expect_equal(type(v), "boolean")
})

# unordered_multiset
testthat::test_that("unordered_multiset type integer", {
  v <- cpp_unordered_multiset(4:6)
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("unordered_multiset type double", {
  v <- cpp_unordered_multiset(seq.int(1, 2, 0.5))
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("unordered_multiset type string", {
  v <- cpp_unordered_multiset(c("hello", "there", "world"))
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("unordered_multiset type boolean", {
  v <- cpp_unordered_multiset(c(TRUE, FALSE))
  testthat::expect_equal(type(v), "boolean")
})

# map
testthat::test_that("map try_emplace integer integer", {
  v <- cpp_map(4:9, 12:17)
  testthat::expect_equal(type(v), c(key = "integer", value = "integer"))
})
testthat::test_that("map try_emplace integer double", {
  v <- cpp_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "integer", value = "double"))
})
testthat::test_that("map try_emplace integer string", {
  v <- cpp_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "integer", value = "string"))
})
testthat::test_that("map try_emplace integer boolean", {
  v <- cpp_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "integer", value = "boolean"))
})
testthat::test_that("map try_emplace double integer", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(type(v), c(key = "double", value = "integer"))
})
testthat::test_that("map try_emplace double double", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "double", value = "double"))
})
testthat::test_that("map try_emplace double string", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "double", value = "string"))
})
testthat::test_that("map try_emplace double boolean", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "double", value = "boolean"))
})
testthat::test_that("map try_emplace string integer", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(type(v), c(key = "string", value = "integer"))
})
testthat::test_that("map try_emplace string double", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "string", value = "double"))
})
testthat::test_that("map try_emplace string string", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "string", value = "string"))
})
testthat::test_that("map try_emplace string boolean", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "string", value = "boolean"))
})
testthat::test_that("map try_emplace boolean integer", {
  v <- cpp_map(TRUE, 12L)
  testthat::expect_equal(type(v), c(key = "boolean", value = "integer"))
})
testthat::test_that("map try_emplace boolean double", {
  v <- cpp_map(TRUE, 1.5)
  testthat::expect_equal(type(v), c(key = "boolean", value = "double"))
})
testthat::test_that("map try_emplace boolean string", {
  v <- cpp_map(TRUE, "upon")
  testthat::expect_equal(type(v), c(key = "boolean", value = "string"))
})
testthat::test_that("map try_emplace boolean boolean", {
  v <- cpp_map(TRUE, FALSE)
  testthat::expect_equal(type(v), c(key = "boolean", value = "boolean"))
})

# unordered_map
testthat::test_that("unordered_map try_emplace integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  testthat::expect_equal(type(v), c(key = "integer", value = "integer"))
})
testthat::test_that("unordered_map try_emplace integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "integer", value = "double"))
})
testthat::test_that("unordered_map try_emplace integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "integer", value = "string"))
})
testthat::test_that("unordered_map try_emplace integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "integer", value = "boolean"))
})
testthat::test_that("unordered_map try_emplace double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(type(v), c(key = "double", value = "integer"))
})
testthat::test_that("unordered_map try_emplace double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "double", value = "double"))
})
testthat::test_that("unordered_map try_emplace double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "double", value = "string"))
})
testthat::test_that("unordered_map try_emplace double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "double", value = "boolean"))
})
testthat::test_that("unordered_map try_emplace string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(type(v), c(key = "string", value = "integer"))
})
testthat::test_that("unordered_map try_emplace string double", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "string", value = "double"))
})
testthat::test_that("unordered_map try_emplace string string", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "string", value = "string"))
})
testthat::test_that("unordered_map try_emplace string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "string", value = "boolean"))
})
testthat::test_that("unordered_map try_emplace boolean integer", {
  v <- cpp_unordered_map(TRUE, 12L)
  testthat::expect_equal(type(v), c(key = "boolean", value = "integer"))
})
testthat::test_that("unordered_map try_emplace boolean double", {
  v <- cpp_unordered_map(TRUE, 1.5)
  testthat::expect_equal(type(v), c(key = "boolean", value = "double"))
})
testthat::test_that("unordered_map try_emplace boolean string", {
  v <- cpp_unordered_map(TRUE, "upon")
  testthat::expect_equal(type(v), c(key = "boolean", value = "string"))
})
testthat::test_that("unordered_map try_emplace boolean boolean", {
  v <- cpp_unordered_map(TRUE, FALSE)
  testthat::expect_equal(type(v), c(key = "boolean", value = "boolean"))
})

# multimap
testthat::test_that("multimap try_emplace integer integer", {
  v <- cpp_multimap(4:9, 12:17)
  testthat::expect_equal(type(v), c(key = "integer", value = "integer"))
})
testthat::test_that("multimap try_emplace integer double", {
  v <- cpp_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "integer", value = "double"))
})
testthat::test_that("multimap try_emplace integer string", {
  v <- cpp_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "integer", value = "string"))
})
testthat::test_that("multimap try_emplace integer boolean", {
  v <- cpp_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "integer", value = "boolean"))
})
testthat::test_that("multimap try_emplace double integer", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(type(v), c(key = "double", value = "integer"))
})
testthat::test_that("multimap try_emplace double double", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "double", value = "double"))
})
testthat::test_that("multimap try_emplace double string", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "double", value = "string"))
})
testthat::test_that("multimap try_emplace double boolean", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "double", value = "boolean"))
})
testthat::test_that("multimap try_emplace string integer", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(type(v), c(key = "string", value = "integer"))
})
testthat::test_that("multimap try_emplace string double", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "string", value = "double"))
})
testthat::test_that("multimap try_emplace string string", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "string", value = "string"))
})
testthat::test_that("multimap try_emplace string boolean", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "string", value = "boolean"))
})
testthat::test_that("multimap try_emplace boolean integer", {
  v <- cpp_multimap(TRUE, 12L)
  testthat::expect_equal(type(v), c(key = "boolean", value = "integer"))
})
testthat::test_that("multimap try_emplace boolean double", {
  v <- cpp_multimap(TRUE, 1.5)
  testthat::expect_equal(type(v), c(key = "boolean", value = "double"))
})
testthat::test_that("multimap try_emplace boolean string", {
  v <- cpp_multimap(TRUE, "upon")
  testthat::expect_equal(type(v), c(key = "boolean", value = "string"))
})
testthat::test_that("multimap try_emplace boolean boolean", {
  v <- cpp_multimap(TRUE, FALSE)
  testthat::expect_equal(type(v), c(key = "boolean", value = "boolean"))
})

# unordered_multimap
testthat::test_that("unordered_multimap try_emplace integer integer", {
  v <- cpp_unordered_multimap(4:9, 12:17)
  testthat::expect_equal(type(v), c(key = "integer", value = "integer"))
})
testthat::test_that("unordered_multimap try_emplace integer double", {
  v <- cpp_unordered_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "integer", value = "double"))
})
testthat::test_that("unordered_multimap try_emplace integer string", {
  v <- cpp_unordered_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "integer", value = "string"))
})
testthat::test_that("unordered_multimap try_emplace integer boolean", {
  v <- cpp_unordered_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "integer", value = "boolean"))
})
testthat::test_that("unordered_multimap try_emplace double integer", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(type(v), c(key = "double", value = "integer"))
})
testthat::test_that("unordered_multimap try_emplace double double", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "double", value = "double"))
})
testthat::test_that("unordered_multimap try_emplace double string", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "double", value = "string"))
})
testthat::test_that("unordered_multimap try_emplace double boolean", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "double", value = "boolean"))
})
testthat::test_that("unordered_multimap try_emplace string integer", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(type(v), c(key = "string", value = "integer"))
})
testthat::test_that("unordered_multimap try_emplace string double", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(type(v), c(key = "string", value = "double"))
})
testthat::test_that("unordered_multimap try_emplace string string", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(type(v), c(key = "string", value = "string"))
})
testthat::test_that("unordered_multimap try_emplace string boolean", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(type(v), c(key = "string", value = "boolean"))
})
testthat::test_that("unordered_multimap try_emplace boolean integer", {
  v <- cpp_unordered_multimap(TRUE, 12L)
  testthat::expect_equal(type(v), c(key = "boolean", value = "integer"))
})
testthat::test_that("unordered_multimap try_emplace boolean double", {
  v <- cpp_unordered_multimap(TRUE, 1.5)
  testthat::expect_equal(type(v), c(key = "boolean", value = "double"))
})
testthat::test_that("unordered_multimap try_emplace boolean string", {
  v <- cpp_unordered_multimap(TRUE, "upon")
  testthat::expect_equal(type(v), c(key = "boolean", value = "string"))
})
testthat::test_that("unordered_multimap try_emplace boolean boolean", {
  v <- cpp_unordered_multimap(TRUE, FALSE)
  testthat::expect_equal(type(v), c(key = "boolean", value = "boolean"))
})

# stack
testthat::test_that("stack type integer", {
  v <- cpp_stack(4:6)
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("stack type double", {
  v <- cpp_stack(seq.int(1, 2, 0.5))
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("stack type string", {
  v <- cpp_stack(c("hello", "there", "world"))
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("stack type boolean", {
  v <- cpp_stack(c(TRUE, FALSE))
  testthat::expect_equal(type(v), "boolean")
})

# queue
testthat::test_that("queue type integer", {
  v <- cpp_queue(4:6)
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("queue type double", {
  v <- cpp_queue(seq.int(1, 2, 0.5))
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("queue type string", {
  v <- cpp_queue(c("hello", "there", "world"))
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("queue type boolean", {
  v <- cpp_queue(c(TRUE, FALSE))
  testthat::expect_equal(type(v), "boolean")
})

# priority_queue
testthat::test_that("priority_queue type integer descending", {
  v <- cpp_priority_queue(4:6)
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("priority_queue type double descending", {
  v <- cpp_priority_queue(seq.int(1, 2, 0.5))
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("priority_queue type string descending", {
  v <- cpp_priority_queue(c("hello", "there", "world"))
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("priority_queue type boolean descending", {
  v <- cpp_priority_queue(c(TRUE, FALSE))
  testthat::expect_equal(type(v), "boolean")
})
testthat::test_that("priority_queue type integer ascending", {
  v <- cpp_priority_queue(4:6, "ascending")
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("priority_queue type double ascending", {
  v <- cpp_priority_queue(seq.int(1, 2, 0.5), "ascending")
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("priority_queue type string ascending", {
  v <- cpp_priority_queue(c("hello", "there", "world"), "ascending")
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("priority_queue type boolean ascending", {
  v <- cpp_priority_queue(c(TRUE, FALSE), "ascending")
  testthat::expect_equal(type(v), "boolean")
})

# vector
testthat::test_that("vector type integer", {
  v <- cpp_vector(4:6)
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("vector type double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("vector type string", {
  v <- cpp_vector(c("hello", "there", "world"))
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("vector type boolean", {
  v <- cpp_vector(c(TRUE, FALSE))
  testthat::expect_equal(type(v), "boolean")
})

# deque
testthat::test_that("deque type integer", {
  v <- cpp_deque(4:6)
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("deque type double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("deque type string", {
  v <- cpp_deque(c("hello", "there", "world"))
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("deque type boolean", {
  v <- cpp_deque(c(TRUE, FALSE))
  testthat::expect_equal(type(v), "boolean")
})

# forward_list
testthat::test_that("forward_list type integer", {
  v <- cpp_forward_list(4:6)
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("forward_list type double", {
  v <- cpp_forward_list(seq.int(1, 2, 0.5))
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("forward_list type string", {
  v <- cpp_forward_list(c("hello", "there", "world"))
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("forward_list type boolean", {
  v <- cpp_forward_list(c(TRUE, FALSE))
  testthat::expect_equal(type(v), "boolean")
})

# list
testthat::test_that("list type integer", {
  v <- cpp_list(4:6)
  testthat::expect_equal(type(v), "integer")
})
testthat::test_that("list type double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_equal(type(v), "double")
})
testthat::test_that("list type string", {
  v <- cpp_list(c("hello", "there", "world"))
  testthat::expect_equal(type(v), "string")
})
testthat::test_that("list type boolean", {
  v <- cpp_list(c(TRUE, FALSE))
  testthat::expect_equal(type(v), "boolean")
})
