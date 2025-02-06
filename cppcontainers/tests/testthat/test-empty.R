skip_on_os("mac")
# set
testthat::test_that("set empty integer", {
  v <- cpp_set(4L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("set empty double", {
  v <- cpp_set(1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("set empty string", {
  v <- cpp_set("hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("set empty boolean", {
  v <- cpp_set(TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})

# unordered_set
testthat::test_that("unordered_set empty integer", {
  v <- cpp_unordered_set(4L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_set empty double", {
  v <- cpp_unordered_set(1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_set empty string", {
  v <- cpp_unordered_set("hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_set empty boolean", {
  v <- cpp_unordered_set(TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})

# multiset
testthat::test_that("multiset empty integer", {
  v <- cpp_multiset(4L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multiset empty double", {
  v <- cpp_multiset(1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multiset empty string", {
  v <- cpp_multiset("hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multiset empty boolean", {
  v <- cpp_multiset(TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})

# unordered_multiset
testthat::test_that("unordered_multiset empty integer", {
  v <- cpp_unordered_multiset(4L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multiset empty double", {
  v <- cpp_unordered_multiset(1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multiset empty string", {
  v <- cpp_unordered_multiset("hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multiset empty boolean", {
  v <- cpp_unordered_multiset(TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})

# map
testthat::test_that("map empty integer integer", {
  v <- cpp_map(4L, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty integer double", {
  v <- cpp_map(4L, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty integer string", {
  v <- cpp_map(4L, "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty integer boolean", {
  v <- cpp_map(4L, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty double integer", {
  v <- cpp_map(1.2, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty double double", {
  v <- cpp_map(1.2, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty double string", {
  v <- cpp_map(1.2, "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty double boolean", {
  v <- cpp_map(1.2, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty string integer", {
  v <- cpp_map("there", 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty string double", {
  v <- cpp_map("there", 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty string string", {
  v <- cpp_map("there", "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty string boolean", {
  v <- cpp_map("there", TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty boolean integer", {
  v <- cpp_map(FALSE, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty boolean double", {
  v <- cpp_map(FALSE, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty boolean string", {
  v <- cpp_map(FALSE, "hello")
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("map empty boolean boolean", {
  v <- cpp_map(FALSE, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})

# unordered_map
testthat::test_that("unordered_map empty integer integer", {
  v <- cpp_unordered_map(4L, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty integer double", {
  v <- cpp_unordered_map(4L, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty integer string", {
  v <- cpp_unordered_map(4L, "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty integer boolean", {
  v <- cpp_unordered_map(4L, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty double integer", {
  v <- cpp_unordered_map(1.2, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty double double", {
  v <- cpp_unordered_map(1.2, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty double string", {
  v <- cpp_unordered_map(1.2, "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty double boolean", {
  v <- cpp_unordered_map(1.2, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty string integer", {
  v <- cpp_unordered_map("there", 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty string double", {
  v <- cpp_unordered_map("there", 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty string string", {
  v <- cpp_unordered_map("there", "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty string boolean", {
  v <- cpp_unordered_map("there", TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty boolean integer", {
  v <- cpp_unordered_map(FALSE, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty boolean double", {
  v <- cpp_unordered_map(FALSE, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty boolean string", {
  v <- cpp_unordered_map(FALSE, "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_map empty boolean boolean", {
  v <- cpp_unordered_map(FALSE, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})

# multimap
testthat::test_that("multimap empty integer integer", {
  v <- cpp_multimap(4L, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty integer double", {
  v <- cpp_multimap(4L, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty integer string", {
  v <- cpp_multimap(4L, "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty integer boolean", {
  v <- cpp_multimap(4L, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty double integer", {
  v <- cpp_multimap(1.2, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty double double", {
  v <- cpp_multimap(1.2, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty double string", {
  v <- cpp_multimap(1.2, "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty double boolean", {
  v <- cpp_multimap(1.2, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty string integer", {
  v <- cpp_multimap("there", 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty string double", {
  v <- cpp_multimap("there", 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty string string", {
  v <- cpp_multimap("there", "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty string boolean", {
  v <- cpp_multimap("there", TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty boolean integer", {
  v <- cpp_multimap(FALSE, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty boolean double", {
  v <- cpp_multimap(FALSE, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty boolean string", {
  v <- cpp_multimap(FALSE, "hello")
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("multimap empty boolean boolean", {
  v <- cpp_multimap(FALSE, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})

# unordered_multimap
testthat::test_that("unordered_multimap empty integer integer", {
  v <- cpp_unordered_multimap(4L, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty integer double", {
  v <- cpp_unordered_multimap(4L, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty integer string", {
  v <- cpp_unordered_multimap(4L, "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty integer boolean", {
  v <- cpp_unordered_multimap(4L, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty double integer", {
  v <- cpp_unordered_multimap(1.2, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty double double", {
  v <- cpp_unordered_multimap(1.2, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty double string", {
  v <- cpp_unordered_multimap(1.2, "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty double boolean", {
  v <- cpp_unordered_multimap(1.2, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty string integer", {
  v <- cpp_unordered_multimap("there", 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty string double", {
  v <- cpp_unordered_multimap("there", 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty string string", {
  v <- cpp_unordered_multimap("there", "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty string boolean", {
  v <- cpp_unordered_multimap("there", TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty boolean integer", {
  v <- cpp_unordered_multimap(FALSE, 6L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty boolean double", {
  v <- cpp_unordered_multimap(FALSE, 1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty boolean string", {
  v <- cpp_unordered_multimap(FALSE, "hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("unordered_multimap empty boolean boolean", {
  v <- cpp_unordered_multimap(FALSE, TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})

# stack
testthat::test_that("stack empty integer", {
  v <- cpp_stack(4L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("stack empty double", {
  v <- cpp_stack(1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("stack empty string", {
  v <- cpp_stack("hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("stack empty boolean", {
  v <- cpp_stack(TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})

# queue
testthat::test_that("queue empty integer", {
  v <- cpp_queue(4L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("queue empty double", {
  v <- cpp_queue(1.2)
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("queue empty string", {
  v <- cpp_queue("hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("queue empty boolean", {
  v <- cpp_queue(TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})

# priority_queue
testthat::test_that("priority_queue empty integer", {
  v <- cpp_priority_queue(4L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("priority_queue empty double", {
  v <- cpp_priority_queue(1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("priority_queue empty string", {
  v <- cpp_priority_queue("hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("priority_queue empty boolean", {
  v <- cpp_priority_queue(TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(pop(v))
  testthat::expect_true(empty(v))
})

# vector
testthat::test_that("vector empty integer", {
  v <- cpp_vector(4L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("vector empty double", {
  v <- cpp_vector(1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("vector empty string", {
  v <- cpp_vector("hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("vector empty boolean", {
  v <- cpp_vector(TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})

# deque
testthat::test_that("deque empty integer", {
  v <- cpp_deque(4L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("deque empty double", {
  v <- cpp_deque(1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("deque empty string", {
  v <- cpp_deque("hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("deque empty boolean", {
  v <- cpp_deque(TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})

# forward_list
testthat::test_that("forward_list empty integer", {
  v <- cpp_forward_list(4L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("forward_list empty double", {
  v <- cpp_forward_list(1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("forward_list empty string", {
  v <- cpp_forward_list("hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("forward_list empty boolean", {
  v <- cpp_forward_list(TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})

# list
testthat::test_that("list empty integer", {
  v <- cpp_list(4L)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("list empty double", {
  v <- cpp_list(1.2)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("list empty string", {
  v <- cpp_list("hello")
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
testthat::test_that("list empty boolean", {
  v <- cpp_list(TRUE)
  testthat::expect_false(empty(v))
  testthat::expect_invisible(clear(v))
  testthat::expect_true(empty(v))
})
