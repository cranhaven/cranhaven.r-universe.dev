skip_on_os("mac")
# set
testthat::test_that("set erase integer", {
  v <- cpp_set(4:9)
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_equal(to_r(v), c(4:5, 8:9))
})
testthat::test_that("set erase double", {
  v <- cpp_set(seq.int(1, 2, 0.5))
  testthat::expect_invisible(erase(v, c(1.5, 2)))
  testthat::expect_equal(to_r(v), 1)
})
testthat::test_that("set erase string", {
  v <- cpp_set(c("hello", "there", "world"))
  testthat::expect_invisible(erase(v, c("there", "world")))
  testthat::expect_equal(to_r(v), "hello")
})
testthat::test_that("set erase boolean", {
  v <- cpp_set(c(TRUE, FALSE))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v), FALSE)
})

# unordered_set
testthat::test_that("unordered_set erase integer", {
  v <- cpp_unordered_set(4:9)
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_setequal(to_r(v), c(4:5, 8:9))
})
testthat::test_that("unordered_set erase double", {
  v <- cpp_unordered_set(seq.int(1, 2, 0.5))
  testthat::expect_invisible(erase(v, c(1.5, 2)))
  testthat::expect_equal(to_r(v), 1)
})
testthat::test_that("unordered_set erase string", {
  v <- cpp_unordered_set(c("hello", "there", "world"))
  testthat::expect_invisible(erase(v, c("there", "world")))
  testthat::expect_equal(to_r(v), "hello")
})
testthat::test_that("unordered_set erase boolean", {
  v <- cpp_unordered_set(c(TRUE, FALSE))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v), FALSE)
})

# multiset
testthat::test_that("multiset erase integer", {
  v <- cpp_multiset(4:9)
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_equal(to_r(v), c(4:5, 8:9))
})
testthat::test_that("multiset erase double", {
  v <- cpp_multiset(seq.int(1, 2, 0.5))
  testthat::expect_invisible(erase(v, c(1.5, 2)))
  testthat::expect_equal(to_r(v), 1)
})
testthat::test_that("multiset erase string", {
  v <- cpp_multiset(c("hello", "there", "world"))
  testthat::expect_invisible(erase(v, c("there", "world")))
  testthat::expect_equal(to_r(v), "hello")
})
testthat::test_that("multiset erase boolean", {
  v <- cpp_multiset(c(TRUE, FALSE))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v), FALSE)
})

# unordered_multiset
testthat::test_that("unordered_multiset erase integer", {
  v <- cpp_unordered_multiset(4:9)
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_setequal(to_r(v), c(4:5, 8:9))
})
testthat::test_that("unordered_multiset erase double", {
  v <- cpp_unordered_multiset(seq.int(1, 2, 0.5))
  testthat::expect_invisible(erase(v, c(1.5, 2)))
  testthat::expect_equal(to_r(v), 1)
})
testthat::test_that("unordered_multiset erase string", {
  v <- cpp_unordered_multiset(c("hello", "there", "world"))
  testthat::expect_invisible(erase(v, c("there", "world")))
  testthat::expect_equal(to_r(v), "hello")
})
testthat::test_that("unordered_multiset erase boolean", {
  v <- cpp_unordered_multiset(c(TRUE, FALSE))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v), FALSE)
})

# map
testthat::test_that("map erase integer integer", {
  v <- cpp_map(4:9, 12:17)
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_equal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("map erase integer double", {
  v <- cpp_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_equal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("map erase integer string", {
  v <- cpp_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_equal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("map erase integer boolean", {
  v <- cpp_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_equal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("map erase double integer", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_equal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("map erase double double", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_equal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("map erase double string", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_equal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("map erase double boolean", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_equal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("map erase string integer", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_equal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("map erase string double", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_equal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("map erase string string", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_equal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("map erase string boolean", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_equal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("map erase boolean integer", {
  v <- cpp_map(c(TRUE, FALSE), 12:13)
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("map erase boolean double", {
  v <- cpp_map(c(TRUE, FALSE), c(1.5, 2))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("map erase boolean string", {
  v <- cpp_map(c(TRUE, FALSE), c("upon", "package"))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("map erase boolean boolean", {
  v <- cpp_map(c(TRUE, FALSE), c(TRUE, FALSE))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})

# unordered_map
testthat::test_that("unordered_map erase integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_setequal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("unordered_map erase integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_setequal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("unordered_map erase integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_setequal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("unordered_map erase integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_setequal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("unordered_map erase double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_setequal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("unordered_map erase double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_setequal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("unordered_map erase double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_setequal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("unordered_map erase double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_setequal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("unordered_map erase string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_setequal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("unordered_map erase string double", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_setequal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("unordered_map erase string string", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_setequal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("unordered_map erase string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_setequal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("unordered_map erase boolean integer", {
  v <- cpp_unordered_map(c(TRUE, FALSE), 12:13)
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("unordered_map erase boolean double", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c(1.5, 2))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("unordered_map erase boolean string", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c("upon", "package"))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("unordered_map erase boolean boolean", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c(TRUE, FALSE))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})

# multimap
testthat::test_that("multimap erase integer integer", {
  v <- cpp_multimap(4:9, 12:17)
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_equal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("multimap erase integer double", {
  v <- cpp_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_equal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("multimap erase integer string", {
  v <- cpp_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_equal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("multimap erase integer boolean", {
  v <- cpp_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_equal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("multimap erase double integer", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_equal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("multimap erase double double", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_equal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("multimap erase double string", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_equal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("multimap erase double boolean", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_equal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("multimap erase string integer", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_equal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("multimap erase string double", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_equal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("multimap erase string string", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_equal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("multimap erase string boolean", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_equal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("multimap erase boolean integer", {
  v <- cpp_multimap(c(TRUE, FALSE), 12:13)
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("multimap erase boolean double", {
  v <- cpp_multimap(c(TRUE, FALSE), c(1.5, 2))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("multimap erase boolean string", {
  v <- cpp_multimap(c(TRUE, FALSE), c("upon", "package"))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("multimap erase boolean boolean", {
  v <- cpp_multimap(c(TRUE, FALSE), c(TRUE, FALSE))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})

# unordered_multimap
testthat::test_that("unordered_multimap erase integer integer", {
  v <- cpp_unordered_multimap(4:9, 12:17)
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_setequal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("unordered_multimap erase integer double", {
  v <- cpp_unordered_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_setequal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("unordered_multimap erase integer string", {
  v <- cpp_unordered_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_setequal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("unordered_multimap erase integer boolean", {
  v <- cpp_unordered_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, 6:7))
  testthat::expect_setequal(to_r(v)$key, c(4:5, 8:9))
})
testthat::test_that("unordered_multimap erase double integer", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_setequal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("unordered_multimap erase double double", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_setequal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("unordered_multimap erase double string", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_setequal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("unordered_multimap erase double boolean", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, c(2.5, 3)))
  testthat::expect_setequal(to_r(v)$key, c(2, seq.int(3.5, 4.5, 0.5)))
})
testthat::test_that("unordered_multimap erase string integer", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_setequal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("unordered_multimap erase string double", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_setequal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("unordered_multimap erase string string", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_setequal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("unordered_multimap erase string boolean", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(erase(v, c("of", "the")))
  testthat::expect_setequal(to_r(v)$key, c("A", "package", "quick", "test"))
})
testthat::test_that("unordered_multimap erase boolean integer", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), 12:13)
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("unordered_multimap erase boolean double", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c(1.5, 2))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("unordered_multimap erase boolean string", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c("upon", "package"))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})
testthat::test_that("unordered_multimap erase boolean boolean", {
  v <- cpp_unordered_multimap(c(TRUE, FALSE), c(TRUE, FALSE))
  testthat::expect_invisible(erase(v, TRUE))
  testthat::expect_equal(to_r(v)$key, FALSE)
})

# vector
testthat::test_that("vector erase integer", {
  v <- cpp_vector(4:9)
  testthat::expect_invisible(erase(v, from = 3, to = 4))
  testthat::expect_equal(to_r(v), c(4:5, 8:9))
})
testthat::test_that("vector erase double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_invisible(erase(v, from = 2, to = 3))
  testthat::expect_equal(to_r(v), 1)
})
testthat::test_that("vector erase string", {
  v <- cpp_vector(c("hello", "there", "world"))
  testthat::expect_invisible(erase(v, from = 2, to = 3))
  testthat::expect_equal(to_r(v), "hello")
})
testthat::test_that("vector erase boolean", {
  v <- cpp_vector(c(TRUE, FALSE))
  testthat::expect_invisible(erase(v, from = 1, to = 1))
  testthat::expect_equal(to_r(v), FALSE)
})

# deque
testthat::test_that("deque erase integer", {
  v <- cpp_deque(4:9)
  testthat::expect_invisible(erase(v, from = 3, to = 4))
  testthat::expect_equal(to_r(v), c(4:5, 8:9))
})
testthat::test_that("deque erase double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_invisible(erase(v, from = 2, to = 3))
  testthat::expect_equal(to_r(v), 1)
})
testthat::test_that("deque erase string", {
  v <- cpp_deque(c("hello", "there", "world"))
  testthat::expect_invisible(erase(v, from = 2, to = 3))
  testthat::expect_equal(to_r(v), "hello")
})
testthat::test_that("deque erase boolean", {
  v <- cpp_deque(c(TRUE, FALSE))
  testthat::expect_invisible(erase(v, from = 1, to = 1))
  testthat::expect_equal(to_r(v), FALSE)
})

# list
testthat::test_that("list erase integer", {
  v <- cpp_list(4:9)
  testthat::expect_invisible(erase(v, from = 3, to = 4))
  testthat::expect_equal(to_r(v), c(4:5, 8:9))
})
testthat::test_that("list erase double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(erase(v, from = 2, to = 3))
  testthat::expect_equal(to_r(v), 1)
})
testthat::test_that("list erase string", {
  v <- cpp_list(c("hello", "there", "world"))
  testthat::expect_invisible(erase(v, from = 2, to = 3))
  testthat::expect_equal(to_r(v), "hello")
})
testthat::test_that("list erase boolean", {
  v <- cpp_list(c(TRUE, FALSE))
  testthat::expect_invisible(erase(v, from = 1, to = 1))
  testthat::expect_equal(to_r(v), FALSE)
})
