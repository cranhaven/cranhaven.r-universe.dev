skip_on_os("mac")
# set
testthat::test_that("set emplace integer", {
  v <- cpp_set(4:9)
  testthat::expect_invisible(emplace(v, 12L))
  testthat::expect_equal(to_r(v), c(4:9, 12L))
  testthat::expect_error(emplace(v, 13:14))
})
testthat::test_that("set emplace double", {
  v <- cpp_set(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace(v, 12))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 12))
  testthat::expect_error(emplace(v, c(13, 14)))
})
testthat::test_that("set emplace string", {
  v <- cpp_set(c("hello", "there"))
  testthat::expect_invisible(emplace(v, "world"))
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
  testthat::expect_error(emplace(v, c("test", "vector")))
})
testthat::test_that("set emplace boolean", {
  v <- cpp_set(TRUE)
  testthat::expect_invisible(emplace(v, FALSE))
  testthat::expect_equal(to_r(v), c(FALSE, TRUE))
  testthat::expect_error(emplace(v, c(TRUE, FALSE)))
})

# unordered_set
testthat::test_that("unordered_set emplace integer", {
  v <- cpp_unordered_set(4:9)
  testthat::expect_invisible(emplace(v, 12L))
  testthat::expect_setequal(to_r(v), c(4:9, 12L))
  testthat::expect_error(emplace(v, 13:14))
})
testthat::test_that("unordered_set emplace double", {
  v <- cpp_unordered_set(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace(v, 12))
  testthat::expect_setequal(to_r(v), c(seq.int(1, 2, 0.5), 12))
  testthat::expect_error(emplace(v, c(13, 14)))
})
testthat::test_that("unordered_set emplace string", {
  v <- cpp_unordered_set(c("hello", "there"))
  testthat::expect_invisible(emplace(v, "world"))
  testthat::expect_setequal(to_r(v), c("hello", "there", "world"))
  testthat::expect_error(emplace(v, c("test", "vector")))
})
testthat::test_that("unordered_set emplace boolean", {
  v <- cpp_unordered_set(TRUE)
  testthat::expect_invisible(emplace(v, FALSE))
  testthat::expect_setequal(to_r(v), c(FALSE, TRUE))
  testthat::expect_error(emplace(v, c(TRUE, FALSE)))
})

# multiset
testthat::test_that("multiset emplace integer", {
  v <- cpp_multiset(4:9)
  testthat::expect_invisible(emplace(v, 12L))
  testthat::expect_equal(to_r(v), c(4:9, 12L))
  testthat::expect_error(emplace(v, 13:14))
})
testthat::test_that("multiset emplace double", {
  v <- cpp_multiset(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace(v, 12))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 12))
  testthat::expect_error(emplace(v, c(13, 14)))
})
testthat::test_that("multiset emplace string", {
  v <- cpp_multiset(c("hello", "there"))
  testthat::expect_invisible(emplace(v, "world"))
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
  testthat::expect_error(emplace(v, c("test", "vector")))
})
testthat::test_that("multiset emplace boolean", {
  v <- cpp_multiset(TRUE)
  testthat::expect_invisible(emplace(v, FALSE))
  testthat::expect_equal(to_r(v), c(FALSE, TRUE))
  testthat::expect_error(emplace(v, c(TRUE, FALSE)))
})

# unordered_multiset
testthat::test_that("unordered_multiset emplace integer", {
  v <- cpp_unordered_multiset(4:9)
  testthat::expect_invisible(emplace(v, 12L))
  testthat::expect_setequal(to_r(v), c(4:9, 12L))
  testthat::expect_error(emplace(v, 13:14))
})
testthat::test_that("unordered_multiset emplace double", {
  v <- cpp_unordered_multiset(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace(v, 12))
  testthat::expect_setequal(to_r(v), c(seq.int(1, 2, 0.5), 12))
  testthat::expect_error(emplace(v, c(13, 14)))
})
testthat::test_that("unordered_multiset emplace string", {
  v <- cpp_unordered_multiset(c("hello", "there"))
  testthat::expect_invisible(emplace(v, "world"))
  testthat::expect_setequal(to_r(v), c("hello", "there", "world"))
  testthat::expect_error(emplace(v, c("test", "vector")))
})
testthat::test_that("unordered_multiset emplace boolean", {
  v <- cpp_unordered_multiset(TRUE)
  testthat::expect_invisible(emplace(v, FALSE))
  testthat::expect_setequal(to_r(v), c(FALSE, TRUE))
  testthat::expect_error(emplace(v, c(TRUE, FALSE)))
})

# map
testthat::test_that("map emplace integer integer", {
  v <- cpp_map(4:9, 12:17)
  testthat::expect_invisible(emplace(v, 12L, 14L))
  testthat::expect_equal(to_r(v), data.frame(key = c(4:9, 14L), value = c(12:17, 12L)))
  testthat::expect_error(emplace(v, 13:14L, 10:11))
})
testthat::test_that("map emplace integer double", {
  v <- cpp_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, 14L))
  testthat::expect_equal(to_r(v), data.frame(key = c(4:9, 14L), value = c(seq.int(1, 3.5, 0.5), 12)))
  testthat::expect_error(emplace(v, c(13, 14), 10:11))
})
testthat::test_that("map emplace integer string", {
  v <- cpp_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "world", 14L))
  testthat::expect_equal(to_r(v), data.frame(key = c(4:9, 14L), value = c("Once", "upon", "a", "time", "in", "R", "world")))
  testthat::expect_error(emplace(v, c("hello", "world"), 10:11))
})
testthat::test_that("map emplace integer boolean", {
  v <- cpp_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, TRUE, 14L))
  testthat::expect_equal(to_r(v), data.frame(key = c(4:9, 14L), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), 10:11))
})
testthat::test_that("map emplace double integer", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(emplace(v, 12L, 5))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), 5), value = c(12:17, 12L)))
  testthat::expect_error(emplace(v, 13:14L, c(10, 11)))
})
testthat::test_that("map emplace double double", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, 5))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), 5), value = c(seq.int(1, 3.5, 0.5), 12)))
  testthat::expect_error(emplace(v, c(13, 14), c(10, 11)))
})
testthat::test_that("map emplace double string", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "world", 14))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), 14), value = c("Once", "upon", "a", "time", "in", "R", "world")))
  testthat::expect_error(emplace(v, c("hello", "world"), c(10, 11)))
})
testthat::test_that("map emplace double boolean", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, TRUE, 14))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), 14), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c(10, 11)))
})
testthat::test_that("map emplace string integer", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_invisible(emplace(v, 12L, "world"))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(12L, 15L, 17L, 13L, 14L, 16L, 12L)))
  testthat::expect_error(emplace(v, 13:14L, c("hello", "there")))
})
testthat::test_that("map emplace string double", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, "world"))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(1, 2.5, 3.5, 1.5, 2, 3, 12)))
  testthat::expect_error(emplace(v, c(13, 14), c("hello", "there")))
})
testthat::test_that("map emplace string string", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "there", "world"))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c("Once", "time", "R", "upon", "a",
    "in", "there")))
  testthat::expect_error(emplace(v, c("a", "test"), c("hello", "there")))
})
testthat::test_that("map emplace string boolean", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, FALSE, "world"))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(TRUE, TRUE, FALSE, FALSE, TRUE,
    FALSE, FALSE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c("hello", "there")))
})
testthat::test_that("map emplace boolean integer", {
  v <- cpp_map(TRUE, 12L)
  testthat::expect_invisible(emplace(v, 2L, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(2L, 12L)))
  testthat::expect_error(emplace(v, 3:4, c(TRUE, FALSE)))
})
testthat::test_that("map emplace boolean double", {
  v <- cpp_map(TRUE, 1.5)
  testthat::expect_invisible(emplace(v, 2, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(2, 1.5)))
  testthat::expect_error(emplace(v, c(3, 4), c(TRUE, FALSE)))
})
testthat::test_that("map emplace boolean string", {
  v <- cpp_map(TRUE, "upon")
  testthat::expect_invisible(emplace(v, "world", FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c("world", "upon")))
  testthat::expect_error(emplace(v, c("hello", "there"), c(TRUE, FALSE)))
})
testthat::test_that("map emplace boolean boolean", {
  v <- cpp_map(TRUE, FALSE)
  testthat::expect_invisible(emplace(v, TRUE, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(TRUE, FALSE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c(TRUE, FALSE)))
})

# unordered_map
testthat::test_that("unordered_map emplace integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  testthat::expect_invisible(emplace(v, 12L, 14L))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(4:9, 14L), value = c(12:17, 12L)))
  testthat::expect_error(emplace(v, 13:14L, 10:11))
})
testthat::test_that("unordered_map emplace integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, 14L))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(4:9, 14L), value = c(seq.int(1, 3.5, 0.5), 12)))
  testthat::expect_error(emplace(v, c(13, 14), 10:11))
})
testthat::test_that("unordered_map emplace integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "world", 14L))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(4:9, 14L), value = c("Once", "upon", "a", "time", "in", "R", "world")))
  testthat::expect_error(emplace(v, c("hello", "world"), 10:11))
})
testthat::test_that("unordered_map emplace integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, TRUE, 14L))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(4:9, 14L), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), 10:11))
})
testthat::test_that("unordered_map emplace double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(emplace(v, 12L, 5))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), 5), value = c(12:17, 12L)))
  testthat::expect_error(emplace(v, 13:14L, c(10, 11)))
})
testthat::test_that("unordered_map emplace double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, 5))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), 5), value = c(seq.int(1, 3.5, 0.5), 12)))
  testthat::expect_error(emplace(v, c(13, 14), c(10, 11)))
})
testthat::test_that("unordered_map emplace double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "world", 14))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), 14), value = c("Once", "upon", "a", "time", "in", "R", "world")))
  testthat::expect_error(emplace(v, c("hello", "world"), c(10, 11)))
})
testthat::test_that("unordered_map emplace double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, TRUE, 14))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), 14), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c(10, 11)))
})
testthat::test_that("unordered_map emplace string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_invisible(emplace(v, 12L, "world"))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(12L, 15L, 17L, 13L,
    14L, 16L, 12L)))
  testthat::expect_error(emplace(v, 13:14L, c("hello", "there")))
})
testthat::test_that("unordered_map emplace string double", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, "world"))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(1, 2.5, 3.5, 1.5, 2,
    3, 12)))
  testthat::expect_error(emplace(v, c(13, 14), c("hello", "there")))
})
testthat::test_that("unordered_map emplace string string", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "there", "world"))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c("Once", "time", "R",
    "upon", "a", "in", "there")))
  testthat::expect_error(emplace(v, c("a", "test"), c("hello", "there")))
})
testthat::test_that("unordered_map emplace string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, FALSE, "world"))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(TRUE, TRUE, FALSE,
    FALSE, TRUE, FALSE, FALSE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c("hello", "there")))
})
testthat::test_that("unordered_map emplace boolean integer", {
  v <- cpp_unordered_map(TRUE, 12L)
  testthat::expect_invisible(emplace(v, 2L, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(2L, 12L)))
  testthat::expect_error(emplace(v, 3:4, c(TRUE, FALSE)))
})
testthat::test_that("unordered_map emplace boolean double", {
  v <- cpp_unordered_map(TRUE, 1.5)
  testthat::expect_invisible(emplace(v, 2, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(2, 1.5)))
  testthat::expect_error(emplace(v, c(3, 4), c(TRUE, FALSE)))
})
testthat::test_that("unordered_map emplace boolean string", {
  v <- cpp_unordered_map(TRUE, "upon")
  testthat::expect_invisible(emplace(v, "world", FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c("world", "upon")))
  testthat::expect_error(emplace(v, c("hello", "there"), c(TRUE, FALSE)))
})
testthat::test_that("unordered_map emplace boolean boolean", {
  v <- cpp_unordered_map(TRUE, FALSE)
  testthat::expect_invisible(emplace(v, TRUE, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(TRUE, FALSE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c(TRUE, FALSE)))
})

# multimap
testthat::test_that("multimap emplace integer integer", {
  v <- cpp_multimap(4:9, 12:17)
  testthat::expect_invisible(emplace(v, 12L, 14L))
  testthat::expect_equal(to_r(v), data.frame(key = c(4:9, 14L), value = c(12:17, 12L)))
  testthat::expect_error(emplace(v, 13:14L, 10:11))
})
testthat::test_that("multimap emplace integer double", {
  v <- cpp_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, 14L))
  testthat::expect_equal(to_r(v), data.frame(key = c(4:9, 14L), value = c(seq.int(1, 3.5, 0.5), 12)))
  testthat::expect_error(emplace(v, c(13, 14), 10:11))
})
testthat::test_that("multimap emplace integer string", {
  v <- cpp_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "world", 14L))
  testthat::expect_equal(to_r(v), data.frame(key = c(4:9, 14L), value = c("Once", "upon", "a", "time", "in", "R", "world")))
  testthat::expect_error(emplace(v, c("hello", "world"), 10:11))
})
testthat::test_that("multimap emplace integer boolean", {
  v <- cpp_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, TRUE, 14L))
  testthat::expect_equal(to_r(v), data.frame(key = c(4:9, 14L), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), 10:11))
})
testthat::test_that("multimap emplace double integer", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(emplace(v, 12L, 5))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), 5), value = c(12:17, 12L)))
  testthat::expect_error(emplace(v, 13:14L, c(10, 11)))
})
testthat::test_that("multimap emplace double double", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, 5))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), 5), value = c(seq.int(1, 3.5, 0.5), 12)))
  testthat::expect_error(emplace(v, c(13, 14), c(10, 11)))
})
testthat::test_that("multimap emplace double string", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "world", 14))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), 14), value = c("Once", "upon", "a", "time", "in", "R", "world")))
  testthat::expect_error(emplace(v, c("hello", "world"), c(10, 11)))
})
testthat::test_that("multimap emplace double boolean", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, TRUE, 14))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), 14), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c(10, 11)))
})
testthat::test_that("multimap emplace string integer", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_invisible(emplace(v, 12L, "world"))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(12L, 15L, 17L, 13L, 14L, 16L, 12L)))
  testthat::expect_error(emplace(v, 13:14L, c("hello", "there")))
})
testthat::test_that("multimap emplace string double", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, "world"))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(1, 2.5, 3.5, 1.5, 2, 3, 12)))
  testthat::expect_error(emplace(v, c(13, 14), c("hello", "there")))
})
testthat::test_that("multimap emplace string string", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "there", "world"))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c("Once", "time", "R", "upon", "a",
    "in", "there")))
  testthat::expect_error(emplace(v, c("a", "test"), c("hello", "there")))
})
testthat::test_that("multimap emplace string boolean", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, FALSE, "world"))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(TRUE, TRUE, FALSE, FALSE, TRUE,
    FALSE, FALSE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c("hello", "there")))
})
testthat::test_that("multimap emplace boolean integer", {
  v <- cpp_multimap(TRUE, 12L)
  testthat::expect_invisible(emplace(v, 2L, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(2L, 12L)))
  testthat::expect_error(emplace(v, 3:4, c(TRUE, FALSE)))
})
testthat::test_that("multimap emplace boolean double", {
  v <- cpp_multimap(TRUE, 1.5)
  testthat::expect_invisible(emplace(v, 2, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(2, 1.5)))
  testthat::expect_error(emplace(v, c(3, 4), c(TRUE, FALSE)))
})
testthat::test_that("multimap emplace boolean string", {
  v <- cpp_multimap(TRUE, "upon")
  testthat::expect_invisible(emplace(v, "world", FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c("world", "upon")))
  testthat::expect_error(emplace(v, c("hello", "there"), c(TRUE, FALSE)))
})
testthat::test_that("multimap emplace boolean boolean", {
  v <- cpp_multimap(TRUE, FALSE)
  testthat::expect_invisible(emplace(v, TRUE, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(TRUE, FALSE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c(TRUE, FALSE)))
})

# unordered_multimap
testthat::test_that("unordered_multimap emplace integer integer", {
  v <- cpp_unordered_multimap(4:9, 12:17)
  testthat::expect_invisible(emplace(v, 12L, 14L))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(4:9, 14L), value = c(12:17, 12L)))
  testthat::expect_error(emplace(v, 13:14L, 10:11))
})
testthat::test_that("unordered_multimap emplace integer double", {
  v <- cpp_unordered_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, 14L))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(4:9, 14L), value = c(seq.int(1, 3.5, 0.5), 12)))
  testthat::expect_error(emplace(v, c(13, 14), 10:11))
})
testthat::test_that("unordered_multimap emplace integer string", {
  v <- cpp_unordered_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "world", 14L))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(4:9, 14L), value = c("Once", "upon", "a", "time", "in", "R", "world")))
  testthat::expect_error(emplace(v, c("hello", "world"), 10:11))
})
testthat::test_that("unordered_multimap emplace integer boolean", {
  v <- cpp_unordered_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, TRUE, 14L))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(4:9, 14L), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), 10:11))
})
testthat::test_that("unordered_multimap emplace double integer", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(emplace(v, 12L, 5))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), 5), value = c(12:17, 12L)))
  testthat::expect_error(emplace(v, 13:14L, c(10, 11)))
})
testthat::test_that("unordered_multimap emplace double double", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, 5))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), 5), value = c(seq.int(1, 3.5, 0.5), 12)))
  testthat::expect_error(emplace(v, c(13, 14), c(10, 11)))
})
testthat::test_that("unordered_multimap emplace double string", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "world", 14))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), 14), value = c("Once", "upon", "a", "time", "in", "R", "world")))
  testthat::expect_error(emplace(v, c("hello", "world"), c(10, 11)))
})
testthat::test_that("unordered_multimap emplace double boolean", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, TRUE, 14))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), 14), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c(10, 11)))
})
testthat::test_that("unordered_multimap emplace string integer", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_invisible(emplace(v, 12L, "world"))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(12L, 15L, 17L, 13L,
    14L, 16L, 12L)))
  testthat::expect_error(emplace(v, 13:14L, c("hello", "there")))
})
testthat::test_that("unordered_multimap emplace string double", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(emplace(v, 12, "world"))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(1, 2.5, 3.5, 1.5, 2,
    3, 12)))
  testthat::expect_error(emplace(v, c(13, 14), c("hello", "there")))
})
testthat::test_that("unordered_multimap emplace string string", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(emplace(v, "there", "world"))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c("Once", "time", "R",
    "upon", "a", "in", "there")))
  testthat::expect_error(emplace(v, c("a", "test"), c("hello", "there")))
})
testthat::test_that("unordered_multimap emplace string boolean", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(emplace(v, FALSE, "world"))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "of", "package", "quick", "test", "the", "world"), value = c(TRUE, TRUE, FALSE,
    FALSE, TRUE, FALSE, FALSE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c("hello", "there")))
})
testthat::test_that("unordered_multimap emplace boolean integer", {
  v <- cpp_unordered_multimap(TRUE, 12L)
  testthat::expect_invisible(emplace(v, 2L, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(2L, 12L)))
  testthat::expect_error(emplace(v, 3:4, c(TRUE, FALSE)))
})
testthat::test_that("unordered_multimap emplace boolean double", {
  v <- cpp_unordered_multimap(TRUE, 1.5)
  testthat::expect_invisible(emplace(v, 2, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(2, 1.5)))
  testthat::expect_error(emplace(v, c(3, 4), c(TRUE, FALSE)))
})
testthat::test_that("unordered_multimap emplace boolean string", {
  v <- cpp_unordered_multimap(TRUE, "upon")
  testthat::expect_invisible(emplace(v, "world", FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c("world", "upon")))
  testthat::expect_error(emplace(v, c("hello", "there"), c(TRUE, FALSE)))
})
testthat::test_that("unordered_multimap emplace boolean boolean", {
  v <- cpp_unordered_multimap(TRUE, FALSE)
  testthat::expect_invisible(emplace(v, TRUE, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(TRUE, FALSE)))
  testthat::expect_error(emplace(v, c(TRUE, FALSE), c(TRUE, FALSE)))
})

# stack
testthat::test_that("stack emplace integer", {
  v <- cpp_stack(4:9)
  testthat::expect_invisible(emplace(v, 12L))
  testthat::expect_equal(to_r(v), c(12L, 9:4))
  testthat::expect_error(emplace(v, 13:14))
})
testthat::test_that("stack emplace double", {
  v <- cpp_stack(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace(v, 12))
  testthat::expect_equal(to_r(v), c(12, seq.int(2, 1, -0.5)))
  testthat::expect_error(emplace(v, c(13, 14)))
})
testthat::test_that("stack emplace string", {
  v <- cpp_stack(c("hello", "there"))
  testthat::expect_invisible(emplace(v, "world"))
  testthat::expect_equal(to_r(v), c("world", "there", "hello"))
  testthat::expect_error(emplace(v, c("R", "package")))
})
testthat::test_that("stack emplace boolean", {
  v <- cpp_stack(c(TRUE, FALSE))
  testthat::expect_invisible(emplace(v, FALSE))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, TRUE))
  testthat::expect_error(emplace(v, c(TRUE, FALSE)))
})

# queue
testthat::test_that("queue emplace integer", {
  v <- cpp_queue(4:9)
  testthat::expect_invisible(emplace(v, 12L))
  testthat::expect_equal(to_r(v), c(4:9, 12L))
  testthat::expect_error(emplace(v, 13:14))
})
testthat::test_that("queue emplace double", {
  v <- cpp_queue(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace(v, 12))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 12))
  testthat::expect_error(emplace(v, c(13, 14)))
})
testthat::test_that("queue emplace string", {
  v <- cpp_queue(c("hello", "there"))
  testthat::expect_invisible(emplace(v, "world"))
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
  testthat::expect_error(emplace(v, c("R", "package")))
})
testthat::test_that("queue emplace boolean", {
  v <- cpp_queue(c(TRUE, FALSE))
  testthat::expect_invisible(emplace(v, FALSE))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, FALSE))
  testthat::expect_error(emplace(v, c(TRUE, FALSE)))
})

# priority_queue
testthat::test_that("priority_queue emplace integer", {
  v <- cpp_priority_queue(4:9)
  testthat::expect_invisible(emplace(v, 12L))
  testthat::expect_equal(to_r(v), c(12L, 9:4))
  testthat::expect_error(emplace(v, 13:14))
})
testthat::test_that("priority_queue emplace double", {
  v <- cpp_priority_queue(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace(v, 12))
  testthat::expect_equal(to_r(v), c(12, seq.int(2, 1, -0.5)))
  testthat::expect_error(emplace(v, c(13, 14)))
})
testthat::test_that("priority_queue emplace string", {
  v <- cpp_priority_queue(c("hello", "there"))
  testthat::expect_invisible(emplace(v, "world"))
  testthat::expect_equal(to_r(v), c("world", "there", "hello"))
  testthat::expect_error(emplace(v, c("R", "package")))
})
testthat::test_that("priority_queue emplace boolean", {
  v <- cpp_priority_queue(c(TRUE, FALSE))
  testthat::expect_invisible(emplace(v, FALSE))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, FALSE))
  testthat::expect_error(emplace(v, c(TRUE, FALSE)))
})
testthat::test_that("priority_queue emplace integer", {
  v <- cpp_priority_queue(4:9, "ascending")
  testthat::expect_invisible(emplace(v, 12L))
  testthat::expect_equal(to_r(v), c(4:9, 12L))
  testthat::expect_error(emplace(v, 13:14))
})
testthat::test_that("priority_queue emplace double", {
  v <- cpp_priority_queue(seq.int(1, 2, 0.5), "ascending")
  testthat::expect_invisible(emplace(v, 12))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), 12))
  testthat::expect_error(emplace(v, c(13, 14)))
})
testthat::test_that("priority_queue emplace string", {
  v <- cpp_priority_queue(c("hello", "there"), "ascending")
  testthat::expect_invisible(emplace(v, "world"))
  testthat::expect_equal(to_r(v), c("hello", "there", "world"))
  testthat::expect_error(emplace(v, c("R", "package")))
})
testthat::test_that("priority_queue emplace boolean", {
  v <- cpp_priority_queue(c(TRUE, FALSE), "ascending")
  testthat::expect_invisible(emplace(v, FALSE))
  testthat::expect_equal(to_r(v), c(FALSE, FALSE, TRUE))
  testthat::expect_error(emplace(v, c(TRUE, FALSE)))
})

# vector
testthat::test_that("vector emplace integer", {
  v <- cpp_vector(4:9)
  testthat::expect_invisible(emplace(v, 12L, position = 2L))
  testthat::expect_equal(to_r(v), c(4L, 12L, 5:9))
  testthat::expect_error(emplace(v, 13:14))
})
testthat::test_that("vector emplace double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace(v, 12, position = 2L))
  testthat::expect_equal(to_r(v), c(1, 12, 1.5, 2))
  testthat::expect_error(emplace(v, c(13, 14)))
})
testthat::test_that("vector emplace string", {
  v <- cpp_vector(c("hello", "there"))
  testthat::expect_invisible(emplace(v, "world", position = 2L))
  testthat::expect_equal(to_r(v), c("hello", "world", "there"))
  testthat::expect_error(emplace(v, c("test", "vector")))
})
testthat::test_that("vector emplace boolean", {
  v <- cpp_vector(c(TRUE, FALSE))
  testthat::expect_invisible(emplace(v, TRUE, position = 2L))
  testthat::expect_equal(to_r(v), c(TRUE, TRUE, FALSE))
  testthat::expect_error(emplace(v, c(TRUE, FALSE)))
})

# deque
testthat::test_that("deque emplace integer", {
  v <- cpp_deque(4:9)
  testthat::expect_invisible(emplace(v, 12L, position = 2L))
  testthat::expect_equal(to_r(v), c(4L, 12L, 5:9))
  testthat::expect_error(emplace(v, 13:14))
})
testthat::test_that("deque emplace double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace(v, 12, position = 2L))
  testthat::expect_equal(to_r(v), c(1, 12, 1.5, 2))
  testthat::expect_error(emplace(v, c(13, 14)))
})
testthat::test_that("deque emplace string", {
  v <- cpp_deque(c("hello", "there"))
  testthat::expect_invisible(emplace(v, "world", position = 2L))
  testthat::expect_equal(to_r(v), c("hello", "world", "there"))
  testthat::expect_error(emplace(v, c("test", "vector")))
})
testthat::test_that("deque emplace boolean", {
  v <- cpp_deque(c(TRUE, FALSE))
  testthat::expect_invisible(emplace(v, TRUE, position = 2L))
  testthat::expect_equal(to_r(v), c(TRUE, TRUE, FALSE))
  testthat::expect_error(emplace(v, c(TRUE, FALSE)))
})

# list
testthat::test_that("list emplace integer", {
  v <- cpp_list(4:9)
  testthat::expect_invisible(emplace(v, 12L, position = 2L))
  testthat::expect_equal(to_r(v), c(4L, 12L, 5:9))
  testthat::expect_error(emplace(v, 13:14))
})
testthat::test_that("list emplace double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(emplace(v, 12, position = 2L))
  testthat::expect_equal(to_r(v), c(1, 12, 1.5, 2))
  testthat::expect_error(emplace(v, c(13, 14)))
})
testthat::test_that("list emplace string", {
  v <- cpp_list(c("hello", "there"))
  testthat::expect_invisible(emplace(v, "world", position = 2L))
  testthat::expect_equal(to_r(v), c("hello", "world", "there"))
  testthat::expect_error(emplace(v, c("test", "vector")))
})
testthat::test_that("list emplace boolean", {
  v <- cpp_list(c(TRUE, FALSE))
  testthat::expect_invisible(emplace(v, TRUE, position = 2L))
  testthat::expect_equal(to_r(v), c(TRUE, TRUE, FALSE))
  testthat::expect_error(emplace(v, c(TRUE, FALSE)))
})
