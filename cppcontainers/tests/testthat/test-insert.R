skip_on_os("mac")
# set
testthat::test_that("set insert integer", {
  v <- cpp_set(4:9)
  testthat::expect_invisible(insert(v, 12:13))
  testthat::expect_equal(to_r(v), c(4:9, 12:13))
})
testthat::test_that("set insert double", {
  v <- cpp_set(seq.int(1, 2, 0.5))
  testthat::expect_invisible(insert(v, c(12, 13)))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), c(12, 13)))
})
testthat::test_that("set insert string", {
  v <- cpp_set(c("hello", "there"))
  testthat::expect_invisible(insert(v, c("world", "a")))
  testthat::expect_equal(to_r(v), c("a", "hello", "there", "world"))
})
testthat::test_that("set insert boolean", {
  v <- cpp_set(TRUE)
  testthat::expect_invisible(insert(v, FALSE))
  testthat::expect_equal(to_r(v), c(FALSE, TRUE))
})

# unordered_set
testthat::test_that("unordered_set insert integer", {
  v <- cpp_unordered_set(4:9)
  testthat::expect_invisible(insert(v, 12:13))
  testthat::expect_setequal(to_r(v), c(4:9, 12:13))
})
testthat::test_that("unordered_set insert double", {
  v <- cpp_unordered_set(seq.int(1, 2, 0.5))
  testthat::expect_invisible(insert(v, c(12, 13)))
  testthat::expect_setequal(to_r(v), c(seq.int(1, 2, 0.5), c(12, 13)))
})
testthat::test_that("unordered_set insert string", {
  v <- cpp_unordered_set(c("hello", "there"))
  testthat::expect_invisible(insert(v, c("world", "a")))
  testthat::expect_setequal(to_r(v), c("a", "hello", "there", "world"))
})
testthat::test_that("unordered_set insert boolean", {
  v <- cpp_unordered_set(TRUE)
  testthat::expect_invisible(insert(v, FALSE))
  testthat::expect_setequal(to_r(v), c(FALSE, TRUE))
})

# multiset
testthat::test_that("multiset insert integer", {
  v <- cpp_multiset(4:9)
  testthat::expect_invisible(insert(v, 12:13))
  testthat::expect_equal(to_r(v), c(4:9, 12:13))
})
testthat::test_that("multiset insert double", {
  v <- cpp_multiset(seq.int(1, 2, 0.5))
  testthat::expect_invisible(insert(v, c(12, 13)))
  testthat::expect_equal(to_r(v), c(seq.int(1, 2, 0.5), c(12, 13)))
})
testthat::test_that("multiset insert string", {
  v <- cpp_multiset(c("hello", "there"))
  testthat::expect_invisible(insert(v, c("world", "a")))
  testthat::expect_equal(to_r(v), c("a", "hello", "there", "world"))
})
testthat::test_that("multiset insert boolean", {
  v <- cpp_multiset(TRUE)
  testthat::expect_invisible(insert(v, FALSE))
  testthat::expect_equal(to_r(v), c(FALSE, TRUE))
})

# unordered_multiset
testthat::test_that("unordered_multiset insert integer", {
  v <- cpp_unordered_multiset(4:9)
  testthat::expect_invisible(insert(v, 12:13))
  testthat::expect_setequal(to_r(v), c(4:9, 12:13))
})
testthat::test_that("unordered_multiset insert double", {
  v <- cpp_unordered_multiset(seq.int(1, 2, 0.5))
  testthat::expect_invisible(insert(v, c(12, 13)))
  testthat::expect_setequal(to_r(v), c(seq.int(1, 2, 0.5), c(12, 13)))
})
testthat::test_that("unordered_multiset insert string", {
  v <- cpp_unordered_multiset(c("hello", "there"))
  testthat::expect_invisible(insert(v, c("world", "a")))
  testthat::expect_setequal(to_r(v), c("a", "hello", "there", "world"))
})
testthat::test_that("unordered_multiset insert boolean", {
  v <- cpp_unordered_multiset(TRUE)
  testthat::expect_invisible(insert(v, FALSE))
  testthat::expect_setequal(to_r(v), c(FALSE, TRUE))
})

# map
testthat::test_that("map insert integer integer", {
  v <- cpp_map(4:9, 12:17)
  testthat::expect_invisible(insert(v, 6:7, 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c(12:17, 6:7)))
})
testthat::test_that("map insert integer double", {
  v <- cpp_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("map insert integer string", {
  v <- cpp_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert(v, c("there", "world"), 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c("Once", "upon", "a", "time", "in", "R", "there", "world")))
})
testthat::test_that("map insert integer boolean", {
  v <- cpp_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("map insert double integer", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(insert(v, 10:11, c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(12:17, 10:11)))
})
testthat::test_that("map insert double double", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("map insert double string", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert(v, c("there", "world"), c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c("Once", "upon", "a", "time", "in", "R", "there",
    "world")))
})
testthat::test_that("map insert double boolean", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("map insert string integer", {
  v <- cpp_map(c("A", "quick", "test"), 12:14)
  testthat::expect_invisible(insert(v, 10:11, c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c(12:10, 13:14)))
})
testthat::test_that("map insert string double", {
  v <- cpp_map(c("A", "quick", "test"), seq.int(1, 2.0, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c(1, 4.3, 4.2, 1.5, 2)))
})
testthat::test_that("map insert string string", {
  v <- cpp_map(c("A", "quick", "test"), c("Once", "upon", "R"))
  testthat::expect_invisible(insert(v, c("a", "time"), c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c("Once", "time", "a", "upon", "R")))
})
testthat::test_that("map insert string boolean", {
  v <- cpp_map(c("A", "quick", "test"), c(TRUE, FALSE, TRUE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c(TRUE, FALSE, TRUE, FALSE, TRUE)))
})
testthat::test_that("map insert boolean integer", {
  v <- cpp_map(TRUE, 12L)
  testthat::expect_invisible(insert(v, 13L, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = 13:12))
})
testthat::test_that("map insert boolean double", {
  v <- cpp_map(TRUE, 1.5)
  testthat::expect_invisible(insert(v, 1, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(1, 1.5)))
})
testthat::test_that("map insert boolean string", {
  v <- cpp_map(TRUE, "package")
  testthat::expect_invisible(insert(v, "a", FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c("a", "package")))
})
testthat::test_that("map insert boolean boolean", {
  v <- cpp_map(TRUE, FALSE)
  testthat::expect_invisible(insert(v, FALSE, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(FALSE, FALSE)))
})

# unordered_map
testthat::test_that("unordered_map insert integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  testthat::expect_invisible(insert(v, 6:7, 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c(12:17, 6:7)))
})
testthat::test_that("unordered_map insert integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("unordered_map insert integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert(v, c("there", "world"), 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c("Once", "upon", "a", "time", "in", "R", "there", "world")))
})
testthat::test_that("unordered_map insert integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("unordered_map insert double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(insert(v, 10:11, c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(12:17, 10:11)))
})
testthat::test_that("unordered_map insert double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("unordered_map insert double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert(v, c("there", "world"), c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c("Once", "upon", "a", "time", "in", "R", "there",
    "world")))
})
testthat::test_that("unordered_map insert double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("unordered_map insert string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test"), 12:14)
  testthat::expect_invisible(insert(v, 10:11, c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c(12:10, 13:14)))
})
testthat::test_that("unordered_map insert string double", {
  v <- cpp_unordered_map(c("A", "quick", "test"), seq.int(1, 2.0, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c(1, 4.3, 4.2, 1.5, 2)))
})
testthat::test_that("unordered_map insert string string", {
  v <- cpp_unordered_map(c("A", "quick", "test"), c("Once", "upon", "R"))
  testthat::expect_invisible(insert(v, c("a", "time"), c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c("Once", "time", "a", "upon", "R")))
})
testthat::test_that("unordered_map insert string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test"), c(TRUE, FALSE, TRUE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c(TRUE, FALSE, TRUE, FALSE, TRUE)))
})
testthat::test_that("unordered_map insert boolean integer", {
  v <- cpp_unordered_map(TRUE, 12L)
  testthat::expect_invisible(insert(v, 13L, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = 13:12))
})
testthat::test_that("unordered_map insert boolean double", {
  v <- cpp_unordered_map(TRUE, 1.5)
  testthat::expect_invisible(insert(v, 1, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(1, 1.5)))
})
testthat::test_that("unordered_map insert boolean string", {
  v <- cpp_unordered_map(TRUE, "package")
  testthat::expect_invisible(insert(v, "a", FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c("a", "package")))
})
testthat::test_that("unordered_map insert boolean boolean", {
  v <- cpp_unordered_map(TRUE, FALSE)
  testthat::expect_invisible(insert(v, FALSE, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(FALSE, FALSE)))
})

# multimap
testthat::test_that("multimap insert integer integer", {
  v <- cpp_multimap(4:9, 12:17)
  testthat::expect_invisible(insert(v, 6:7, 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c(12:17, 6:7)))
})
testthat::test_that("multimap insert integer double", {
  v <- cpp_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("multimap insert integer string", {
  v <- cpp_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert(v, c("there", "world"), 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c("Once", "upon", "a", "time", "in", "R", "there", "world")))
})
testthat::test_that("multimap insert integer boolean", {
  v <- cpp_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("multimap insert double integer", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(insert(v, 10:11, c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(12:17, 10:11)))
})
testthat::test_that("multimap insert double double", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("multimap insert double string", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert(v, c("there", "world"), c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c("Once", "upon", "a", "time", "in", "R", "there",
    "world")))
})
testthat::test_that("multimap insert double boolean", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("multimap insert string integer", {
  v <- cpp_multimap(c("A", "quick", "test"), 12:14)
  testthat::expect_invisible(insert(v, 10:11, c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c(12:10, 13:14)))
})
testthat::test_that("multimap insert string double", {
  v <- cpp_multimap(c("A", "quick", "test"), seq.int(1, 2.0, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c(1, 4.3, 4.2, 1.5, 2)))
})
testthat::test_that("multimap insert string string", {
  v <- cpp_multimap(c("A", "quick", "test"), c("Once", "upon", "R"))
  testthat::expect_invisible(insert(v, c("a", "time"), c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c("Once", "time", "a", "upon", "R")))
})
testthat::test_that("multimap insert string boolean", {
  v <- cpp_multimap(c("A", "quick", "test"), c(TRUE, FALSE, TRUE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c(TRUE, FALSE, TRUE, FALSE, TRUE)))
})
testthat::test_that("multimap insert boolean integer", {
  v <- cpp_multimap(TRUE, 12L)
  testthat::expect_invisible(insert(v, 13L, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = 13:12))
})
testthat::test_that("multimap insert boolean double", {
  v <- cpp_multimap(TRUE, 1.5)
  testthat::expect_invisible(insert(v, 1, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(1, 1.5)))
})
testthat::test_that("multimap insert boolean string", {
  v <- cpp_multimap(TRUE, "package")
  testthat::expect_invisible(insert(v, "a", FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c("a", "package")))
})
testthat::test_that("multimap insert boolean boolean", {
  v <- cpp_multimap(TRUE, FALSE)
  testthat::expect_invisible(insert(v, FALSE, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(FALSE, FALSE)))
})

# unordered_multimap
testthat::test_that("unordered_multimap insert integer integer", {
  v <- cpp_unordered_multimap(4:9, 12:17)
  testthat::expect_invisible(insert(v, 6:7, 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c(12:17, 6:7)))
})
testthat::test_that("unordered_multimap insert integer double", {
  v <- cpp_unordered_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("unordered_multimap insert integer string", {
  v <- cpp_unordered_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert(v, c("there", "world"), 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c("Once", "upon", "a", "time", "in", "R", "there", "world")))
})
testthat::test_that("unordered_multimap insert integer boolean", {
  v <- cpp_unordered_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("unordered_multimap insert double integer", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(insert(v, 10:11, c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(12:17, 10:11)))
})
testthat::test_that("unordered_multimap insert double double", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("unordered_multimap insert double string", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert(v, c("there", "world"), c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c("Once", "upon", "a", "time", "in", "R", "there",
    "world")))
})
testthat::test_that("unordered_multimap insert double boolean", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("unordered_multimap insert string integer", {
  v <- cpp_unordered_multimap(c("A", "quick", "test"), 12:14)
  testthat::expect_invisible(insert(v, 10:11, c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c(12:10, 13:14)))
})
testthat::test_that("unordered_multimap insert string double", {
  v <- cpp_unordered_multimap(c("A", "quick", "test"), seq.int(1, 2.0, 0.5))
  testthat::expect_invisible(insert(v, c(4.2, 4.3), c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c(1, 4.3, 4.2, 1.5, 2)))
})
testthat::test_that("unordered_multimap insert string string", {
  v <- cpp_unordered_multimap(c("A", "quick", "test"), c("Once", "upon", "R"))
  testthat::expect_invisible(insert(v, c("a", "time"), c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c("Once", "time", "a", "upon", "R")))
})
testthat::test_that("unordered_multimap insert string boolean", {
  v <- cpp_unordered_multimap(c("A", "quick", "test"), c(TRUE, FALSE, TRUE))
  testthat::expect_invisible(insert(v, c(TRUE, FALSE), c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c(TRUE, FALSE, TRUE, FALSE, TRUE)))
})
testthat::test_that("unordered_multimap insert boolean integer", {
  v <- cpp_unordered_multimap(TRUE, 12L)
  testthat::expect_invisible(insert(v, 13L, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = 13:12))
})
testthat::test_that("unordered_multimap insert boolean double", {
  v <- cpp_unordered_multimap(TRUE, 1.5)
  testthat::expect_invisible(insert(v, 1, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(1, 1.5)))
})
testthat::test_that("unordered_multimap insert boolean string", {
  v <- cpp_unordered_multimap(TRUE, "package")
  testthat::expect_invisible(insert(v, "a", FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c("a", "package")))
})
testthat::test_that("unordered_multimap insert boolean boolean", {
  v <- cpp_unordered_multimap(TRUE, FALSE)
  testthat::expect_invisible(insert(v, FALSE, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(FALSE, FALSE)))
})

# vector
testthat::test_that("vector insert integer", {
  v <- cpp_vector(4:9)
  testthat::expect_invisible(insert(v, 12:13, position = 2))
  testthat::expect_equal(to_r(v), c(4L, 12:13, 5:9))
})
testthat::test_that("vector insert double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_invisible(insert(v, c(12, 13), position = 2))
  testthat::expect_equal(to_r(v), c(1, 12, 13, 1.5, 2))
})
testthat::test_that("vector insert string", {
  v <- cpp_vector(c("hello", "there"))
  testthat::expect_invisible(insert(v, c("world", "a"), position = 2))
  testthat::expect_equal(to_r(v), c("hello", "world", "a", "there"))
})
testthat::test_that("vector insert boolean", {
  v <- cpp_vector(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert(v, c(FALSE, TRUE), position = 2))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, TRUE, FALSE, FALSE))
})

# deque
testthat::test_that("deque insert integer", {
  v <- cpp_deque(4:9)
  testthat::expect_invisible(insert(v, 12:13, position = 2))
  testthat::expect_equal(to_r(v), c(4L, 12:13, 5:9))
})
testthat::test_that("deque insert double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_invisible(insert(v, c(12, 13), position = 2))
  testthat::expect_equal(to_r(v), c(1, 12, 13, 1.5, 2))
})
testthat::test_that("deque insert string", {
  v <- cpp_deque(c("hello", "there"))
  testthat::expect_invisible(insert(v, c("world", "a"), position = 2))
  testthat::expect_equal(to_r(v), c("hello", "world", "a", "there"))
})
testthat::test_that("deque insert boolean", {
  v <- cpp_deque(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert(v, c(FALSE, TRUE), position = 2))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, TRUE, FALSE, FALSE))
})

# list
testthat::test_that("list insert integer", {
  v <- cpp_list(4:9)
  testthat::expect_invisible(insert(v, 12:13, position = 2))
  testthat::expect_equal(to_r(v), c(4L, 12:13, 5:9))
})
testthat::test_that("list insert double", {
  v <- cpp_list(seq.int(1, 2, 0.5))
  testthat::expect_invisible(insert(v, c(12, 13), position = 2))
  testthat::expect_equal(to_r(v), c(1, 12, 13, 1.5, 2))
})
testthat::test_that("list insert string", {
  v <- cpp_list(c("hello", "there"))
  testthat::expect_invisible(insert(v, c("world", "a"), position = 2))
  testthat::expect_equal(to_r(v), c("hello", "world", "a", "there"))
})
testthat::test_that("list insert boolean", {
  v <- cpp_list(c(TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert(v, c(FALSE, TRUE), position = 2))
  testthat::expect_equal(to_r(v), c(TRUE, FALSE, TRUE, FALSE, FALSE))
})
