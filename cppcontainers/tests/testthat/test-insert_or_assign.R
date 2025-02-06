skip_on_os("mac")
# map
testthat::test_that("map insert_or_assign integer integer", {
  v <- cpp_map(4:9, 12:17)
  testthat::expect_invisible(insert_or_assign(v, 6:7, 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c(12:17, 6:7)))
})
testthat::test_that("map insert_or_assign integer double", {
  v <- cpp_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert_or_assign(v, c(4.2, 4.3), 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("map insert_or_assign integer string", {
  v <- cpp_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert_or_assign(v, c("there", "world"), 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c("Once", "upon", "a", "time", "in", "R", "there", "world")))
})
testthat::test_that("map insert_or_assign integer boolean", {
  v <- cpp_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert_or_assign(v, c(TRUE, FALSE), 10:11))
  testthat::expect_equal(to_r(v), data.frame(key = 4:11, value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("map insert_or_assign double integer", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(insert_or_assign(v, 10:11, c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(12:17, 10:11)))
})
testthat::test_that("map insert_or_assign double double", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert_or_assign(v, c(4.2, 4.3), c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("map insert_or_assign double string", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert_or_assign(v, c("there", "world"), c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c("Once", "upon", "a", "time", "in", "R", "there",
    "world")))
})
testthat::test_that("map insert_or_assign double boolean", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert_or_assign(v, c(TRUE, FALSE), c(6.1, 6.3)))
  testthat::expect_equal(to_r(v), data.frame(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("map insert_or_assign string integer", {
  v <- cpp_map(c("A", "quick", "test"), 12:14)
  testthat::expect_invisible(insert_or_assign(v, 10:11, c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c(12:10, 13:14)))
})
testthat::test_that("map insert_or_assign string double", {
  v <- cpp_map(c("A", "quick", "test"), seq.int(1, 2.0, 0.5))
  testthat::expect_invisible(insert_or_assign(v, c(4.2, 4.3), c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c(1, 4.3, 4.2, 1.5, 2)))
})
testthat::test_that("map insert_or_assign string string", {
  v <- cpp_map(c("A", "quick", "test"), c("Once", "upon", "R"))
  testthat::expect_invisible(insert_or_assign(v, c("a", "time"), c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c("Once", "time", "a", "upon", "R")))
})
testthat::test_that("map insert_or_assign string boolean", {
  v <- cpp_map(c("A", "quick", "test"), c(TRUE, FALSE, TRUE))
  testthat::expect_invisible(insert_or_assign(v, c(TRUE, FALSE), c("in", "here")))
  testthat::expect_equal(to_r(v), data.frame(key = c("A", "here", "in", "quick", "test"), value = c(TRUE, FALSE, TRUE, FALSE, TRUE)))
})
testthat::test_that("map insert_or_assign boolean integer", {
  v <- cpp_map(TRUE, 12L)
  testthat::expect_invisible(insert_or_assign(v, 13L, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = 13:12))
})
testthat::test_that("map insert_or_assign boolean double", {
  v <- cpp_map(TRUE, 1.5)
  testthat::expect_invisible(insert_or_assign(v, 1, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(1, 1.5)))
})
testthat::test_that("map insert_or_assign boolean string", {
  v <- cpp_map(TRUE, "package")
  testthat::expect_invisible(insert_or_assign(v, "a", FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c("a", "package")))
})
testthat::test_that("map insert_or_assign boolean boolean", {
  v <- cpp_map(TRUE, FALSE)
  testthat::expect_invisible(insert_or_assign(v, FALSE, FALSE))
  testthat::expect_equal(to_r(v), data.frame(key = c(FALSE, TRUE), value = c(FALSE, FALSE)))
})

# unordered_map
testthat::test_that("unordered_map insert_or_assign integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  testthat::expect_invisible(insert_or_assign(v, 6:7, 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c(12:17, 6:7)))
})
testthat::test_that("unordered_map insert_or_assign integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert_or_assign(v, c(4.2, 4.3), 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("unordered_map insert_or_assign integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert_or_assign(v, c("there", "world"), 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c("Once", "upon", "a", "time", "in", "R", "there", "world")))
})
testthat::test_that("unordered_map insert_or_assign integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert_or_assign(v, c(TRUE, FALSE), 10:11))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = 4:11, value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("unordered_map insert_or_assign double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_invisible(insert_or_assign(v, 10:11, c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(12:17, 10:11)))
})
testthat::test_that("unordered_map insert_or_assign double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_invisible(insert_or_assign(v, c(4.2, 4.3), c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(seq.int(1, 3.5, 0.5), c(4.2, 4.3))))
})
testthat::test_that("unordered_map insert_or_assign double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_invisible(insert_or_assign(v, c("there", "world"), c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c("Once", "upon", "a", "time", "in", "R", "there",
    "world")))
})
testthat::test_that("unordered_map insert_or_assign double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_invisible(insert_or_assign(v, c(TRUE, FALSE), c(6.1, 6.3)))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(seq.int(2, 4.5, 0.5), c(6.1, 6.3)), value = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)))
})
testthat::test_that("unordered_map insert_or_assign string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test"), 12:14)
  testthat::expect_invisible(insert_or_assign(v, 10:11, c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c(12:10, 13:14)))
})
testthat::test_that("unordered_map insert_or_assign string double", {
  v <- cpp_unordered_map(c("A", "quick", "test"), seq.int(1, 2.0, 0.5))
  testthat::expect_invisible(insert_or_assign(v, c(4.2, 4.3), c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c(1, 4.3, 4.2, 1.5, 2)))
})
testthat::test_that("unordered_map insert_or_assign string string", {
  v <- cpp_unordered_map(c("A", "quick", "test"), c("Once", "upon", "R"))
  testthat::expect_invisible(insert_or_assign(v, c("a", "time"), c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c("Once", "time", "a", "upon", "R")))
})
testthat::test_that("unordered_map insert_or_assign string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test"), c(TRUE, FALSE, TRUE))
  testthat::expect_invisible(insert_or_assign(v, c(TRUE, FALSE), c("in", "here")))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c("A", "here", "in", "quick", "test"), value = c(TRUE, FALSE, TRUE, FALSE, TRUE)))
})
testthat::test_that("unordered_map insert_or_assign boolean integer", {
  v <- cpp_unordered_map(TRUE, 12L)
  testthat::expect_invisible(insert_or_assign(v, 13L, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = 13:12))
})
testthat::test_that("unordered_map insert_or_assign boolean double", {
  v <- cpp_unordered_map(TRUE, 1.5)
  testthat::expect_invisible(insert_or_assign(v, 1, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(1, 1.5)))
})
testthat::test_that("unordered_map insert_or_assign boolean string", {
  v <- cpp_unordered_map(TRUE, "package")
  testthat::expect_invisible(insert_or_assign(v, "a", FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c("a", "package")))
})
testthat::test_that("unordered_map insert_or_assign boolean boolean", {
  v <- cpp_unordered_map(TRUE, FALSE)
  testthat::expect_invisible(insert_or_assign(v, FALSE, FALSE))
  r <- testthat::expect_no_error(to_r(v))
  testthat::expect_equal(as.list(r[order(r$key),]), list(key = c(FALSE, TRUE), value = c(FALSE, FALSE)))
})
