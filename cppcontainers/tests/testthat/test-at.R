skip_on_os("mac")
# map
testthat::test_that("map at integer integer", {
  v <- cpp_map(4:9, 12:17)
  testthat::expect_equal(at(v, 6L), 14L)
})
testthat::test_that("map at integer double", {
  v <- cpp_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(at(v, 6L), 2)
})
testthat::test_that("map at integer string", {
  v <- cpp_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(at(v, 6L), "a")
})
testthat::test_that("map at integer boolean", {
  v <- cpp_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(at(v, 6L), TRUE)
})
testthat::test_that("map at double integer", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(at(v, 3), 14L)
})
testthat::test_that("map at double double", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(at(v, 3), 2)
})
testthat::test_that("map at double string", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(at(v, 3), "a")
})
testthat::test_that("map at double boolean", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(at(v, 3), TRUE)
})
testthat::test_that("map at string integer", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(at(v, "test"), 14L)
})
testthat::test_that("map at string double", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(at(v, "test"), 2)
})
testthat::test_that("map at string string", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(at(v, "test"), "a")
})
testthat::test_that("map at string boolean", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(at(v, "test"), TRUE)
})
testthat::test_that("map at boolean integer", {
  v <- cpp_map(c(TRUE, FALSE), 12:13)
  testthat::expect_equal(at(v, FALSE), 13L)
})
testthat::test_that("map at boolean double", {
  v <- cpp_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  testthat::expect_equal(at(v, FALSE), 1.5)
})
testthat::test_that("map at boolean string", {
  v <- cpp_map(c(TRUE, FALSE), c("Once", "upon"))
  testthat::expect_equal(at(v, FALSE), "upon")
})
testthat::test_that("map at boolean boolean", {
  v <- cpp_map(c(TRUE, FALSE), c(FALSE, TRUE))
  testthat::expect_equal(at(v, FALSE), TRUE)
})

# unordered_map
testthat::test_that("unordered_map at integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  testthat::expect_equal(at(v, 6L), 14L)
})
testthat::test_that("unordered_map at integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(at(v, 6L), 2)
})
testthat::test_that("unordered_map at integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(at(v, 6L), "a")
})
testthat::test_that("unordered_map at integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(at(v, 6L), TRUE)
})
testthat::test_that("unordered_map at double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(at(v, 3), 14L)
})
testthat::test_that("unordered_map at double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(at(v, 3), 2)
})
testthat::test_that("unordered_map at double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(at(v, 3), "a")
})
testthat::test_that("unordered_map at double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(at(v, 3), TRUE)
})
testthat::test_that("unordered_map at string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(at(v, "test"), 14L)
})
testthat::test_that("unordered_map at string double", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(at(v, "test"), 2)
})
testthat::test_that("unordered_map at string string", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(at(v, "test"), "a")
})
testthat::test_that("unordered_map at string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(at(v, "test"), TRUE)
})
testthat::test_that("unordered_map at boolean integer", {
  v <- cpp_unordered_map(c(TRUE, FALSE), 12:13)
  testthat::expect_equal(at(v, FALSE), 13L)
})
testthat::test_that("unordered_map at boolean double", {
  v <- cpp_unordered_map(c(TRUE, FALSE), seq.int(1, 1.5, 0.5))
  testthat::expect_equal(at(v, FALSE), 1.5)
})
testthat::test_that("unordered_map at boolean string", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c("Once", "upon"))
  testthat::expect_equal(at(v, FALSE), "upon")
})
testthat::test_that("unordered_map at boolean boolean", {
  v <- cpp_unordered_map(c(TRUE, FALSE), c(FALSE, TRUE))
  testthat::expect_equal(at(v, FALSE), TRUE)
})

# vector
testthat::test_that("vector at integer", {
  v <- cpp_vector(4:9)
  testthat::expect_equal(at(v, 3L), 6L)
})
testthat::test_that("vector at double", {
  v <- cpp_vector(seq.int(1, 2, 0.5))
  testthat::expect_equal(at(v, 3L), 2)
})
testthat::test_that("vector at string", {
  v <- cpp_vector(c("hello", "there"))
  testthat::expect_equal(at(v, 2L), "there")
})
testthat::test_that("vector at boolean", {
  v <- cpp_vector(c(TRUE, FALSE, TRUE))
  testthat::expect_equal(at(v, 2L), FALSE)
})

# deque
testthat::test_that("deque at integer", {
  v <- cpp_deque(4:9)
  testthat::expect_equal(at(v, 3L), 6L)
})
testthat::test_that("deque at double", {
  v <- cpp_deque(seq.int(1, 2, 0.5))
  testthat::expect_equal(at(v, 3L), 2)
})
testthat::test_that("deque at string", {
  v <- cpp_deque(c("hello", "there"))
  testthat::expect_equal(at(v, 2L), "there")
})
testthat::test_that("deque at boolean", {
  v <- cpp_deque(c(TRUE, FALSE, TRUE))
  testthat::expect_equal(at(v, 2L), FALSE)
})
