skip_on_os("mac")
# set
testthat::test_that("set count integer", {
  v <- cpp_set(4:9)
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("set count double", {
  v <- cpp_set(seq.int(1, 2, 0.5))
  testthat::expect_equal(count(v, c(2, -1)), c(1, 0))
})
testthat::test_that("set count string", {
  v <- cpp_set(c("hello", "there"))
  testthat::expect_equal(count(v, c("there", "world")), c(1, 0))
})
testthat::test_that("set count boolean", {
  v <- cpp_set(TRUE)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})

# unordered_set
testthat::test_that("unordered_set count integer", {
  v <- cpp_unordered_set(4:9)
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("unordered_set count double", {
  v <- cpp_unordered_set(seq.int(1, 2, 0.5))
  testthat::expect_equal(count(v, c(2, -1)), c(1, 0))
})
testthat::test_that("unordered_set count string", {
  v <- cpp_unordered_set(c("hello", "there"))
  testthat::expect_equal(count(v, c("there", "world")), c(1, 0))
})
testthat::test_that("unordered_set count boolean", {
  v <- cpp_unordered_set(TRUE)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})

# multiset
testthat::test_that("multiset count integer", {
  v <- cpp_multiset(4:9)
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("multiset count double", {
  v <- cpp_multiset(seq.int(1, 2, 0.5))
  testthat::expect_equal(count(v, c(2, -1)), c(1, 0))
})
testthat::test_that("multiset count string", {
  v <- cpp_multiset(c("hello", "there"))
  testthat::expect_equal(count(v, c("there", "world")), c(1, 0))
})
testthat::test_that("multiset count boolean", {
  v <- cpp_multiset(TRUE)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})

# unordered_multiset
testthat::test_that("unordered_multiset count integer", {
  v <- cpp_unordered_multiset(4:9)
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("unordered_multiset count double", {
  v <- cpp_unordered_multiset(seq.int(1, 2, 0.5))
  testthat::expect_equal(count(v, c(2, -1)), c(1, 0))
})
testthat::test_that("unordered_multiset count string", {
  v <- cpp_unordered_multiset(c("hello", "there"))
  testthat::expect_equal(count(v, c("there", "world")), c(1, 0))
})
testthat::test_that("unordered_multiset count boolean", {
  v <- cpp_unordered_multiset(TRUE)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})

# map
testthat::test_that("map count integer integer", {
  v <- cpp_map(4:9, 12:17)
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("map count integer double", {
  v <- cpp_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("map count integer string", {
  v <- cpp_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("map count integer boolean", {
  v <- cpp_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("map count double integer", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("map count double double", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("map count double string", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("map count double boolean", {
  v <- cpp_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("map count string integer", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("map count string double", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("map count string string", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("map count string boolean", {
  v <- cpp_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("map count boolean integer", {
  v <- cpp_map(TRUE, 12L)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("map count boolean double", {
  v <- cpp_map(TRUE, 1.5)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("map count boolean string", {
  v <- cpp_map(TRUE, "upon")
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("map count boolean boolean", {
  v <- cpp_map(TRUE, FALSE)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})

# unordered_map
testthat::test_that("unordered_map count integer integer", {
  v <- cpp_unordered_map(4:9, 12:17)
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("unordered_map count integer double", {
  v <- cpp_unordered_map(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("unordered_map count integer string", {
  v <- cpp_unordered_map(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("unordered_map count integer boolean", {
  v <- cpp_unordered_map(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("unordered_map count double integer", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("unordered_map count double double", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("unordered_map count double string", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("unordered_map count double boolean", {
  v <- cpp_unordered_map(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("unordered_map count string integer", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("unordered_map count string double", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("unordered_map count string string", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("unordered_map count string boolean", {
  v <- cpp_unordered_map(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("unordered_map count boolean integer", {
  v <- cpp_unordered_map(TRUE, 12L)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("unordered_map count boolean double", {
  v <- cpp_unordered_map(TRUE, 1.5)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("unordered_map count boolean string", {
  v <- cpp_unordered_map(TRUE, "upon")
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("unordered_map count boolean boolean", {
  v <- cpp_unordered_map(TRUE, FALSE)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})

# multimap
testthat::test_that("multimap count integer integer", {
  v <- cpp_multimap(4:9, 12:17)
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("multimap count integer double", {
  v <- cpp_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("multimap count integer string", {
  v <- cpp_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("multimap count integer boolean", {
  v <- cpp_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("multimap count double integer", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("multimap count double double", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("multimap count double string", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("multimap count double boolean", {
  v <- cpp_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("multimap count string integer", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("multimap count string double", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("multimap count string string", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("multimap count string boolean", {
  v <- cpp_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("multimap count boolean integer", {
  v <- cpp_multimap(TRUE, 12L)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("multimap count boolean double", {
  v <- cpp_multimap(TRUE, 1.5)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("multimap count boolean string", {
  v <- cpp_multimap(TRUE, "upon")
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("multimap count boolean boolean", {
  v <- cpp_multimap(TRUE, FALSE)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})

# unordered_multimap
testthat::test_that("unordered_multimap count integer integer", {
  v <- cpp_unordered_multimap(4:9, 12:17)
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("unordered_multimap count integer double", {
  v <- cpp_unordered_multimap(4:9, seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("unordered_multimap count integer string", {
  v <- cpp_unordered_multimap(4:9, c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("unordered_multimap count integer boolean", {
  v <- cpp_unordered_multimap(4:9, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c(6L, -1L)), c(1, 0))
})
testthat::test_that("unordered_multimap count double integer", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), 12:17)
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("unordered_multimap count double double", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("unordered_multimap count double string", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("unordered_multimap count double boolean", {
  v <- cpp_unordered_multimap(seq.int(2, 4.5, 0.5), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c(3, -1)), c(1, 0))
})
testthat::test_that("unordered_multimap count string integer", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), 12:17)
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("unordered_multimap count string double", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), seq.int(1, 3.5, 0.5))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("unordered_multimap count string string", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c("Once", "upon", "a", "time", "in", "R"))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("unordered_multimap count string boolean", {
  v <- cpp_unordered_multimap(c("A", "quick", "test", "of", "the", "package"), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(count(v, c("the", "world")), c(1, 0))
})
testthat::test_that("unordered_multimap count boolean integer", {
  v <- cpp_unordered_multimap(TRUE, 12L)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("unordered_multimap count boolean double", {
  v <- cpp_unordered_multimap(TRUE, 1.5)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("unordered_multimap count boolean string", {
  v <- cpp_unordered_multimap(TRUE, "upon")
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
testthat::test_that("unordered_multimap count boolean boolean", {
  v <- cpp_unordered_multimap(TRUE, FALSE)
  testthat::expect_equal(count(v, c(TRUE, FALSE)), c(1, 0))
})
