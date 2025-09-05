bar = list(A = c(1, 2, 3, 4, 5), B = c(3, 4, 5, 6, 7), C = c(5, 6, 7, 8, 9))
foo = list(A = c(1, 2, 3, 4, 5), B = c(3, 4, 5, 6, 7),
           C = c(5, 6, 7, 8, 9), D = c(7, 8, 9, 10, 11))


# Testing overlap() and unite () functions ======
test_that("overlap & unite: all and slice", {
  expect_equal(overlap(Venn(bar)), 5)
  expect_equal(overlap(Venn(bar), slice = c(1, 2)), c(3, 4, 5))
  expect_equal(overlap(Venn(bar), slice = c("A", "B")), c(3, 4, 5))
  expect_equal(unite(Venn(bar)), as.numeric(1:9))
  expect_equal(unite(Venn(bar), slice = c(1, 2)), 1:7)
  expect_equal(unite(Venn(bar), slice = c("A", "B")), 1:7)
}
)

# Testing discern() function ====================
test_that("discern: all and slice", {
  expect_equal(discern(Venn(foo), 1, 2), c(1, 2))
  expect_equal(discern(Venn(foo), 1, c(2, 3)), c(1, 2))
  expect_equal(discern(Venn(foo), c(3, 4), 1), 6:11)
  expect_equal(discern(Venn(foo), "A", "B"), c(1, 2))
  expect_equal(discern(Venn(foo), "A", c("B", "D")), c(1, 2))
  expect_equal(discern(Venn(foo), c("C", "D"), "A"), 6:11)
}
)
