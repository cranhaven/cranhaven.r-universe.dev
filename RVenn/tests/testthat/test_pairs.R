bar = list(A = c(1, 2, 3, 4, 5), B = c(3, 4, 5, 6, 7), C = c(5, 6, 7, 8, 9))
foo = list(A = c(1, 2, 3, 4, 5), B = c(3, 4, 5, 6, 7),
           C = c(5, 6, 7, 8, 9), D = c(7, 8, 9, 10, 11))

# Testing pair functions ========================
test_that("overlap, unite and discern: all and slice", {
  expect_equal(class(overlap_pairs(Venn(bar))), "list")
  expect_equal(class(unite_pairs(Venn(bar))), "list")
  expect_equal(class(discern_pairs(Venn(bar))), "list")
  expect_equal(class(overlap_pairs(Venn(foo), slice = 2:4)), "list")
  expect_equal(class(unite_pairs(Venn(foo), slice = 2:4)), "list")
  expect_equal(class(discern_pairs(Venn(foo), slice = 2:4)), "list")
}
)
