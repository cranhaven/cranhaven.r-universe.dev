foo = list(A = c(1, 2, 3, 4, 5), B = c(3, 4, 5, 6, 7),
           C = c(5, 6, 7, 8, 9), D = c(7, 8, 9, 10, 11))


# Testing enrichment_test() function ============
test_that("encrihment_test: p-value", {
  expect_equal(class(enrichment_test(Venn(foo), "A", "B")), "list")
  expect_equal(class(enrichment_test(Venn(foo), 1, 2)), "list")
  expect_equal(class(enrichment_test(Venn(foo), 1, 2, univ = 1:1000)), "list")
}
)

test_that("enrichment_test: error", {
  expect_error(enrichment_test(Venn(foo), 1, 2, n = 100))
}
)
