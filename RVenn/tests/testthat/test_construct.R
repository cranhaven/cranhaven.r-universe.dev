# Testing the Venn() function ===================

bar = list(A = c(1, 2, 3, 4, 5), B = c(3, 4, 5, 6, 7), C = c(5, 6, 7, 8, 9))
foo = list(c(1, 2, 3, 4, 5), c(3, 4, 5, 6, 7), c(5, 6, 7, 8, 9))
baz = list(c(1, 2, 3, 4, 5), c(3, 4, 5, 6, 7), letters[1:5])
qux = list(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, FALSE))

test_that("Venn: checking types", {
  expect_equal(class(Venn(foo)@sets), "list")
  expect_equal(class(Venn(foo)@names), "character")
  expect_equal(class(Venn(bar)@names), "character")
}
)

test_that("Venn: errors", {
  expect_error(Venn(baz[[1]]))
  expect_error(Venn(baz))
  expect_error(Venn(qux))
}
)
