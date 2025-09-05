# Testing ggvenn() function =====================
bar = list(A = c(1, 2, 3, 4, 5), B = c(3, 4, 5, 6, 7), C = c(5, 6, 7, 8, 9), D = c(10, 11, 12))
foo = list(c(1, 2, 3, 4, 5), c(3, 4, 5, 6, 7), c(5, 6, 7, 8, 9))
baz = list(c(1, 2, 3, 4, 5), c(3, 4, 5, 6, 7))

test_that("ggvenn: plot for 2 and 3 sets", {
  g1 = ggvenn(Venn(baz))
  g2 = ggvenn(Venn(foo))
  expect_equal(class(g1), c("gg", "ggplot"))
  expect_equal(class(g2), c("gg", "ggplot"))
}
)

test_that("ggvenn: slices", {
  v1 = Venn(bar)
  expect_equal(class(ggvenn(v1, slice = c(2, 4))), c("gg", "ggplot"))
  expect_equal(class(ggvenn(v1, slice = c("A", "B", "C"))), c("gg", "ggplot"))
}
)

test_that("ggvenn: error", {
  v1 = Venn(bar)
  v2 = Venn(foo)
  expect_error(ggvenn(v1))
  expect_error(ggvenn(v2, alpha = 1))
  # expect_error(ggvenn(v2, fill = "dodgerblue3"))
}
)
