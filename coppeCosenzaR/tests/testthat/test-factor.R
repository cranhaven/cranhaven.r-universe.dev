require(testthat)

context("\n\nFactor constructor\n")

test_that("Factor constructor\n", {
  expect_error(Factor(""), "Factor@name cannot be void")
  expect_error(Factor("   "), "Factor@name cannot be only blanc spaces")
  expect_error(Factor("test", "test2"), )
  expect_error(Factor(), )
  expect_is(Factor(2L),"Factor")
  expect_is(Factor(TRUE), "Factor")
  expect_is(Factor(3.5), "Factor")
  expect_is(Factor("test.factor"), "Factor")
}
)


context("\n\nFactors.of.interest constructor\n")

fat1 <- Factor("fat1")
fat2 <- Factor("_23FAT")
fat3 <- Factor(22)
fat4 <- Factor(TRUE)
fat5 <- 10
fat6 <- NA

test_that("Factors.of.interest constructor\n", {
  expect_error(Factors.of.interest(), )
  expect_error(Factors.of.interest("   "), )
  expect_error(Factors.of.interest(list(fat1,fat5)), "'@all' must be a list of Factor S4 objects")
  expect_is(Factors.of.interest(list(fat1, fat2, fat3, fat4)),"Factors.of.interest")
  expect_is(Factors.of.interest(list(Factor("fat1"), Factor(TRUE), Factor(22))), "Factors.of.interest")
}
)

