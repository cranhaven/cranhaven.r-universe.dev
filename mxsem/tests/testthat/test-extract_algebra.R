test_that("extracting algebra", {
  library(mxsem)
  simple_algebra <- mxAlgebraFromString("exp(b + c)", name = "a")

  alg <- mxsem:::extract_algebra_elements(simple_algebra@formula)

  testthat::expect_equal(alg, c("b", "c"))
})
