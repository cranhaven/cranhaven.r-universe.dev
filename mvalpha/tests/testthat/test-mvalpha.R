test_that(
  "mvalpha returns the expected value", {
    load(file = testthat::test_path("testdata", "ex_table3.rda"))
    testthat::expect_equal(
      round(mvalpha(ex_table3)$mvalpha, 4),
      round(0.2865907, 4)
    )
})
