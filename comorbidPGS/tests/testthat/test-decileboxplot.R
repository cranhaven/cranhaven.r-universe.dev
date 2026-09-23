test_that("Cases/Controls Phenotype", {
  expect_error(decileboxplot(
    df = comorbidData, prs_col = "brc_PGS",
    phenotype_col = "brc"
  ))
})

test_that("Categorical Phenotype", {
  expect_error(decileboxplot(
    df = comorbidData, prs_col = "t2d_PGS",
    phenotype_col = "t2d"
  ))
})

test_that("Not enough sample/individual warning for centiles", {
  expect_warning(decileboxplot(
    df = comorbidData[1:999, ], prs_col = "brc_PGS",
    phenotype_col = "log_ldl"
  ))
})

test_that("Test of decileboxplot using a Continuous Phenotype from comorbidData", {
  expect_s3_class(
    object = decileboxplot(
      df = comorbidData, prs_col = "brc_PGS",
      phenotype_col = "log_ldl"
    ),
    class = "ggplot"
  )
})
