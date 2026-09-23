test_that("Wrong threshold", {
  expect_warning(densityplot(
    df = comorbidData, prs_col = "ldl_PGS",
    phenotype_col = "log_ldl",
    threshold = "WRONG_THRESHOLD"
  ))
})

for (scaling in c(T, F)) {
  test_that(paste("Test with scaling =", scaling, "; threshold = NA ; Cases/Controls Phenotype"), {
    expect_s3_class(
      object = densityplot(
        df = comorbidData, prs_col = "ldl_PGS",
        phenotype_col = "t2d",
        scale = scaling
      ),
      class = "ggplot"
    )
  })

  test_that(paste("Test with scaling =", scaling, "; threshold = NA ; Categorical Phenotype"), {
    expect_s3_class(
      object = densityplot(
        df = comorbidData, prs_col = "ldl_PGS",
        phenotype_col = "sbp_cat",
        scale = scaling
      ),
      class = "ggplot"
    )
  })

  test_that(paste("Test with scaling =", scaling, "; threshold = 1.5 ; Continuous Phenotype"), {
    expect_s3_class(
      object = densityplot(
        df = comorbidData, prs_col = "ldl_PGS",
        phenotype_col = "log_ldl",
        scale = scaling, threshold = 1.5
      ),
      class = "ggplot"
    )
  })
}
