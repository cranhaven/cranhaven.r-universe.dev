test_that("Wrong continuous_metric", {
  expect_error(centileplot(
    df = comorbidData, prs_col = "ldl_PGS",
    phenotype_col = "t2d",
    continuous_metric = "WRONG_CONTINUOUS_METRIC"
  ))
})

test_that("Not enough sample/individual warning for centiles", {
  expect_warning(centileplot(
    df = comorbidData[1:9000, ], prs_col = "ldl_PGS",
    phenotype_col = "log_ldl",
    decile = F
  ))
})

test_that("Categorical Phenotype", {
  expect_error(centileplot(
    df = comorbidData, prs_col = "t2d_PGS",
    phenotype_col = "sbp_cat"
  ))
})

for (deciling in c(T, F)) {
  for (cont_met in c(NA, "mean", "median")) {
    test_that(paste("Test with centile =", deciling, "; continuous_metric =", cont_met), {
      expect_s3_class(
        object = centileplot(
          df = comorbidData, prs_col = "ldl_PGS",
          phenotype_col = ifelse(!is.na(cont_met), "log_ldl", "t2d"),
          decile = deciling, continuous_metric = cont_met
        ),
        class = "ggplot"
      )
    })
  }
}
