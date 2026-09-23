test_that("Missing df", {
  expect_error(mr_2sls())
})

test_that("Wrong df", {
  expect_error(mr_2sls(df = "WRONG_DF"))
})

test_that("Insufficient number of Column df", {
  expect_error(mr_2sls(df = data.frame(matrix(NA, nrow = 10, ncol = 3))))
})

test_that("Null prs_col", {
  expect_error(mr_2sls(df = comorbidData, prs_col = NULL, exposure_col = "log_ldl", outcome_col = "bmi"))
})

test_that("NA prs_col", {
  expect_error(mr_2sls(df = comorbidData, prs_col = NA, exposure_col = "log_ldl", outcome_col = "bmi"))
})


test_that("Wrong prs_col", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "WRONG_PGS", exposure_col = "log_ldl", outcome_col = "bmi"))
})


test_that("Not numeric prs_col", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "hypertension", exposure_col = "log_ldl", outcome_col = "bmi"))
})

test_that("Null exposure_col", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = NULL, outcome_col = "bmi"))
})

test_that("NA exposure_col", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = NA, outcome_col = "bmi"))
})

test_that("Wrong exposure_col", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = "WRONG_EXPOSURE", outcome_col = "bmi"))
})

test_that("Null outcome_col", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = "bmi", outcome_col = NULL))
})

test_that("NA outcome_col", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = "bmi", outcome_col = NA))
})

test_that("Wrong outcome_col", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = "bmi", outcome_col = "WRONG_OUTCOME"))
})

test_that("Wrong scale", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = "log_ldl", outcome_col = "bmi", scale = "WRONG_SCALE"))
})

test_that("Wrong verbose", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = "log_ldl", outcome_col = "bmi", verbose = "WRONG_SCALE"))
})

test_that("Wrong Exposure: only one value", {
  expect_error(mr_2sls(df = cbind(comorbidData, data.frame("WRONG_PHENO" = rep(0, nrow(comorbidData)))), prs_col = "ldl_PGS", exposure_col = "WRONG_PHENO", outcome_col = "bmi"))
})

test_that("Wrong Exposure: categorical value", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = "sbp_cat", outcome_col = "bmi"))
})

test_that("Wrong Outcome: categorical value", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = "bmi", outcome_col = "sbp_cat"))
})

test_that("Null log", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = "log_ldl", outcome_col = "bmi", log = NULL))
})

test_that("Wrong log", {
  expect_error(mr_2sls(df = comorbidData, prs_col = "ldl_PGS", exposure_col = "log_ldl", outcome_col = "bmi", log = 1))
})

test_that("Binary exposure", {
  expect_warning(mr_2sls(df = comorbidData, prs_col = "t2d_PGS", exposure_col = "t2d", outcome_col = "bmi"))
})

for (prs in grep("PGS", names(comorbidData), value = T)) {
  for (exposure in c("bmi","sbp")) {
    for (outcome in c("t2d","log_ldl")) {
      test_that(paste("Test of MR using", prs, "between", exposure, "and", outcome), {
        expect_s3_class(
          object = mr_2sls(df = comorbidData, prs_col = prs, exposure_col =
                              exposure, outcome_col = outcome, verbose = FALSE),
          class = "data.frame"
        )
      })
    }
  }
}
