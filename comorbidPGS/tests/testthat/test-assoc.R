test_that("Missing df", {
  expect_error(assoc())
})

test_that("Wrong df", {
  expect_error(assoc(df = "WRONG_DF"))
})

test_that("Insufficient number of Column df", {
  expect_error(assoc(df = data.frame(matrix(NA, nrow = 10, ncol = 2))))
})

test_that("Null prs_col", {
  expect_error(assoc(df = comorbidData, prs_col = NULL, phenotype_col = "log_ldl"))
})

test_that("NA prs_col", {
  expect_error(assoc(df = comorbidData, prs_col = NA, phenotype_col = "log_ldl"))
})


test_that("Wrong prs_col", {
  expect_error(assoc(df = comorbidData, prs_col = "WRONG_PGS", phenotype_col = "log_ldl"))
})


test_that("Not numeric prs_col", {
  expect_error(assoc(df = comorbidData, prs_col = "hypertension", phenotype_col = "log_ldl"))
})

test_that("Null phenotype_col", {
  expect_error(assoc(df = comorbidData, prs_col = "ldl_PGS", phenotype_col = NULL))
})

test_that("NA phenotype_col", {
  expect_error(assoc(df = comorbidData, prs_col = "ldl_PGS", phenotype_col = NA))
})

test_that("Wrong phenotype_col", {
  expect_error(assoc(df = comorbidData, prs_col = "ldl_PGS", phenotype_col = "WRONG_PHENOTYPE"))
})

test_that("Wrong scale", {
  expect_error(assoc(df = comorbidData, prs_col = "ldl_PGS", phenotype_col = "log_ldl", scale = "WRONG_SCALE"))
})

test_that("Wrong verbose", {
  expect_error(assoc(df = comorbidData, prs_col = "ldl_PGS", phenotype_col = "log_ldl", verbose = "WRONG_SCALE"))
})

test_that("Wrong Phenotype: only one value", {
  expect_error(assoc(df = cbind(comorbidData, data.frame("WRONG_PHENO" = rep(0, nrow(comorbidData)))), prs_col = "ldl_PGS", phenotype_col = "WRONG_PHENO"))
})

df <- comorbidData
df$test <- (sample(c(0, 2), nrow(df), replace = T) + df$ldl_PGS / max(df$ldl_PGS))
df$test2 <- c(df[1:4000, "test"], rep(NA, nrow(df)-4000))
test_that("Continuous Phenotype without normal distribution (N_pheno > 5000)", {
  expect_warning(assoc(df = df, prs_col = "ldl_PGS", phenotype_col = "test"))
})
test_that("Continuous Phenotype without normal distribution (N_pheno < 5000)", {
  expect_warning(assoc(df = df, prs_col = "ldl_PGS", phenotype_col = "test2"))
})

test_that("Wrong covar_col", {
  expect_error(assoc(df = comorbidData, prs_col = "ldl_PGS", phenotype_col = "log_ldl", scale = "WRONG_COVARIATE"))
})

test_that("Not enough sample/individuals", {
  expect_error(assoc(df = cbind(comorbidData, data.frame("WRONG_PHENO" = rep(NA, nrow(comorbidData)))), prs_col = "ldl_PGS", phenotype_col = "WRONG_PHENO"))
})

test_that("Null log", {
  expect_error(assoc(df = comorbidData, prs_col = "ldl_PGS", phenotype_col = "log_ldl", log = NULL))
})

test_that("Wrong log", {
  expect_error(assoc(df = comorbidData, prs_col = "ldl_PGS", phenotype_col = "log_ldl", log = 1))
})

for (prs in grep("PGS", names(comorbidData), value = T)) {
  for (phenotype in c("ethnicity","brc","t2d","log_ldl","sbp_cat")) {
    test_that(paste("Test of", prs, "on", phenotype), {
      expect_s3_class(
        object = assoc(df = comorbidData, prs_col = prs, phenotype_col = phenotype, verbose = FALSE),
        class = "data.frame"
      )
    })
  }
}
