test_that("Only one Phenotype", {
  expect_warning(multiphenassoc(df = comorbidData, prs_col = "ldl_PGS", phenotype_col = "brc"))
})

test_that("Null log", {
  expect_error(multiphenassoc(df = comorbidData, prs_col = "ldl_PGS", phenotype_col = c("brc","t2d"), log = NULL))
})

test_that("Wrong log", {
  expect_error(assoc(df = comorbidData, prs_col = "ldl_PGS", phenotype_col = c("brc","t2d"), log = 1))
})

prs <- grep("PGS", names(comorbidData), value = T)[1]
phenotype <- c("ethnicity","brc","t2d","log_ldl","sbp_cat")

test_that("Test of first PGS and several Phenotypes using an assoc_table matrix and covariates", {
  expect_s3_class(
    object = multiphenassoc(df = comorbidData, prs_col = prs, phenotype_col = phenotype, covar_col = c("age", "sex", "gen_array"), verbose = FALSE),
    class = "data.frame"
  )
})
