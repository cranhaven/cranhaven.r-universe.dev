test_that("Missing assoc_table", {
  expect_error(multiassoc(df = comorbidData, parallel = FALSE))
})

test_that("Wrong assoc_table", {
  expect_error(multiassoc(df = comorbidData, assoc_table = "WRONG_ASSOC_TABLE", parallel = FALSE))
})

test_that("Wrong assoc_table", {
  expect_error(multiassoc(df = comorbidData, assoc_table = "WRONG_ASSOC_TABLE", parallel = FALSE))
})

test_that("Not enough column for assoc_table", {
  expect_error(multiassoc(df = comorbidData, parallel = FALSE, assoc_table = matrix(NA, nrow = 2, ncol = 1)))
})

test_that("Only one association", {
  expect_warning(multiassoc(df = comorbidData, parallel = FALSE, assoc_table = data.frame(
    PGS = "ldl_PGS",
    Phenotype = "t2d"
  )))
})

prs <- grep("PGS", names(comorbidData), value = T)
phenotype <- c("ethnicity","t2d","log_ldl","sbp_cat")
assoc <- cbind(prs, phenotype)
assoc <- na.omit(assoc)

test_that("Null log", {
  expect_error(multiassoc(df = comorbidData, parallel = FALSE, assoc_table = assoc, log = NULL))
})

test_that("Wrong log", {
  expect_error(multiassoc(df = comorbidData, parallel = FALSE, assoc_table = assoc, log = 1))
})

test_that("Null parallel", {
  expect_error(multiassoc(df = comorbidData, assoc_table = assoc, parallel = NULL))
})

test_that("Wrong parallel", {
  expect_error(multiassoc(df = comorbidData, assoc_table = assoc, parallel = 2))
})

test_that("Test of several PGS and Phenotype using an assoc_table matrix and covariates", {
  skip_on_cran()
  expect_s3_class(
    object = multiassoc(df = comorbidData, assoc_table = assoc, covar_col = c("age", "sex", "gen_array"), parallel = FALSE),
    class = "data.frame"
  )
})

cores <- min(nrow(assoc), getOption("mc.cores", 2L))

test_that("Test of several PGS and Phenotype using an assoc_table matrix and covariates, parallel version and number of cores", {
  skip_on_cran()
  expect_warning(
    object = multiassoc(df = comorbidData, assoc_table = assoc, covar_col = c("age", "sex", "gen_array"), parallel = TRUE, num_cores = cores)
  )
})
