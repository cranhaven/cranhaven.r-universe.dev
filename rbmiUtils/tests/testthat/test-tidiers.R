test_that("positive test tidy_pool_obj", {
  data("ADMI")
  set.seed(122)
  N_IMPUTATIONS <- 100
  BURN_IN <- 200
  BURN_BETWEEN <- 5

  # Convert key columns to factors
  ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
  ADMI$USUBJID <- factor(ADMI$USUBJID)
  ADMI$AVISIT <- factor(ADMI$AVISIT)

  # Define key variables for ANCOVA analysis
  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION") # Covariates for adjustment
  )

  # Specify the imputation method (Bayesian) - need for pool step
  method <- rbmi::method_bayes(
    n_samples = N_IMPUTATIONS,
    control = rbmi::control_bayes(
      warmup = BURN_IN,
      thin = BURN_BETWEEN
    )
  )

  # Perform ANCOVA Analysis on Each Imputed Dataset
  ana_obj_ancova <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = rbmi::ancova, # Apply ANCOVA
    delta = NULL # No sensitivity analysis adjustment
  )

  pool_obj_ancova <- rbmi::pool(ana_obj_ancova)

  # Just check the class of pool_obj_ancova before passing to tidy_pool_obj
  expect_s3_class(pool_obj_ancova, "pool")

  tidy_df <- tidy_pool_obj(pool_obj_ancova)

  # Test output
  # class check
  expect_s3_class(tidy_df, c("tbl_df", "tbl", "data.frame"))

  # column names check
  expect_named(
    tidy_df,
    c(
      "parameter",
      "description",
      "visit",
      "parameter_type",
      "lsm_type",
      "est",
      "se",
      "lci",
      "uci",
      "pval"
    )
  )

  # column types check
  expect_identical(
    lapply(tidy_df, class),
    list(
      parameter = "character",
      description = "character",
      visit = "character",
      parameter_type = "character",
      lsm_type = "character",
      est = "numeric",
      se = "numeric",
      lci = "numeric",
      uci = "numeric",
      pval = "numeric"
    )
  )

  # Columns created by tidy_pool_obj are:
  # - parameter_type
  # - lsm_type
  # - visit
  # - description
  expect_identical(
    tidy_df[, c('description', 'visit', 'parameter_type', 'lsm_type')],
    dplyr::as_tibble(
      data.frame(
        description = c(
          "Treatment Comparison",
          "Least Squares Mean for Reference at Week 24",
          "Least Squares Mean for Alternative at Week 24",
          "Treatment Comparison",
          "Least Squares Mean for Reference at Week 48",
          "Least Squares Mean for Alternative at Week 48"
        ),
        visit = c(
          "Week 24",
          "Week 24",
          "Week 24",
          "Week 48",
          "Week 48",
          "Week 48"
        ),
        parameter_type = c("trt", "lsm", "lsm", "trt", "lsm", "lsm"),
        lsm_type = c(NA, "ref", "alt", NA, "ref", "alt")
      )
    )
  )
})

test_that("tidy_pool_obj requires a pool object", {
  expect_true(class(mtcars) != "pool")

  expect_error(
    tidy_pool_obj(mtcars),
    "Input object must be of class 'pool'"
  )
})
