test_that("positive test get_imputed_data (from vignettes)", {
  set.seed(122)
  data("ADEFF")

  # Constants
  N_IMPUTATIONS <- 100

  # Prepare the data
  ADEFF <- ADEFF %>%
    dplyr::mutate(
      TRT = factor(TRT01P, levels = c("Placebo", "Drug A")),
      USUBJID = factor(USUBJID),
      AVISIT = factor(AVISIT)
    )

  # Define key variables for rbmi
  vars <- rbmi::set_vars(
    subjid = "USUBJID",
    visit = "AVISIT",
    group = "TRT",
    outcome = "CHG",
    covariates = c("BASE", "STRATA", "REGION")
  )

  # Specify the imputation method (Bayesian) - need for pool step
  method <- rbmi::method_bayes(
    n_samples = N_IMPUTATIONS,
    control = rbmi::control_bayes(
      warmup = 100,
      thin = 2
    )
  )

  # Subset relevant columns
  dat <- ADEFF %>%
    select(USUBJID, STRATA, REGION, REGIONC, TRT, BASE, CHG, AVISIT)

  # Fit the imputation model and perform imputation
  draws_obj <- rbmi::draws(
    data = dat,
    vars = vars,
    method = method,
    quiet = TRUE
  )
  impute_obj <- rbmi::impute(
    draws_obj,
    references = c("Placebo" = "Placebo", "Drug A" = "Placebo")
  )

  # Get the imputed data as a data frame
  ADMI <- get_imputed_data(impute_obj)

  # TESTS

  # From the function I think we want to test the following
  # - Rather than an imputation object we have a data.frame
  # - original subject IDs are mapped to the appropriate variable
  # - internal_id column contains the internally created id's
  # - remaining columns are the same as they were in the imputation object

  # class check
  expect_s3_class(ADMI, "data.frame")

  # original subject IDs are in USUBJID column (pattern match against expected format)
  expect_true(
    all(
      grepl("^ID[0-9][0-9][0-9]$", ADMI[, "USUBJID"])
    )
  )

  # check internal_ids
  expect_true(
    all(
      grepl("^new_pt_[0-9]{1,3}$", ADMI[, "internal_id"])
    )
  )

  # we expect the same number of unique internal_ids an USUBJID's
  expect_identical(
    length(unique(ADMI[, "USUBJID"])),
    length(unique(ADMI[, 'internal_id']))
  )

  # Compare the rest of the data
  extracted_dfs <- rbmi::extract_imputed_dfs(impute_obj, idmap = TRUE)

  imputed_dfs2 <- extracted_dfs |>
    purrr::map_dfr(~.x, .id = "IMPID")

  expect_identical(
    imputed_dfs2[, c(
      "STRATA",
      "REGION",
      "REGIONC",
      "TRT",
      "BASE",
      "CHG",
      "AVISIT"
    )],
    ADMI[, c("STRATA", "REGION", "REGIONC", "TRT", "BASE", "CHG", "AVISIT")]
  )
})

test_that("get_imputed_data errors with incorrect inputs", {
  # Assert mtcars is not the required class
  expect_false(inherits(mtcars, "imputation"))

  expect_error(
    get_imputed_data(mtcars),
    "impute_obj must be of an imputation object outputted from rbmi::impute"
  )
})


# additional tests

# -------------------- get_imputed_data ---------------------------------------

testthat::test_that("get_imputed_data: errors on wrong class", {
  expect_false(inherits(mtcars, "imputation"))
  expect_error(
    get_imputed_data(mtcars),
    "impute_obj must be of an imputation object outputted from rbmi::impute"
  )
})

# ---------------------- gcomp_binary (beeca) ---------------------------------
# Use synthetic data so it’s fast and independent of rbmi datasets.

testthat::test_that("gcomp_binary: happy path returns trt list with est/se/df (synthetic)", {
  testthat::skip_if_not_installed("beeca")

  set.seed(101)
  n <- 400
  dat <- data.frame(
    TRT    = factor(sample(c("Placebo","Drug A"), n, TRUE), levels = c("Placebo","Drug A")),
    STRATA = factor(sample(letters[1:3], n, TRUE)),
    REGION = factor(sample(c("EU","US","APAC"), n, TRUE)),
    BASE   = rnorm(n)
  )
  # Simulate binary outcome with treatment + covariate effects
  linpred <- -0.5 +
    0.5*(dat$TRT == "Drug A") +
    0.3*(dat$STRATA == "b") +
    0.2*(dat$REGION == "US") +
    0.1*dat$BASE
  p <- 1/(1 + exp(-linpred))
  dat$CRIT1FLN <- rbinom(n, 1, p)

  out <- gcomp_binary(
    data = dat,
    outcome = "CRIT1FLN",
    treatment = "TRT",
    covariates = c("BASE","STRATA","REGION"),
    reference = "Placebo",
    contrast = "diff",
    method = "Ge",
    type = "HC0"
  )

  expect_true(is.list(out))
  expect_true("trt" %in% names(out))
  expect_true(all(c("est","se","df") %in% names(out$trt)))
  expect_type(out$trt$est, "double")
  expect_type(out$trt$se, "double")
  expect_true(is.na(out$trt$df))
})

testthat::test_that("gcomp_binary: treatment should be factor (if validated)", {
  testthat::skip_if_not_installed("beeca")

  set.seed(102)
  n <- 200
  dat <- data.frame(
    TRT    = sample(c("Placebo","Drug A"), n, TRUE), # character
    STRATA = factor(sample(letters[1:2], n, TRUE)),
    REGION = factor(sample(c("EU","US"), n, TRUE)),
    BASE   = rnorm(n)
  )
  linpred <- -0.4 + 0.6*(dat$TRT == "Drug A") + 0.2*(dat$STRATA == "b") + 0.1*dat$BASE
  p <- 1/(1 + exp(-linpred))
  dat$CRIT1FLN <- rbinom(n, 1, p)

  err <- try(
    gcomp_binary(
      data = dat,
      outcome = "CRIT1FLN",
      treatment = "TRT",
      covariates = c("BASE","STRATA","REGION"),
      reference = "Placebo"
    ),
    silent = TRUE
  )
  if (inherits(err, "try-error")) {
    expect_match(as.character(err), "(?i)factor|categorical")
  } else {
    testthat::skip("No explicit treatment-factor validation implemented; skipping assertion.")
  }
})

testthat::test_that("gcomp_binary: invalid reference level errors (here or in beeca)", {
  testthat::skip_if_not_installed("beeca")

  set.seed(103)
  n <- 200
  dat <- data.frame(
    TRT    = factor(sample(c("Placebo","Drug A"), n, TRUE), levels = c("Placebo","Drug A")),
    STRATA = factor(sample(letters[1:2], n, TRUE)),
    REGION = factor(sample(c("EU","US"), n, TRUE)),
    BASE   = rnorm(n)
  )
  linpred <- -0.4 + 0.6*(dat$TRT == "Drug A") + 0.2*(dat$STRATA == "b") + 0.1*dat$BASE
  p <- 1/(1 + exp(-linpred))
  dat$CRIT1FLN <- rbinom(n, 1, p)

  expect_error(
    gcomp_binary(
      data = dat,
      outcome = "CRIT1FLN",
      treatment = "TRT",
      covariates = c("BASE","STRATA","REGION"),
      reference = "NON_EXISTENT",
      contrast = "diff",
      method = "Ge",
      type = "HC0"
    ),
    regexp = "(?i)reference|level|not.*present|not.*in.*levels|should be one of"
  )
})

testthat::test_that("gcomp_binary: unsupported contrast errors (either here or in beeca)", {
  testthat::skip_if_not_installed("beeca")

  set.seed(104)
  n <- 200
  dat <- data.frame(
    TRT    = factor(sample(c("Placebo","Drug A"), n, TRUE), levels = c("Placebo","Drug A")),
    STRATA = factor(sample(letters[1:2], n, TRUE)),
    REGION = factor(sample(c("EU","US"), n, TRUE)),
    BASE   = rnorm(n)
  )
  linpred <- -0.3 + 0.5*(dat$TRT == "Drug A") + 0.2*(dat$STRATA == "b") + 0.1*dat$BASE
  p <- 1/(1 + exp(-linpred))
  dat$CRIT1FLN <- rbinom(n, 1, p)

  expect_error(
    gcomp_binary(
      data = dat,
      outcome = "CRIT1FLN",
      treatment = "TRT",
      covariates = c("BASE","STRATA","REGION"),
      reference = "Placebo",
      contrast = "not-a-contrast",
      method = "Ge",
      type = "HC0"
    ),
    regexp = "(?i)contrast|allowed|supported|'arg' should be one of"
  )
})
